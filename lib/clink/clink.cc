#include <clink/clink.hh>
#include <clink/link_elf.hh>

#include <object/elf.h>
#include <object/elf.hh>
#include <object/generic.hh>

#include <lcc/file.hh>
#include <lcc/typedefs.hh>

#include <fmt/format.h>

#include <filesystem>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace clink {

bool link(
    std::vector<std::string> objects_strings,
    std::string_view executable_string
) {
    std::vector<std::filesystem::path> objects{};
    for (const auto& s : objects_strings)
        objects.emplace_back(s);
    std::filesystem::path executable{executable_string};
    return link(objects, executable);
};

namespace {

bool perform_relocation(lcc::GenericObject& out, const lcc::Relocation& relocation) {
    auto found = std::ranges::find_if(out.symbols, [&](const auto& sym) {
        return sym.name == relocation.symbol.name;
    });
    if (found == out.symbols.end()) {
        // TODO: undefined reference to `foo`?
        fmt::print(
            stderr,
            "clink: relocation references symbol that does not exist in object file `{}`\n",
            relocation.symbol.name
        );
        return false;
    }

    // Wait for global symbol resolution pass...
    // Without the merged global object file, we can't perform relocations
    // across object files.
    if (found->kind == lcc::Symbol::Kind::EXTERNAL) return false;

    // If symbol definition is in a different section, wait for global symbol
    // resolution pass...
    // Without the global memory layout of every section already in place, we
    // can't perform relocations across sections.
    if (found->section_name != relocation.symbol.section_name) return false;

    auto& relevant_section = out.section(relocation.symbol.section_name);
    { // Confidence
        if (relevant_section.is_fill) {
            fmt::print(
                stderr,
                "clink: relocation references symbol `{}` that resides in is_fill section (cannot relocate)\n",
                relocation.symbol.name
            );
            return false;
        }
        // Ensure contents is big enough, or something
        // TODO: uint32_t will change based on machine/relocation kind pair.
        if (
            relevant_section.contents().size() < relocation.symbol.byte_offset + sizeof(uint32_t)
            or relevant_section.contents().size() < relocation.symbol.byte_offset
        ) {
            fmt::print(
                stderr,
                "clink: relocation offset {} for `{}` out-of-bounds of section `{}` contents (size:{})\n",
                relocation.symbol.byte_offset,
                relocation.symbol.name,
                relevant_section.name,
                relevant_section.contents().size()
            );
            return false;
        }
    }
    auto* relocation_position = relevant_section.contents().data() + relocation.symbol.byte_offset;
    // TODO: This is 32 bits for x86_64, but probably other sizes for other
    // machines and relocation types and stuff...
    uint32_t calculated_value = (uint32_t) ((found->byte_offset + (lcc::usz) relocation.addend) - relocation.symbol.byte_offset);
    // TODO: big vs little endian?
    memcpy(relocation_position, &calculated_value, sizeof(calculated_value));
    // fmt::print(
    //     "Performed relocation in `{}` at offset {}, writing {:x}\n",
    //     relevant_section.name,
    //     relocation.symbol.byte_offset,
    //     calculated_value
    // );
    return true;
}

// Assuming a section of contiguous NULL terminated strings, collect views to each string.
auto collect_strings(const lcc::Section& section) -> std::vector<std::string_view> {
    std::vector<std::string_view> out{};
    const auto* current = reinterpret_cast<const char*>(section.contents().data());
    const auto* const end = current + section.contents().size();
    while (current < end and *current != '\0') {
        auto view = std::string_view{current};
        out.emplace_back(view);
        // Advance pointer by +1 for NULL terminator
        current += view.length() + 1;
    }
    return out;
}

} // namespace

bool link(
    std::vector<std::filesystem::path> objects,
    std::filesystem::path executable
) {
    // Entry point breakdown:
    // crt1.o -- contains _start() which calls main()
    // crti.o, crtn.o -- contains .init and .fini sections

    // TODO:
    // 1. Build some sort of in-memory structure that saves (necessary)
    //    metadata about encountered *defined* symbols in each input object file.
    //  1a. This will likely involve first determining the binary format of the
    //      object file, and then parsing (portions of) it for the necessary info.
    // 2. Resolve required symbol references, perform relocations, etc. using
    //    the in-memory metadata.
    // 3. Build a new object file from the resolved, "linked", in-memory
    //    structure, and write it to the output path parameter.
    //
    // Tangent: We could also go for a sort of "on demand" approach, where we
    // only attempt to fetch or build this metadata once we have encountered a
    // relocation or symbol resolution we need to perform, or something like
    // that.

    std::vector<lcc::GenericObject> parsed_objects{};

    for (auto& p : objects) {
        if (not std::filesystem::exists(p)) {
            fmt::print(
                stderr,
                "clink: given object path does not exist `{}`\n",
                p.string()
            );
            return false;
        }

        auto object_contents = lcc::File::Read(p);

        // Determine binary format of object file
        if (object_contents.size() > sizeof(elf64_header)) {
            elf64_header elf_header{};
            memcpy(&elf_header, object_contents.data(), sizeof(elf64_header));
            auto valid_header = lcc::elf::validate_header(elf_header);
            if (not valid_header.first) {
                fmt::print(
                    stderr,
                    "clink: could not determine binary format of given object file `{}` {}\n",
                    p.string(),
                    valid_header.second
                );
                continue;
            }
        }

        // TODO: Other binary formats (COFF, MachO)

        // Scan and Collect
        // Scan input object files, collect metadata about symbol definitions and
        // necessary relocations.
        auto collected_object = clink::collect_elf(object_contents);
        // fmt::print("{}\n", collected_object.print());
        parsed_objects.emplace_back(std::move(collected_object));
    }

    // Merge collected objects into global object.
    lcc::GenericObject out{};
    // TODO: Not every time we are linking is an executable... Sometimes we
    // just want to link libraries together.
    out.kind = lcc::GenericObject::Kind::EXECUTABLE;

    // Handle Easy/Local Relocations (not across object files)
    // This includes function calls within a single translation unit, for example.
    for (auto& object : parsed_objects) {
        std::erase_if(
            object.relocations,
            [&](const auto& relocation) {
                return perform_relocation(out, relocation);
            }
        );
        // fmt::print(
        //     "================================================================\n"
        //     "Local Relocations Resolved ({} global relocations remain)\n"
        //     "================================================================\n"
        //     "{}\n",
        //     object.relocations.size(),
        //     object.print()
        // );
    }

    // Merge alike sections, build global symbol table
    // - Concatenate .text, .data, etc (shared names)
    // - Increase size of .bss
    // - Merge Symbol Tables, ensuring no duplicate definitions
    // - Collect global relocations
    // - Minor: don't duplicate exact .comment sections
    for (auto& object : parsed_objects) {
        for (auto& section : object.sections) {
            auto found_section = out.find_section(section.name);
            // New sections are easy
            if (not found_section) {
                out.sections.emplace_back(section);
                continue;
            }

            // TODO: MERGE section attribute? Maybe NOMERGE

            // Existing section means merging is required.
            auto& existing_section = found_section->get();
            if (
                (existing_section.is_fill and not section.is_fill)
                or (not existing_section.is_fill and section.is_fill)
            ) {
                fmt::print(
                    stderr,
                    "clink: cannot merge sections (fill/non-fill): `{}`\n",
                    existing_section.name
                );
                return false;
            }
            lcc::usz existing_section_size{0};
            if (existing_section.is_fill) {
                if (existing_section.value() != section.value()) {
                    fmt::print(
                        stderr,
                        "clink: cannot merge fill sections (value mismatch): `{}` ({}) and `{}` ({})\n",
                        existing_section.name,
                        existing_section.value(),
                        section.name,
                        section.value()
                    );
                    return false;
                }
                existing_section_size = existing_section.length();
                existing_section.length() += section.length();
            } else {
                if (section.name == ".comment") {
                    std::vector<std::string_view> existing_strings = collect_strings(existing_section);
                    std::vector<std::string_view> encountered_strings = collect_strings(section);
                    // Modify existing strings to include encountered strings, except for
                    // strings we already have.
                    for (auto s : encountered_strings) {
                        if (not std::ranges::contains(existing_strings, s))
                            existing_strings.emplace_back(s);
                    }
                    existing_section.contents().clear();
                    for (auto s : existing_strings) {
                        existing_section.contents().insert(
                            existing_section.contents().end(),
                            s.begin(),
                            s.end()
                        );
                        existing_section += '\0';
                    }
                    existing_section_size = existing_section.contents().size();
                } else {
                    existing_section_size = existing_section.contents().size();
                    existing_section.contents().insert(
                        existing_section.contents().end(),
                        section.contents().begin(),
                        section.contents().end()
                    );
                    // fmt::print("New `{}` size: {}\n", existing_section.name, existing_section.contents().size());
                }
            }
            // For any symbol defined in this section, update address/offset to
            // include merged data.
            for (auto& sym : object.symbols) {
                // Skip symbols not defined in this section
                if (sym.section_name != section.name) continue;
                // Skip section symbol (always points to beginning)
                if (sym.name == sym.section_name) continue;
                // fmt::print(
                //     "Adjusting symbol {} in section {} at offset {}... increasing by {}\n",
                //     sym.name,
                //     sym.section_name,
                //     sym.byte_offset,
                //     existing_section_size
                // );
                sym.byte_offset += existing_section_size;
            }
            for (auto& reloc : object.relocations) {
                // Skip relocations not in this section
                if (reloc.symbol.section_name != section.name) continue;
                // fmt::print(
                //     "Adjusting relocation of {} (addend {}) in section {} at offset {}... increasing by {}\n",
                //     reloc.symbol.name,
                //     reloc.addend,
                //     reloc.symbol.section_name,
                //     reloc.symbol.byte_offset,
                //     existing_section_size
                // );
                reloc.symbol.byte_offset += existing_section_size;
            }
        }

        for (auto& sym : object.symbols) {
            auto found = std::ranges::find_if(out.symbols, [&](const auto& s) {
                return s.name == sym.name;
            });
            // New symbol, simply add it to global symbol table.
            if (found == out.symbols.end()) {
                if (sym.kind != lcc::Symbol::Kind::STATIC)
                    out.symbols.emplace_back(sym);
                continue;
            }
            // Existing symbol, still no definition
            if (
                found->kind == lcc::Symbol::Kind::EXTERNAL
                and sym.kind == lcc::Symbol::Kind::EXTERNAL
            ) {
                if (found->section_name != sym.section_name) {
                    fmt::print(
                        stderr,
                        "clink: duplicate definition of `{}`"
                        " (section mismatch between externals, `{}` and `{}`)\n",
                        sym.name,
                        found->section_name,
                        sym.section_name
                    );
                    return false;
                }
                continue;
            }
            // Existing symbol, but first definition
            else if (found->kind == lcc::Symbol::Kind::EXTERNAL) {
                if (
                    not found->section_name.empty()
                    and not sym.section_name.empty()
                    and found->section_name != sym.section_name
                ) {
                    fmt::print(
                        stderr,
                        "clink: duplicate definition of `{}`"
                        " (section mismatch between external and first definition, `{}` and `{}`)\n",
                        sym.name,
                        found->section_name,
                        sym.section_name
                    );
                    return false;
                }
                // Overwrite previously external symbol with actual definition
                *found = sym;
            } else {
                // Definition already found, this must be an external reference
                if (sym.kind != lcc::Symbol::Kind::EXTERNAL) {
                    fmt::print(stderr, "clink: duplicate definition of `{}`\n", sym.name);
                    // TODO: Would be nice to be able to print what files the symbols came from.
                    return false;
                }
            }
        }

        // Collect Global Relocations
        out.relocations.insert(
            out.relocations.end(),
            object.relocations.begin(),
            object.relocations.end()
        );
        object.relocations.clear();
    }

    // Resolve and Patch
    // Iterate collected relocations, referencing the global symbol table to
    // properly calculate addresses and/or offsets.
    std::erase_if(
        out.relocations,
        [&](const auto& relocation) {
            return perform_relocation(out, relocation);
        }
    );
    // fmt::print(
    //     "================================================================\n"
    //     "Global Relocations Resolved ({} relocations remain)\n"
    //     "================================================================\n"
    //     "{}\n",
    //     out.relocations.size(),
    //     out.print()
    // );

    // TODO: Should we emit an error for each relocation still left, or has
    // that already been handled, since we've tried to the perform relocation
    // and failed?

    FILE* outfile = fopen(executable.c_str(), "wb");
    // TODO: respect context output format
    out.as_elf(outfile);
    return 0 == fclose(outfile);
}

} // namespace clink
