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
#include <vector>

namespace clink {

bool link(
    std::vector<std::filesystem::path> objects,
    std::filesystem::path executable
) {
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

        if (object_contents.size() > sizeof(elf64_header)) {
            elf64_header elf_header{};
            memcpy(&elf_header, object_contents.data(), sizeof(elf64_header));
            auto valid_header = lcc::elf::validate_header(elf_header);
            if (not valid_header.first) {
                fmt::print(
                    stderr,
                    "clink: could not determine binary format of given object file `{}`",
                    p.string()
                );
                continue;
            }
        }

        // TODO: Determine binary format of object file
        // TODO: Scan and Collect
        // Scan input object files, collect metadata about symbol definitions and
        // necessary relocations.
        // TODO ELF:
        // Iterate .rel[a].text for relocation requests at specific code offsets,
        // storing these for later.
        // Iterate .symtab for symbol _definitions_ to add to global symbol table.
        auto collected_object = clink::collect_elf(object_contents);
        fmt::print("{}\n", collected_object.print());
        parsed_objects.emplace_back(std::move(collected_object));
    }

    // Handle Easy/Local Relocations (not across object files)
    // This includes function calls within a single translation unit, for example.
    for (auto& object : parsed_objects) {
        std::erase_if(object.relocations, [&](const auto& relocation) {
            auto found = std::ranges::find_if(object.symbols, [&](const auto& sym) {
                return sym.name == relocation.symbol.name;
            });
            if (found == object.symbols.end()) {
                // TODO: undefined reference to `foo`?
                fmt::print(
                    stderr,
                    "clink: relocation references symbol that does not exist in object file `{}`",
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

            auto& relevant_section = object.section(relocation.symbol.section_name);
            // TODO: Ensure contents is big enough, or something
            auto* relocation_position = relevant_section.contents().data() + relocation.symbol.byte_offset;
            *reinterpret_cast<uint32_t*>(relocation_position)
                = (uint32_t) ((found->byte_offset + (lcc::usz) relocation.addend) - relocation.symbol.byte_offset);
            fmt::print("Performed local relocation:\n{}\n", relocation.print());
            return true;
        });
        fmt::print(
            "================================================================\n"
            "Local Relocations Resolved\n"
            "================================================================\n"
            "{}\n",
            object.print()
        );
    }

    // TODO: Merge collected objects into global object.
    // Merge alike sections
    // - Concatenate .text, .data, etc (shared names)
    // - Increase size of .bss
    // - Merge Symbol Tables, ensuring no duplicate definitions
    lcc::GenericObject out{};
    for (auto& object : parsed_objects) {
        for (auto& section : object.sections) {
            auto found_section = out.find_section(section.name);
            // New sections are easy
            if (not found_section) {
                out.sections.emplace_back(section);
                continue;
            }
            // Existing section means merging is required.
            auto& existing_section = found_section->get();
            if (
                (existing_section.is_fill and not section.is_fill)
                or (not existing_section.is_fill and section.is_fill)
            ) {
                fmt::print(
                    stderr,
                    "clink: cannot merge sections (fill/non-fill): `{}` and `{}`\n",
                    existing_section.name,
                    section.name
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
                existing_section_size = existing_section.contents().size();
                existing_section.contents().insert(
                    existing_section.contents().end(),
                    section.contents().begin(),
                    section.contents().end()
                );
            }
            // For any symbol defined in this section, update address/offset to
            // include merged data.
            for (auto& sym : object.symbols) {
                // Skip symbols not defined in this section
                if (sym.section_name != section.name) continue;
                sym.byte_offset += existing_section_size;
            }
        }

        for (auto& sym : object.symbols) {
            auto found = std::ranges::find_if(out.symbols, [&](const auto& s) {
                return s.name == sym.name;
            });
            // New symbol, simply add it to global symbol table.
            if (found == out.symbols.end()) {
                out.symbols.emplace_back(sym);
                continue;
            }
            // Existing symbol, still no definition
            if (found->kind == lcc::Symbol::Kind::EXTERNAL and sym.kind == lcc::Symbol::Kind::EXTERNAL) {
                if (found->section_name != sym.section_name) {
                    fmt::print(stderr, "clink: duplicate definition of `{}`\n", sym.name);
                    return false;
                }
                continue;
            }
            // Existing symbol, but first definition
            else if (found->kind == lcc::Symbol::Kind::EXTERNAL) {
                if (found->section_name != sym.section_name) {
                    fmt::print(stderr, "clink: duplicate definition of `{}`\n", sym.name);
                    return false;
                }
                // Overwrite previously external symbol with actual definition
                *found = sym;
            } else {
                if (
                    found->section_name != sym.section_name
                    or found->byte_offset != sym.byte_offset
                    or found->kind != sym.kind
                ) {
                    fmt::print(stderr, "clink: duplicate definition of `{}`\n", sym.name);
                    return false;
                }
            }
        }
    }

    // TODO: Resolve and Patch
    // Iterate collected relocations, referencing the global symbol table to
    // properly calculate addresses and/or offsets.
    for (auto& relocation : out.relocations) {
        fmt::print("clink: TODO: global relocation\n{}\n", relocation.print());
    }

    FILE* outfile = fopen(executable.c_str(), "wb");
    out.as_elf(outfile);
    fclose(outfile);

    return false;
}

} // namespace clink
