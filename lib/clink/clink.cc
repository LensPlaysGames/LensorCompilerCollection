#include <clink/clink.hh>

#include <clink/layout.hh>
#include <clink/link_elf.hh>

#include <object/elf.h>
#include <object/elf.hh>
#include <object/generic.hh>

#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/file.hh>
#include <lcc/format.hh>
#include <lcc/target.hh>
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
    lcc::Context& context,
    std::vector<std::string> objects_strings,
    std::string_view executable_string
) {
    std::vector<std::filesystem::path> objects{};
    for (const auto& s : objects_strings)
        objects.emplace_back(s);
    std::filesystem::path executable{executable_string};
    return link(context, objects, executable);
};

namespace {

void perform_relocation__32(
    lcc::Section& relevant_section,
    const lcc::Relocation& relocation,
    const lcc::Symbol& resolved_symbol
) {
    auto* relocation_position = relevant_section.contents().data() + relocation.symbol.byte_offset;
    uint32_t calculated_value = (uint32_t) ((resolved_symbol.byte_offset
                                             + (lcc::usz) relocation.addend)
                                            - relocation.symbol.byte_offset);
    // TODO: big vs little endian?
    memcpy(relocation_position, &calculated_value, sizeof(calculated_value));
}

bool perform_relocation(
    lcc::Context& context,
    lcc::GenericObject& out,
    const lcc::Relocation& relocation,
    std::vector<std::string>& global_offset_table
) {
    auto found_sym = out.find_symbol(relocation.symbol.name);
    if (not found_sym) {
        // NOTE: Not an undefined reference because we may have just not found
        // it *as of yet*.
        return false;
    }
    auto& found = found_sym->get();

    // Wait for global symbol resolution pass...
    // Without the merged global object file, we can't perform relocations
    // across object files.
    if (found.kind == lcc::Symbol::Kind::EXTERNAL) return false;

    // If symbol definition is in a different section, wait for global symbol
    // resolution pass...
    // Without the global memory layout of every section already in place, we
    // can't perform relocations across sections.
    // If we have been passed a memory layout, utilise it to resolve the
    // address of this symbol (since we know the byte offset within the
    // section of the symbol definition, and the layout will tell us the
    // address of the section it is within).
    if (found.section_name != relocation.symbol.section_name) {
        return false;
    }

    // This is the section we are going to perform the relocation within.
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

    switch (relocation.kind) {
        case lcc::Relocation::Kind::NONE:
            return true;
        case lcc::Relocation::Kind::DISPLACEMENT32:
        case lcc::Relocation::Kind::DISPLACEMENT32_PCREL: {
            perform_relocation__32(relevant_section, relocation, found);
            // fmt::print(
            //     "Performed relocation in `{}` at offset {}, writing {:x}\n",
            //     relevant_section.name,
            //     relocation.symbol.byte_offset,
            //     calculated_value
            // );
        } break;

        case lcc::Relocation::Kind::DISPLACEMENT32_GOTPCREL: {
            // Always try to relax first
            // Relaxing involves the linker rewriting assembled machine code to
            // perform optimisations. For example, in a static linking situation,
            // sometimes the compiler is linking in libraries that were compiled as
            // position independent code. That code couldn't make assumptions about
            // how external symbols were going to be resolved (i.e. dynamic vs
            // static), so they went with the safe route and emitted instructions that
            // load a pointer from a section in memory, and then use that pointer as
            // the symbol address. The problem lies in that it is needlessly
            // inefficient to store a fixed pointer into the binary at link time when
            // we could just rewrite the `mov` into a `lea` and be done with it.
            // NOTE: Trickiness due to possible REX prefix
            // - `48 8b 3d 00 00 00 00` -> `48 8d 3d 00 00 00 00`
            //   Change 8b to 8d (Transforms mov to lea).
            // - Replace `ff 15 00 00 00 00` with `e8 00 00 00 00 90`
            //   Change ff 15 to e8 (Transforms indirect far call to direct relative call)
            if (
                relocation.symbol.byte_offset >= 2
            ) {
                // TODO: Only for x86_64
                if (relevant_section.contents().at(relocation.symbol.byte_offset - 2) == 0x8b) {
                    relevant_section.contents().at(relocation.symbol.byte_offset - 2) = 0x8d;
                    perform_relocation__32(relevant_section, relocation, found);
                    // fmt::print("Relaxed load from GOT of address of {} to lea\n", found->name);
                    return true;
                }
                if (
                    relevant_section.contents().at(relocation.symbol.byte_offset - 2) == 0xff
                    and relevant_section.contents().at(relocation.symbol.byte_offset - 1) == 0x15 // indirect call
                ) {
                    // ff 15 -> e8  moves address over by one
                    relevant_section.contents().at(relocation.symbol.byte_offset - 2) = 0xe8; // relative call
                    relevant_section.contents().at(relocation.symbol.byte_offset + 3) = 0x90; // NOP
                    auto adjusted_relocation = relocation;
                    --adjusted_relocation.symbol.byte_offset;
                    perform_relocation__32(relevant_section, adjusted_relocation, found);
                    // If there was a REX prefix, we have to get rid of it. There should never
                    // be one, no modern compiler would ever emit this, but, it's technically
                    // possible.
                    if (
                        relocation.symbol.byte_offset >= 3
                        and relevant_section.contents().at(relocation.symbol.byte_offset - 3) >= 0x40
                        and relevant_section.contents().at(relocation.symbol.byte_offset - 3) <= 0x4f
                    ) relevant_section.contents().at(relocation.symbol.byte_offset - 3) = 0x90;
                    // fmt::print("Relaxed indirect call to {} to direct call + NOP\n", found->name);
                    return true;
                }
                if (
                    relevant_section.contents().at(relocation.symbol.byte_offset - 2) == 0xff
                    and relevant_section.contents().at(relocation.symbol.byte_offset - 1) == 0x25 // indirect jmp
                ) {
                    // ff 25 -> e9  moves address over by one
                    relevant_section.contents().at(relocation.symbol.byte_offset - 2) = 0xe9; // direct jmp
                    relevant_section.contents().at(relocation.symbol.byte_offset + 3) = 0x90; // NOP
                    auto adjusted_relocation = relocation;
                    --adjusted_relocation.symbol.byte_offset;
                    perform_relocation__32(relevant_section, adjusted_relocation, found);
                    // If there was a REX prefix, we have to get rid of it.
                    if (
                        relocation.symbol.byte_offset >= 3
                        and relevant_section.contents().at(relocation.symbol.byte_offset - 3) >= 0x40
                        and relevant_section.contents().at(relocation.symbol.byte_offset - 3) <= 0x4f
                    ) relevant_section.contents().at(relocation.symbol.byte_offset - 3) = 0x90;
                    // fmt::print("Relaxed indirect jmp to {} to direct jmp + NOP\n", found->name);
                    return true;
                }
            }

            // Ensure entry exists in GOT
            if (not std::ranges::contains(global_offset_table, relocation.symbol.name))
                global_offset_table.push_back(relocation.symbol.name);

            // Relocation not yet performed...
            return false;
        } break;
    }

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
    lcc::Context& context,
    std::vector<std::filesystem::path> objects,
    std::filesystem::path executable
) {
    // Entry point breakdown:
    // crt1.o -- contains _start() which calls main()
    // crti.o, crtn.o -- contains .init and .fini sections

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
            if (valid_header.first) {
                // Scan and Collect
                // Scan input object files, collect metadata about symbol definitions and
                // necessary relocations.
                auto collected_object = clink::collect_elf(object_contents);
                // fmt::print("{}\n", collected_object.print());
                parsed_objects.emplace_back(std::move(collected_object));
                continue;
            }
        }

        constexpr std::array<char, 8> archive_magic_bytes{'!', '<', 'a', 'r', 'c', 'h', '>', '\n'};
        if (object_contents.size() > archive_magic_bytes.size()) {
            std::array<char, 8> first_bytes{};
            memcpy(first_bytes.data(), object_contents.data(), archive_magic_bytes.size());
            if (first_bytes == archive_magic_bytes) {
                fmt::print("Got archive at {}\n", p.string());
                fmt::print(stderr, "clink: TODO read symbol table from archive\n");
                fmt::print(stderr, "clink: TODO extract objects for necessary symbols\n");
                continue;
            }
        }

        // TODO: Other binary formats (COFF, MachO)
        fmt::print(
            stderr,
            "clink: could not determine binary format of given object file `{}`\n",
            p.string()
        );
    }

    // Merge collected objects into global object.
    lcc::GenericObject out{};
    // Collect unique symbols that have `GOT` in their relocation, i.e.
    // require a slot in the global offset table for their address to be
    // written into.
    std::vector<std::string> global_offset_table{};
    // TODO: Not every time we are linking is an executable... Sometimes we
    // just want to link libraries together.
    out.kind = lcc::GenericObject::Kind::EXECUTABLE;

    // Handle Easy/Local Relocations (not across object files)
    // This includes function calls within a single translation unit, for example.
    for (auto& object : parsed_objects) {
        std::erase_if(
            object.relocations,
            [&](const auto& relocation) {
                return perform_relocation(context, out, relocation, global_offset_table);
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

    // Merge alike sections, build global symbol table (out.symbols)
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
                // Skip section symbol (always points to beginning, no relocation
                // necessary for merged section)
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

    // Create Global Offset Table section, if necessary (but we can't yet fill
    // it's data out properly).
    if (global_offset_table.size()) {
        lcc::Section global_offset_table_section{".got"};
        global_offset_table_section.attribute(lcc::Section::Attribute::LOAD, true);
        global_offset_table_section.attribute(lcc::Section::Attribute::WRITABLE, true);

        global_offset_table_section.contents().resize(
            global_offset_table.size()
            * (context.target()->size_of_pointer / context.target()->ffi.size_of_char)
        );

        // It is the job of the backend to actually populate this with hard
        // addresses of symbols.

        out.sections.emplace_back(global_offset_table_section);
    }

    // LAYOUT ENGINE
    // First, sort *loaded* sections by alike permissions, such that
    // executable code comes first, then readable, then writable.
    std::ranges::sort(out.sections, [](const lcc::Section& a, const lcc::Section& b) -> bool {
        using Attribute = lcc::Section::Attribute;
        // Return true if ordered before

        // If A is loaded, and B is not, place A before B.
        if (
            a.attribute(Attribute::LOAD)
            and not b.attribute(Attribute::LOAD)
        ) return true;

        // If A is executable, and B is not, place A before B.
        if (
            a.attribute(Attribute::EXECUTABLE)
            and not b.attribute(Attribute::EXECUTABLE)
        ) return true;

        // If A is not writable, and B is, place A before B.
        if (
            (not a.attribute(Attribute::WRITABLE))
            and b.attribute(Attribute::WRITABLE)
        ) return true;

        // If A is not fill, and B is, place A before B.
        if ((not a.is_fill) and b.is_fill)
            return true;

        return false;
    });

    Layout memory_layout = layout(out);

    // Resolve and Patch
    // Iterate collected relocations, referencing the global symbol table to
    // properly calculate addresses and/or offsets.
    std::erase_if(
        out.relocations,
        [&](const auto& relocation) {
            return perform_relocation(context, out, relocation, global_offset_table);
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

    for (auto& r : out.relocations) {
        fmt::print(
            stderr,
            "\nclink: Undefined reference to `{}`\n",
            r.symbol.name
        );
    }

    FILE* outfile = fopen(executable.c_str(), "wb");
    switch (context.format()->format()) {
        case lcc::Format::INVALID:
        case lcc::Format::LCC_IR:
        case lcc::Format::LCC_SSA_IR:
        case lcc::Format::LLVM_TEXTUAL_IR:
        case lcc::Format::WASM_TEXTUAL:
        case lcc::Format::GNU_AS_ATT_ASSEMBLY:
            lcc::Diag::ICE("clink: output format is not supported");

        case lcc::Format::ELF_OBJECT:
            out.as_elf(outfile, memory_layout);
            break;
        case lcc::Format::COFF_OBJECT:
            out.as_coff(outfile, memory_layout);
            break;
    }
    out.as_elf(outfile, memory_layout);
    return 0 == fclose(outfile) and out.relocations.empty();
}

} // namespace clink
