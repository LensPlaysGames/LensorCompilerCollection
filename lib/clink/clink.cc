#include <clink/clink.hh>

#include <clink/layout.hh>
#include <clink/link_elf.hh>
#include <clink/link_uarchive.hh>
#include <clink/relocate.hh>

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

#include <algorithm>
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

void collect_undefined(
    std::vector<std::string_view>& defined_symbols,
    std::vector<std::string_view>& undefined_symbols,
    const lcc::GenericObject& object
) {
    for (const auto& s : object.symbols) {
        // If undefined and not yet defined, add to list of undefined symbols.
        if (s.kind == lcc::Symbol::Kind::EXTERNAL) {
            if (
                // already defined
                not std::ranges::contains(defined_symbols, s.name)
                // no duplicates
                and not std::ranges::contains(undefined_symbols, s.name)
            ) undefined_symbols.emplace_back(s.name);
        } else {
            // If defined and already undefined, remove from list of undefined symbols.
            std::erase(undefined_symbols, s.name);
            // Prevent future external references to this symbol from adding this
            // symbol as undefined (since this is it's definition).
            if (not std::ranges::contains(defined_symbols, s.name))
                defined_symbols.emplace_back(s.name);
        }
    }
}

struct ObjectCollectionContext {
    // output
    std::vector<lcc::GenericObject> parsed_objects{};

    // bookkeeping
    // Contains names of objects that we have already visited, in an attempt
    // to prevent processing duplicate object files.
    std::vector<std::string> visited{};
    // Contains all symbols currently defined.
    std::vector<std::string_view> defined{};
    // Contains all symbols currently declared and not defined.
    std::vector<std::string_view> undefined{};
};

// @param path  if the binary blob came from a file, this points to that
// file. Keep in mind that the binary blob may have been extracted from
// said file, so they are not guaranteed to be equivalent binary-wise.
void collect_object(
    lcc::Context& context,
    std::string_view path,
    std::vector<char>& blob,
    ObjectCollectionContext& object_collection
) {
    // Determine binary format of object file
    if (blob.size() > sizeof(elf64_header)) {
        elf64_header elf_header{};
        memcpy(&elf_header, blob.data(), sizeof(elf64_header));
        auto valid_header = lcc::elf::validate_header(elf_header);
        if (valid_header.first) {
            // Scan and Collect
            // Scan input object files, collect metadata about symbol definitions and
            // necessary relocations.
            // fmt::print("Collecting ELF file at `{}`\n", path);
            auto collected_object = collect_elf(blob);
            // fmt::print("{}\n", collected_object.print());
            collect_undefined(
                object_collection.defined,
                object_collection.undefined,
                collected_object
            );
            object_collection.parsed_objects.emplace_back(
                std::move(collected_object)
            );
            return;
        }
    }

    constexpr std::array<char, 8> archive_magic_bytes{'!', '<', 'a', 'r', 'c', 'h', '>', '\n'};
    if (blob.size() > archive_magic_bytes.size()) {
        std::array<char, 8> first_bytes{};
        memcpy(first_bytes.data(), blob.data(), archive_magic_bytes.size());
        if (first_bytes == archive_magic_bytes) {
            bool found_needed_symbols{true};
            // TODO: Parse uarchive into map or something so we don't have to keep
            // decoding it over and over.
            while (found_needed_symbols) {
                // fmt::print("looking in archive at `{}`, undefined: {}\n", path, object_collection.undefined);
                auto collected_object_blobs = collect_uarchive(
                    blob,
                    object_collection.undefined,
                    object_collection.visited
                );
                found_needed_symbols = collected_object_blobs.size();
                // fmt::print("found_needed_symbols:{}\n", found_needed_symbols);
                for (auto& collected_blob : collected_object_blobs)
                    collect_object(context, path, collected_blob, object_collection);
                // fmt::print("!!collected objects\n");
            }
            return;
        }
    }

    // TODO: Other binary formats (COFF, MachO)
    fmt::print(
        stderr,
        "clink: could not determine binary format of given object file `{}`\n",
        path
    );
}

auto collect_objects(
    lcc::Context& context,
    std::vector<std::filesystem::path> objects
) -> std::vector<lcc::GenericObject> {
    ObjectCollectionContext object_collection{};
    for (auto& p : objects) {
        if (not std::filesystem::exists(p)) {
            fmt::print(
                stderr,
                "clink: given object path does not exist `{}`\n",
                p.string()
            );
            continue;
        }

        auto object_contents = lcc::File::Read(p);
        auto pstr = p.string();
        collect_object(context, pstr, object_contents, object_collection);
    }

    return object_collection.parsed_objects;
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
    // 2. Merge input objects into singular, generic output object.
    // 3. Transform single, generic object into final binary format, updating addresses
    //    and relocation and symbol byte offsets as necessary wrt layout.
    // 4. Resolve required symbol references, perform relocations, etc. directly
    //    within the final binary format, using the single object as reference.

    // Sort archives (.a files) to the back, since only objects that define
    // *currently* undefined symbols are extracted from them.
    std::ranges::stable_partition(
        objects,
        [](const auto& p) -> bool {
            return p.extension() != ".a";
        }
    );
    auto parsed_objects = collect_objects(context, objects);

    // Merge collected objects into global object.
    lcc::GenericObject out{};
    // Collect unique symbols that have `GOT` in their relocation, i.e.
    // require a slot in the global offset table for their address to be
    // written into.
    std::vector<std::string> global_offset_table{};
    // TODO: Not every time we are linking is an executable... Sometimes we
    // just want to link libraries together.
    out.kind = lcc::GenericObject::Kind::EXECUTABLE;

    // Collect GOT entries...
    for (auto& object : parsed_objects) {
        for (auto& relocation : object.relocations) {
            if (relocation.kind == lcc::Relocation::Kind::DISPLACEMENT32_GOTPCREL) {
                global_offset_table.emplace_back(relocation.symbol.name);
            }
        }
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
            using K = lcc::Symbol::Kind;
            auto found = std::ranges::find_if(out.symbols, [&](const auto& s) {
                return s.name == sym.name;
            });

            // Deduplicate section symbols
            if (out.find_section(sym.name)) {
                // Only add once
                if (not out.find_symbol(sym.name))
                    out.symbols.emplace_back(sym);
                continue;
            }

            // New symbol, simply add it to global symbol table.
            if (found == out.symbols.end()) {
                // Exclude file-local symbols from the global symbol table.
                if (sym.kind != K::STATIC)
                    out.symbols.emplace_back(sym);
                continue;
            }

            // Already seen a symbol with this name, merge it or error.

            // Do different things for each pair of existing and encountered symbol
            // kinds.
            switch (found->kind) {
                case lcc::Symbol::Kind::NONE:
                case lcc::Symbol::Kind::EXTERNAL:
                case lcc::Symbol::Kind::WEAK: {
                    if (
                        sym.kind != K::WEAK // two weak symbols are totally fine
                        and sym.section_name.size()
                        and found->section_name.size()
                        and sym.section_name != found->section_name
                    ) {
                        fmt::print(
                            stderr,
                            "clink: duplicate definition of `{}`"
                            " (section mismatch, `{}` and `{}`)\n  old:{}  new:{}",
                            sym.name,
                            found->section_name,
                            sym.section_name,
                            found->print(),
                            sym.print()
                        );
                        return false;
                    }
                    // Found undefined or weak-defined symbol, taking action based on
                    // encountered symbol kind...
                    switch (sym.kind) {
                        // An undefined symbol does not overwrite or redefine an already undefined
                        // or weak-defined symbol, given they are within the same section.
                        case lcc::Symbol::Kind::NONE:
                        case lcc::Symbol::Kind::EXTERNAL:
                            break;

                        // A weak-defined symbol overwrites an undefined symbol.
                        case lcc::Symbol::Kind::WEAK:
                            if (found->kind != K::WEAK)
                                *found = sym;
                            break;

                        // A definition overwrites an undefined symbol.
                        case lcc::Symbol::Kind::FUNCTION:
                        case lcc::Symbol::Kind::STATIC:
                        case lcc::Symbol::Kind::EXPORT:
                            *found = sym;
                            break;
                    }
                } break;

                case lcc::Symbol::Kind::FUNCTION:
                case lcc::Symbol::Kind::STATIC:
                case lcc::Symbol::Kind::EXPORT: {
                    // Found defined symbol, taking action based on encountered symbol kind...
                    switch (sym.kind) {
                        // An undefined or weak-defined symbol does not overwrite or redefine an
                        // already defined symbol.
                        case lcc::Symbol::Kind::NONE:
                        case lcc::Symbol::Kind::EXTERNAL:
                        case lcc::Symbol::Kind::WEAK:
                            break;

                        // A defined symbol redefines an already defined symbol.
                        case lcc::Symbol::Kind::FUNCTION:
                        case lcc::Symbol::Kind::STATIC:
                        case lcc::Symbol::Kind::EXPORT: {
                            fmt::print(stderr, "clink: duplicate definition of `{}`\n", sym.name);
                            fmt::print(stderr, "  existing:    {}", found->print());
                            fmt::print(stderr, "  encountered: {}", sym.print());
                            // TODO: Would be nice to be able to print what files the symbols came from.
                            return false;
                        } break;
                    }
                } break;
            }
        } // for (object.symbols)

        // Collect Global Relocations
        std::erase_if(object.relocations, [&](auto& r) {
            if (r.symbol.kind != lcc::Symbol::Kind::STATIC) {
                out.relocations.emplace_back(std::move(r));
                return true;
            }
            return false;
        });
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
    std::ranges::sort(
        out.sections,
        [](const lcc::Section& a, const lcc::Section& b) -> bool {
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
        }
    );
    Layout memory_layout = layout(out);

    // Linker-defined symbols
    {
        {
            lcc::Symbol s{};
            s.kind = lcc::Symbol::Kind::STATIC;
            s.name = "_GLOBAL_OFFSET_TABLE_";
            if (auto section = out.find_section(".got"))
                s.byte_offset = memory_layout.address(".got");
            if (auto f = out.find_symbol(s.name))
                f->get() = std::move(s);
            else out.symbols.emplace_back(s);
        }
        {
            lcc::Symbol s{};
            s.kind = lcc::Symbol::Kind::STATIC;
            s.name = "_DYNAMIC";
            if (auto section = out.find_section(".dynamic"))
                s.byte_offset = memory_layout.address(".dynamic");
            if (auto f = out.find_symbol(s.name))
                f->get() = std::move(s);
            else out.symbols.emplace_back(s);
        }
        {
            lcc::Symbol start{};
            start.kind = lcc::Symbol::Kind::STATIC;
            start.name = "__init_array_start";

            lcc::Symbol end{};
            end.kind = lcc::Symbol::Kind::STATIC;
            end.name = "__init_array_end";

            if (auto section = out.find_section(".init_array")) {
                start.byte_offset = memory_layout.address(section->get().name);
                end.byte_offset = start.byte_offset + section->get().size();
            }

            if (auto f = out.find_symbol(start.name))
                f->get() = std::move(start);
            else out.symbols.emplace_back(start);
            if (auto f = out.find_symbol(end.name))
                f->get() = std::move(end);
            else out.symbols.emplace_back(end);
        }
        {
            lcc::Symbol start{};
            start.kind = lcc::Symbol::Kind::STATIC;
            start.name = "__fini_array_start";

            lcc::Symbol end{};
            end.kind = lcc::Symbol::Kind::STATIC;
            end.name = "__fini_array_end";

            if (auto section = out.find_section(".fini_array")) {
                start.byte_offset = memory_layout.address(section->get().name);
                end.byte_offset = start.byte_offset + section->get().size();
            }

            if (auto f = out.find_symbol(start.name))
                f->get() = std::move(start);
            else out.symbols.emplace_back(start);
            if (auto f = out.find_symbol(end.name))
                f->get() = std::move(end);
            else out.symbols.emplace_back(end);
        }
    }

    // Emit into final binary format, ensuring it adjusts relocation and
    // symbol byte offsets as necessary (i.e. ELF altering addresses to match
    // page offsets within the file...)
    std::vector<char> binary_blob{};
    switch (context.format()->format()) {
        case lcc::Format::INVALID:
        case lcc::Format::LCC_IR:
        case lcc::Format::LCC_SSA_IR:
        case lcc::Format::LLVM_TEXTUAL_IR:
        case lcc::Format::WASM_TEXTUAL:
        case lcc::Format::GNU_AS_ATT_ASSEMBLY:
            lcc::Diag::ICE("clink: output format is not supported");

        case lcc::Format::ELF_OBJECT:
            binary_blob = out.as_elf(
                memory_layout,
                lcc::GenericObject::EmitRelocations::No
            );
            break;
        case lcc::Format::COFF_OBJECT:
            binary_blob = out.as_coff(
                memory_layout,
                lcc::GenericObject::EmitRelocations::No
            );
            break;
    }

    for (auto [i, got_symbol] : std::ranges::views::enumerate(global_offset_table)) {
        auto found = out.find_symbol(got_symbol);
        if (not found) continue;
        auto symbol = found->get();
        // fmt::print("`{}` address: 0x{:016x}\n", symbol.section_name, memory_layout.address(symbol.section_name));
        uint64_t address
            = memory_layout.address(symbol.section_name)
            + symbol.byte_offset;
        const auto size_of_pointer_bytes
            = context.target()->size_of_pointer
            / context.target()->ffi.size_of_char;
        auto got_slot = binary_blob.data()
                      + memory_layout.section_offset(".got")
                      + (unsigned) i * size_of_pointer_bytes;
        // fmt::print(
        //     "Address of GOT symbol `{}`:0x{:016x}\n",
        //     symbol.name,
        //     address
        // );
        memcpy(
            got_slot,
            &address,
            std::min(
                size_of_pointer_bytes,
                sizeof(address)
            )
        );
    }

    // Update addresses of linker-defined symbols, if needed.
    {
        if (auto f = out.find_symbol("_GLOBAL_OFFSET_TABLE_"))
            f->get().byte_offset = memory_layout.address(".got");
        if (auto f = out.find_symbol("_DYNAMIC")) {
            if (auto section = out.find_section(".dynamic"))
                f->get().byte_offset = memory_layout.address(".dynamic");
        }
        if (auto section = out.find_section(".init_array")) {
            if (auto start = out.find_symbol("__init_array_start"))
                start->get().byte_offset = memory_layout.address(".init_array");
            if (auto end = out.find_symbol("__init_array_end"))
                end->get().byte_offset = memory_layout.address(".init_array") + section->get().size();
        }
        if (auto section = out.find_section(".fini_array")) {
            if (auto start = out.find_symbol("__fini_array_start"))
                start->get().byte_offset = memory_layout.address(".fini_array");
            if (auto end = out.find_symbol("__fini_array_end"))
                end->get().byte_offset = memory_layout.address(".fini_array") + section->get().size();
        }
    }

    // ================================
    // Resolve and Patch
    // ================================

    // Handle Local Relocations, within object files
    // Basically, *static* symbols (and relocations that reference them) MUST
    // NOT be resolved with anything outside the scope of their own object
    // file. But, we need to do a global layout before knowing what addresses
    // to apply, so, we keep the object's symbols and relocations separate, to
    // ensure that no cross-references happen between object files wrt static
    // symbols.
    for (auto& object : parsed_objects) {
        // relocate within sections of `out` generic object, using leftover
        // relocations in `object` generic object (as the sections have been
        // merged, byte offsets updated already).
        std::erase_if(
            object.relocations,
            [&](const auto& relocation) {
                return perform_relocation(
                    context,
                    binary_blob,
                    object,
                    relocation,
                    global_offset_table,
                    memory_layout
                );
            }
        );
        for (auto& r : object.relocations) {
            auto referenced_symbol = object.find_symbol(r.symbol.name);
            fmt::print(
                stderr,
                "\nclink: Undefined local reference to `{}`\n"
                "    {}"
                "    {}",
                r.symbol.name,
                r.print(),
                referenced_symbol ? referenced_symbol->get().print() : r.symbol.print()
            );
        }
    }

    // Iterate collected relocations, referencing the global symbol table to
    // properly calculate addresses and/or offsets.
    std::erase_if(
        out.relocations,
        [&](const auto& relocation) {
            return perform_relocation(
                context,
                binary_blob,
                out,
                relocation,
                global_offset_table,
                memory_layout
            );
        }
    );
    // fmt::print(
    //     "================================================================\n"
    //     "Global Relocations Resolved ({} relocations remain)\n"
    //     "================================================================\n",
    //     out.relocations.size()
    // );

    lcc::usz limit{16};
    for (auto& r : out.relocations) {
        fmt::print(
            stderr,
            "\nclink: Undefined reference to `{}`\n"
            "    {}"
            "    {}",
            r.symbol.name,
            r.print(),
            r.symbol.print()
        );
        if (not --limit) break;
    }

    FILE* outfile = fopen(executable.c_str(), "wb");
    fwrite(binary_blob.data(), 1, binary_blob.size(), outfile);
    return 0 == fclose(outfile) and out.relocations.empty();
}

} // namespace clink
