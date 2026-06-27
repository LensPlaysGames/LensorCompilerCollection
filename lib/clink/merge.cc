#include <clink/merge.hh>

#include <object/generic.hh>

#include <vector>

namespace clink {

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

} // namespace

bool merge(std::vector<lcc::GenericObject>& parsed_objects, lcc::GenericObject& out) {
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

    return true;
}

} // namespace clink
