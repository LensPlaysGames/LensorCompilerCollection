#ifndef LCC_GENERIC_OBJECT_HH
#define LCC_GENERIC_OBJECT_HH

#include <fmt/format.h>
#include <lcc/ir/ir.hh>
#include <lcc/utils.hh>

namespace lcc {

struct Section {
    std::string name{};

    std::vector<u8> contents{};

    u32 length{0};
    u8 value{0};

    // Iff is_fill is true, contents isn't valid; use length + value to
    // construct contents of section.
    bool is_fill{false};

    std::string print() {
        auto out = fmt::format(
            "[SECTION]: {}\n"
            "CONTENTS:",
            name
        );

        if (is_fill) {
            out += fmt::format(" {} {:x}\n", length, value);
        } else {
            const auto size = contents.size();
            out += fmt::format("\n================ {} bytes\n", size);

            // Print bytes like hexdump, kinda
            usz i = 0;
            if (size > 15) {
                for (; i < size - 16; i += 16) {
                    out += fmt::format(
                        "      {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}  {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
                        contents[i],
                        contents[i + 1],
                        contents[i + 2],
                        contents[i + 3],
                        contents[i + 4],
                        contents[i + 5],
                        contents[i + 6],
                        contents[i + 7],
                        contents[i + 8],
                        contents[i + 9],
                        contents[i + 10],
                        contents[i + 11],
                        contents[i + 12],
                        contents[i + 13],
                        contents[i + 14],
                        contents[i + 15]
                    );

                    out += "      |";
                    // Loop over 16 bytes starting at `i`.
                    for (usz index = i; index < i + 16; ++index) {
                        unsigned char c = contents[index];
                        if (c < ' ' || c > '~') c = '.';
                        out += fmt::format("{}", c);
                    }
                    out += "|\n";
                }
            }

            // Last line
            if (i < size) {
                out += "      ";
                for (usz index = i; index < i + 16; ++index) {
                    // If inbounds, print byte, otherwise print blank space to keep alignment.
                    if (index < size) out += fmt::format("{:02x}", contents[index]);
                    else out += "  ";
                    // Space after every one except for the last.
                    if (index - i != 15) out += ' ';
                    // Inbetween the 7th and 8th (0-based), there is an extra space.
                    if (index - i == 7) out += ' ';
                }
                out += "      |";
                for (usz index = i; index < i + 16; ++index) {
                    if (index < size) {
                        unsigned char c = contents[index];
                        if (c < ' ' || c > '~') c = '.';
                        out += fmt::format("{}", c);
                    } else out += '.';
                }
            }

            out += "\n================\n";
        }
        return out;
    }
};

struct Symbol {
    enum struct Kind {
        NONE,
        FUNCTION,
        STATIC,
        EXPORT, // like static but global
        EXTERNAL,
    } kind{Kind::NONE};

    std::string kind_string(Kind k) {
        switch (k) {
            case Kind::NONE:
                return "NONE";
            case Kind::FUNCTION:
                return "FUNCTION";
            case Kind::STATIC:
                return "STATIC";
            case Kind::EXPORT:
                return "EXPORT";
            case Kind::EXTERNAL:
                return "EXTERNAL";
        }
        LCC_UNREACHABLE();
    }

    std::string name{};

    // Originating section
    // THOUGHTS: This /could/ be a Section*, but then we'd have to worry about
    // iterator invalidation and all that. By storing the name, we can iterate
    // the sections and find the section by name, even if the vector has
    // resized, sections removed, etc.
    std::string section_name{};

    // Offset into originating section where symbol is defined.
    usz byte_offset{};

    std::string print() {
        return fmt::format("[SYMBOL]: {}({}) {}  {}\n", byte_offset, section_name, name, kind_string(kind));
    }
};

struct Relocation {
    enum struct Kind {
        NONE,
        DISPLACEMENT32,
        DISPLACEMENT32_PCREL,
    } kind;

    std::string kind_string(Kind k) {
        switch (k) {
            case Kind::NONE:
                return "NONE";
            case Kind::DISPLACEMENT32:
                return "DISP32";
            case Kind::DISPLACEMENT32_PCREL:
                return "DISP32_PCREL";
        }
        LCC_UNREACHABLE();
    }

    // THOUGHTS: This /could/ be an index into symbols array of GenericObject,
    // if we don't want to duplicate all these short strings and stuff.
    Symbol symbol;

    isz addend;

    std::string print() {
        return fmt::format("[RELOC]: {}  {}{:+}\n", kind_string(kind), symbol.name, addend);
    }
};

constexpr isz align_to(isz value, isz alignment) {
    return value + ((alignment - (value % alignment)) % alignment);
}

struct GenericObject {
    std::vector<Section> sections;
    std::vector<Symbol> symbols;
    std::vector<Relocation> relocations;

    Section& section(std::string_view name) {
        auto found = rgs::find_if(sections, [&](const Section& s) {
            return std::string_view(s.name) == name;
        });
        LCC_ASSERT(found != sections.end(), "Could not find section with name {}", name);
        return *found;
    }
    const Section& section(std::string_view name) const {
        return section(name);
    }

    // NOTE: Requires .data, .bss sections to exist. .bss needs is_fill set to true.
    void symbol_from_global(GlobalVariable* var) {
        const bool imported = var->linkage() == Linkage::Imported || var->linkage() == Linkage::Reexported;
        const bool exported = var->linkage() == Linkage::Exported || var->linkage() == Linkage::Reexported;

        if (var->init()) {
            // Write init to .data section
            Symbol::Kind kind = exported
                                  ? Symbol::Kind::EXPORT
                                  : Symbol::Kind::STATIC;

            Section& initialized_data = section(".data");
            symbols.push_back({kind, var->name(), ".data", initialized_data.contents.size()});

            switch (var->init()->kind()) {
                case Value::Kind::IntegerConstant: {
                    auto integer_constant = as<IntegerConstant>(var->init());
                    LCC_ASSERT(integer_constant->type()->bytes() <= 8, "Oversized integer constant");
                    // FIXME: Big/Little endianness handling.
                    u64 value = integer_constant->value().value();
                    for (usz i = 0; i < integer_constant->type()->bytes(); ++i) {
                        u8 byte = (value >> (i * 8)) & 0xff;
                        initialized_data.contents.push_back(byte);
                    }
                } break;

                case Value::Kind::ArrayConstant: {
                    auto array_constant = as<ArrayConstant>(var->init());
                    for (char c : *array_constant) initialized_data.contents.push_back(u8(c));
                } break;

                default:
                    LCC_ASSERT(false, "Sorry, but global variable initialisation with value kind {} is not supported.", Value::ToString(var->init()->kind()));
            }
        } else {
            if (imported) {
                // Create symbol referencing externally-defined value.
                symbols.push_back({Symbol::Kind::EXTERNAL, var->name(), ".bss", 0});
            } else {
                // Allocate space for variable in .bss section
                Symbol s{Symbol::Kind::STATIC, var->name(), ".bss", 0};
                Section& uninitialized_data = section(".bss");

                // Align uninitialized data to variable type's alignment requirements.
                uninitialized_data.length = u32(align_to(
                    uninitialized_data.length,
                    isz(var->type()->align_bytes())
                ));

                // The symbol will now begin at an aligned address.
                s.byte_offset = uninitialized_data.length;

                symbols.push_back(s);

                // Write uninitialized bytes for this variable.
                uninitialized_data.length += u32(var->type()->bytes());
            }
        }
    }

    std::string print() {
        std::string out{};
        //out += fmt::format("SYMBOLS: {}\n", symbols.size());
        for (auto sym : symbols)
            out += sym.print();
        //out += fmt::format("RELOCATIONS: {}\n", relocations.size());
        for (auto relocation : relocations)
            out += relocation.print();
        //out += fmt::format("SECTIONS: {}\n", sections.size());
        for (auto section : sections)
            out += section.print();
        return out;
    }
};

} // namespace lcc

#endif /* LCC_GENERIC_OBJECT_HH */
