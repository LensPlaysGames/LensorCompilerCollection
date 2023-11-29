#ifndef LCC_GENERIC_OBJECT_HH
#define LCC_GENERIC_OBJECT_HH

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
};

struct Symbol {
    enum struct Kind {
        NONE,
        FUNCTION,
        STATIC,
        EXPORT, // like static but global
        EXTERNAL,
    } kind{Kind::NONE};

    std::string name{};

    // Originating section
    // THOUGHTS: This /could/ be a Section*, but then we'd have to worry about
    // iterator invalidation and all that. By storing the name, we can iterate
    // the sections and find the section by name, even if the vector has
    // resized, sections removed, etc.
    std::string section_name{};

    // Offset into originating section where symbol is defined.
    usz byte_offset{};
};

struct Relocation {
    enum struct Kind {
        NONE,
        DISPLACEMENT32,
        DISPLACEMENT32_PCREL,
    } kind;
    Symbol symbol;
    isz addend;
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

            LCC_TODO("Handle initialized globals in symbol_from_global");

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
                uninitialized_data.length += var->type()->bytes();
            }
        }
    }
};

} // namespace lcc

#endif /* LCC_GENERIC_OBJECT_HH */
