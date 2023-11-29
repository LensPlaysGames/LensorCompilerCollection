#include <lcc/codegen/generic_object.hh>
#include <lcc/codegen/x86_64/object.hh>

namespace lcc {
namespace x86_64 {

GenericObject emit_mcode_gobj(Module* module, const MachineDescription& desc, std::vector<MFunction>& mir) {
    GenericObject out{};

    Section text_{".text", {}, 0, 0, false};
    Section data_{".data", {}, 0, 0, false};
    Section bss_{".bss", {}, 0, 0, true};
    out.sections.push_back(text_);
    out.sections.push_back(data_);
    out.sections.push_back(bss_);

    Section& text = out.section(".text");
    Section& data = out.section(".data");
    Section& bss = out.section(".bss");

    LCC_TODO("Actually assemble into machine code, populate symbols, etc");

    return {};
}

} // namespace x86_64
} // namespace lcc
