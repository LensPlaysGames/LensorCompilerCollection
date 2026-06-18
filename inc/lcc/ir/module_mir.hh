#ifndef LCC_MODULE_MIR_HH
#define LCC_MODULE_MIR_HH

#include <lcc/codegen/mir.hh>
#include <lcc/diags.hh>
#include <lcc/forward.hh>
#include <lcc/ir/core.hh>
#include <lcc/utils/result.hh>

#include <memory>
#include <unordered_map>
#include <vector>

namespace lcc {
struct MIRBuildContext {
    Module& mod;

    // THOUGHTS: It may be beneficial cache-wise to use a union between a
    // vector and a map and switch between them at a certain size (like >4096
    // values, off the top of my head).
    // You are going to want to populate this first :).
    std::unordered_map<Value*, usz> virts{};

    std::vector<MFunction> funcs{};

    Function* func_memcpy{};

    // Find machine instruction based on virtual register.
    [[nodiscard]]
    auto minst_by_virtual_register(usz virtual_register) -> MInst*;

    // Handle inlining of values into operands vs using register references.
    // Generate an MIR Operand that references the MIR generated from the given IR Value.
    [[nodiscard]]
    auto moperand_value_reference(
        Function* f_ir,
        MFunction& f,
        Value* v
    ) -> MOperand;

    void populate_virts();
    void populate_funcs();
};
} // namespace lcc

#endif /* LCC_MODULE_MIR_HH */
