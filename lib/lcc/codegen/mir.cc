#include <lcc/codegen/mir.hh>

#include <lcc/ir/module.hh>

namespace lcc {

[[nodiscard]]
auto PrintMOperand(const MOperand& op) -> std::string {
    if (std::holds_alternative<MOperandImmediate>(op))
        return fmt::format("{}", std::get<MOperandImmediate>(op));
    if (std::holds_alternative<MOperandRegister>(op))
        return fmt::format("r{}.{}", std::get<MOperandRegister>(op).value, std::get<MOperandRegister>(op).size);
    if (std::holds_alternative<MOperandLocal>(op))
        return fmt::format("local({})", +std::get<MOperandLocal>(op));
    if (std::holds_alternative<MOperandStatic>(op))
        return fmt::format("{}", std::get<MOperandStatic>(op)->name());
    if (std::holds_alternative<MOperandFunction>(op))
        return fmt::format("{}", std::get<MOperandFunction>(op)->name());
    if (std::holds_alternative<MOperandBlock>(op))
        return fmt::format("{}", std::get<MOperandBlock>(op)->name());
    return "<?>";
}

[[nodiscard]]
auto PrintMInst(const MInst& inst) -> std::string {
    // Instructions that can never produce a value shouldn't show register.
    if (inst.opcode() < +MInst::Kind::ArchStart
        and (inst.kind() == MInst::Kind::Store
             or inst.kind() == MInst::Kind::Branch
             or inst.kind() == MInst::Kind::CondBranch
             or inst.kind() == MInst::Kind::Return
             or inst.kind() == MInst::Kind::Unreachable))
        return fmt::format("    {}{}{}",
                           ToString(inst.kind()),
                           inst.all_operands().empty() ? "" : " ",
                           fmt::join(vws::transform(inst.all_operands(), PrintMOperand), " "));

    return fmt::format("    r{}.{} | {}{}{}{}",
                       inst.reg(), inst.regsize(),
                       inst.use_count() or MInst::is_terminator(MInst::Kind(inst.opcode()))
                           ? ""
                           : "Unused ",
                       inst.opcode() < +MInst::Kind::ArchStart
                           ? ToString(inst.kind())
                           : fmt::format("{}", inst.opcode()),
                       inst.all_operands().empty() ? "" : " ",
                       fmt::join(vws::transform(inst.all_operands(), PrintMOperand), " "));
}

[[nodiscard]]
auto PrintMBlock(const MBlock& block) -> std::string {
    return fmt::format("  {}:\n{}",
                       block.name(),
                       fmt::join(vws::transform(block.instructions(), PrintMInst), "\n"));
};

[[nodiscard]]
auto PrintMFunction(const MFunction& function) -> std::string {
    const auto PrintLocal = [&](auto&& pair) -> std::string {
        auto&& [first, second] = std::forward<decltype(pair)>(pair);
        return fmt::format("  {}: {} ({} bytes)",
                           first,
                           *second->allocated_type(),
                           second->allocated_type()->bytes());
    };
    return  fmt::format("{}:\n{}{}{}",
                        function.name(),
                        fmt::join(vws::transform(vws::enumerate(function.locals()), PrintLocal), "\n"),
                        function.locals().empty() ? "" : "\n",
                        fmt::join(vws::transform(function.blocks(), PrintMBlock), "\n"));
};

[[nodiscard]]
auto PrintMIR(std::vector<GlobalVariable*>& vars, std::vector<MFunction>& mcode) -> std::string {
    const auto PrintGlobal = [&](const GlobalVariable* global) -> std::string {
        return fmt::format("{}: {}", global->name(), *global->type());
    };
    return fmt::format("{}{}{}{}",
                       fmt::join(vws::transform(vars, PrintGlobal), "\n"),
                       vars.empty() ? "" : "\n",
                       fmt::join(vws::transform(mcode, PrintMFunction), "\n"),
                       mcode.empty() ? "" : "\n");
};

} // namespace lcc
