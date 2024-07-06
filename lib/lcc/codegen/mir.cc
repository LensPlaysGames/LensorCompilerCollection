#include <lcc/codegen/mir.hh>
#include <lcc/ir/module.hh>

namespace lcc {

[[nodiscard]]
auto PrintMOperand(const MOperand& op) -> std::string {
    if (std::holds_alternative<MOperandImmediate>(op)) {
        auto imm = std::get<MOperandImmediate>(op);
        return fmt::format("{}.{}", imm.value, imm.size);
    }
    if (std::holds_alternative<MOperandRegister>(op)) {
        auto reg = std::get<MOperandRegister>(op);
        return fmt::format(
            "{}r{}.{}",
            reg.defining_use ? "DEF:" : "",
            reg.value,
            reg.size
        );
    }
    if (std::holds_alternative<MOperandLocal>(op))
        return fmt::format("local({})", +std::get<MOperandLocal>(op));
    if (std::holds_alternative<MOperandGlobal>(op))
        return fmt::format("global({})", std::get<MOperandGlobal>(op)->name());
    if (std::holds_alternative<MOperandFunction>(op))
        return fmt::format("function({})", std::get<MOperandFunction>(op)->name());
    if (std::holds_alternative<MOperandBlock>(op))
        return fmt::format("block({})", std::get<MOperandBlock>(op)->name());
    return "<?>";
}

auto MInstOpcodeToString(usz opcode) -> std::string {
    return opcode < +MInst::Kind::ArchStart
             ? ToString(static_cast<MInst::Kind>(opcode))
             : fmt::format("{}", opcode);
}

[[nodiscard]]
auto PrintMInstImpl(const MInst& inst, auto&& inst_opcode) -> std::string {
    // Instructions that can never produce a value shouldn't show register.
    // clang-format off
    if (inst.opcode() < +MInst::Kind::ArchStart
        and (inst.kind() == MInst::Kind::Store
             or inst.kind() == MInst::Kind::Branch
             or inst.kind() == MInst::Kind::CondBranch
             or inst.kind() == MInst::Kind::Return
             or inst.kind() == MInst::Kind::Unreachable))
        // clang-format on
        return fmt::format(
            "    {}{}{}",
            ToString(inst.kind()),
            inst.all_operands().empty() ? "" : " ",
            fmt::join(vws::transform(inst.all_operands(), PrintMOperand), " ")
        );

    return fmt::format(
        "    {}r{}.{} | {}{}{}{}",
        inst.is_defining() ? "DEF " : "",
        inst.reg(),
        inst.regsize(),
        inst.use_count() or MInst::is_terminator(MInst::Kind(inst.opcode())) ? "" : "Unused ",
        inst_opcode(inst.opcode()),
        inst.all_operands().empty() ? "" : " ",
        fmt::join(vws::transform(inst.all_operands(), PrintMOperand), " ")
    );
}

[[nodiscard]]
auto PrintMInst(const MInst& inst) -> std::string {
    return PrintMInstImpl(inst, MInstOpcodeToString);
}

[[nodiscard]]
auto PrintMBlockImpl(const MBlock& block, auto&& inst_opcode) -> std::string {
    auto out = fmt::format("  {}:\n", block.name());
    if (block.predecessors().size()) {
        out += fmt::format(
            "    predecessors: {}\n",
            fmt::join(block.predecessors(), ", ")
        );
    }
    if (block.successors().size()) {
        out += fmt::format(
            "    successors: {}\n",
            fmt::join(block.successors(), ", ")
        );
    }
    for (auto& instruction : block.instructions()) {
        out += PrintMInstImpl(instruction, inst_opcode);
        if (&instruction != &block.instructions().back())
            out += '\n';
    }
    return out;
};

[[nodiscard]]
auto PrintMBlock(const MBlock& block) -> std::string {
    return PrintMBlockImpl(block, MInstOpcodeToString);
}

[[nodiscard]]
auto PrintMFunctionImpl(const MFunction& function, auto&& inst_opcode) -> std::string {
    const auto PrintLocal = [&](auto&& pair) -> std::string {
        auto&& [index, local] = std::forward<decltype(pair)>(pair);
        return fmt::format(
            "  {}: {} ({} bytes)",
            index,
            *local->allocated_type(),
            local->allocated_type()->bytes()
        );
    };
    auto out = fmt::format(
        "{}:\n{}{}",
        function.name(),
        fmt::join(vws::transform(vws::enumerate(function.locals()), PrintLocal), "\n"),
        function.locals().empty() ? "" : "\n"
    );
    for (auto& block : function.blocks()) {
        out += PrintMBlockImpl(block, inst_opcode);
        out += '\n';
    }
    return out;
};

[[nodiscard]]
auto PrintMFunction(const MFunction& function) -> std::string {
    return PrintMFunctionImpl(function, MInstOpcodeToString);
}

[[nodiscard]]
auto PrintMIR(std::vector<GlobalVariable*>& vars, std::vector<MFunction>& mcode) -> std::string {
    const auto PrintGlobal = [&](const GlobalVariable* global) -> std::string {
        return fmt::format("{}: {}", global->name(), *global->type());
    };
    return fmt::format(
        "{}{}{}{}",
        fmt::join(vws::transform(vars, PrintGlobal), "\n"),
        vars.empty() ? "" : "\n",
        fmt::join(vws::transform(mcode, PrintMFunction), ""),
        mcode.empty() ? "" : "\n"
    );
};

} // namespace lcc
