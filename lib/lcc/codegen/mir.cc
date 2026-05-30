#include <lcc/codegen/mir.hh>
#include <lcc/core.hh>
#include <lcc/ir/module.hh>

#include <fmt/format.h>
#include <fmt/ranges.h>

namespace lcc {

static auto register_category_fmt(Register::Category category) -> std::string {
    if (category != Register::Category::DEFAULT)
        return fmt::format(" ({})", ToString(category));
    return "";
}

[[nodiscard]]
auto PrintMOperand(const MOperand& op) -> std::string {
    static_assert(
        std::variant_size_v<MOperand> == 6,
        "Exhaustive handling of MOperand alternatives in debug printing"
    );
    if (std::holds_alternative<MOperandImmediate>(op)) {
        auto imm = std::get<MOperandImmediate>(op);
        return fmt::format("{}.{}", imm.value, imm.size);
    }
    if (std::holds_alternative<MOperandRegister>(op)) {
        auto reg = std::get<MOperandRegister>(op);
        return fmt::format(
            "{}r{}.{}{}",
            reg.defining_use ? "DEF:" : "",
            reg.value,
            reg.size,
            register_category_fmt(reg.category)
        );
    }
    if (std::holds_alternative<MOperandLocal>(op)) {
        auto l = std::get<MOperandLocal>(op);
        std::string index_string;
        if (l.index == MOperandLocal::absolute_index)
            index_string = "abs";
        else index_string = fmt::format("{}", l.index);
        return fmt::format("local({}){:+}", index_string, l.offset);
    }
    if (std::holds_alternative<MOperandGlobal>(op))
        // It doesn't matter which name we refer to.
        return fmt::format("global({})", std::get<MOperandGlobal>(op)->names().at(0).name);
    if (std::holds_alternative<MOperandFunction>(op))
        // It doesn't matter which name we refer to.
        return fmt::format("function({})", std::get<MOperandFunction>(op)->names().at(0).name);
    if (std::holds_alternative<MOperandBlock>(op))
        return fmt::format("block({})", std::get<MOperandBlock>(op)->name());
    return "<?>";
}

auto MInstOpcodeToString(usz opcode) -> std::string {
    return opcode < +MInst::Kind::ArchStart
             ? ToString(static_cast<MInst::Kind>(opcode))
             : fmt::format("code.{}", opcode);
}

[[nodiscard]]
auto PrintMInstImpl(const MInst& inst, auto&& inst_opcode) -> std::string {
    // Instructions that can never produce a value shouldn't show register.
    // clang-format off
    if (inst.opcode() < +MInst::Kind::ArchStart
        and (inst.kind() == MInst::Kind::Store
             or inst.kind() == MInst::Kind::Branch
             or inst.kind() == MInst::Kind::CondBranch
             or inst.kind() == MInst::Kind::Spill
             or inst.kind() == MInst::Kind::Return
             or inst.kind() == MInst::Kind::Unreachable))
        // clang-format on
        return fmt::format(
            "    {}{}{}",
            ToString(inst.kind()),
            inst.all_operands().empty() ? "" : " ",
            fmt::join(vws::transform(inst.all_operands(), PrintMOperand), " ")
        );

    const auto PrintMInstRegister = [](const MInst& i) {
        return fmt::format(
            "{}r{}.{}{}",
            i.is_defining() ? "DEF " : "",
            i.reg(),
            i.regsize(),
            register_category_fmt((Register::Category) i.regcategory())
        );
    };

    bool used = inst.use_count()
             or MInst::is_terminator(MInst::Kind(inst.opcode()));

    const auto PrintMInstClobbers = [](const MInst& i) {
        bool has_clobbers = i.operand_clobbers().size()
                         or i.register_clobbers().size();

        // FIXME: possibly confusing parameter naming to avoid shadowing
        const auto PrintMInstRegisterClobbers = [](const MInst& ii) {
            return fmt::join(
                vws::transform(
                    ii.register_clobbers(),
                    [&](usz clobbered_register) {
                        return fmt::format("r.{}", clobbered_register);
                    }
                ),
                ", "
            );
        };

        const auto PrintMInstOperandClobbers = [](const MInst& ii) {
            return fmt::join(
                vws::transform(
                    ii.operand_clobbers(),
                    [&](usz operand_index) {
                        return fmt::format("op.{}", operand_index);
                    }
                ),
                ", "
            );
        };

        return fmt::format(
            "{}{}{}{}{}",
            (not has_clobbers) ? "" : " {CLOBBERS: ",
            PrintMInstOperandClobbers(i),
            // If there are register clobbers *and* operand clobbers, we have to
            // separate them somehow.
            i.operand_clobbers().empty() or i.register_clobbers().empty() ? "" : ", ",
            PrintMInstRegisterClobbers(i),
            (not has_clobbers) ? "" : "}"
        );
    };

    const auto PrintMInstOperands = [](const MInst& i) {
        return fmt::format(
            "{}{}",
            i.all_operands().empty() ? "" : " ",
            fmt::join(
                vws::transform(i.all_operands(), PrintMOperand),
                " "
            )
        );
    };

    return fmt::format(
        "    {} | {}{}{}{}",
        PrintMInstRegister(inst),
        used ? "" : "Unused ",
        inst_opcode(inst.opcode()),
        PrintMInstOperands(inst),
        PrintMInstClobbers(inst)
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
    std::string out{};
    for (auto n : function.names()) {
        out += fmt::format(
            "{}:\n",
            n.name
        );
    }
    if (not function.locals().empty()) {
        out += fmt::format(
            "{}\n",
            fmt::join(vws::transform(vws::enumerate(function.locals()), PrintLocal), "\n")
        );
    }
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
        std::string out{};
        for (auto n : global->names()) {
            if (n.name != global->names().at(0).name)
                out += ", ";
            out += fmt::format("{}", n.name);
        }
        // FIXME: Don't emit colors if LCC context doesn't ask us to...
        return fmt::format("{}", *global->type());
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
