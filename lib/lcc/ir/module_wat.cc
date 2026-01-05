#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils.hh>

#include <string>
#include <unordered_map>

namespace lcc {

auto wat_parameter_name(Parameter* p) -> std::string {
    LCC_ASSERT(p);
    return fmt::format("$p{}", p->index());
}

auto wat_block_name(Block* bb) -> std::string {
    LCC_ASSERT(bb);
    return fmt::format("${}", bb->name());
}

auto wat_function_name(Function* f) -> std::string {
    LCC_ASSERT(f);
    if (f->names().empty()) {
        LCC_TODO("WAT Handle unnamed function");
        return fmt::format("lambda{}", "TODO");
    }
    return fmt::format("${}", f->names().at(0).name);
}

auto wat_local_name(AllocaInst* a) -> std::string {
    LCC_ASSERT(a);
    // FIXME: hacky
    return fmt::format("$tmp{}", a->instructions_before_this().size());
}

auto wat_type(Module& m, Type* t) -> std::string {
    switch (t->kind) {
        case Type::Kind::Unknown:
            LCC_UNREACHABLE();

        case Type::Kind::Integer: {
            auto i = as<IntegerType>(t);
            if (i->bitwidth() <= 32) return "i32";
            if (i->bitwidth() <= 64) return "i64";
            LCC_ASSERT(false, "WAT Overlarge integer type");
        }

            // In WebAssembly, pointers are i32's that contain an offset into the linear memory region.
        case Type::Kind::Pointer: return "i32";

        case Type::Kind::Void:
        case Type::Kind::Array:
        case Type::Kind::Function:
        case Type::Kind::Struct:
            LCC_TODO("WAT unhandled type {}", t->string(m.context()->option_use_colour()));
    }
    LCC_UNREACHABLE();
}

// Get WAT representation of a given value. NOTE: May result in
// multiple WAT instructions.
auto wat_value(Module& m, Value* v) -> std::string {
    LCC_ASSERT(v);

    switch (v->kind()) {
        case Value::Kind::ArrayConstant:
        case Value::Kind::Function:
        case Value::Kind::Poison:
            LCC_UNREACHABLE();

        case Value::Kind::IntegerConstant: {
            if (v->type()->bits() <= 32)
                return fmt::format("i32.const {}", 0); // todo

            if (v->type()->bits() <= 64)
                return fmt::format("i64.const {}", 0); // todo

            LCC_ASSERT(
                false,
                "WAT cannot handle integer constant of bitsize {}",
                v->type()->bits()
            );
        }

        case Value::Kind::GlobalVariable: {
            LCC_TODO("WAT global");
            return fmt::format("global {} {}", "todo name", "todo type");
        }

        case Value::Kind::Block: {
            std::string o{};
            for (auto c : as<Block>(v)->instructions())
                o += fmt::format("{}\n", wat_value(m, c));
            return fmt::format(
                "(block {}\n{})",
                wat_block_name(as<Block>(v)),
                o
            );
        }

        case Value::Kind::Parameter: {
            return fmt::format(
                "local.get {}",
                wat_parameter_name(as<Parameter>(v))
            );
        }

        case Value::Kind::Alloca: {
            auto a = as<AllocaInst>(v);
            return fmt::format("local.get {}", wat_local_name(a));
        }

        case Value::Kind::Call: {
            auto c = as<CallInst>(v);

            std::string o{};

            // Push argument values to stack
            for (auto a : c->args()) {
                o += wat_value(m, a);
                o += '\n';
            }

            // Perform call
            switch (c->callee()->kind()) {
                default: LCC_ASSERT(false, "WAT Unhandled callee kind");
                case Value::Kind::Function: {
                    o += fmt::format(
                        "call {}",
                        as<Function>((c->callee()))->names().at(0).name
                    );
                } break;
            }

            return o;
        }

        case Value::Kind::Return:
        case Value::Kind::Store: LCC_UNREACHABLE();

        case Value::Kind::Load: {
            auto l = as<LoadInst>(v);
            return fmt::format(
                "{}\n{}.load",
                wat_value(m, l->ptr()),
                wat_type(m, l->type())
            );
        }

        case Value::Kind::GetElementPtr:
        case Value::Kind::GetMemberPtr:
        case Value::Kind::Intrinsic:
        case Value::Kind::Phi:
        case Value::Kind::Branch:
        case Value::Kind::CondBranch:
        case Value::Kind::Unreachable:
        case Value::Kind::ZExt:
        case Value::Kind::SExt:
        case Value::Kind::Trunc:
        case Value::Kind::Bitcast:
        case Value::Kind::Neg:
        case Value::Kind::Copy:
        case Value::Kind::Compl:
        case Value::Kind::Add:
        case Value::Kind::Sub:
        case Value::Kind::Mul:
        case Value::Kind::SDiv:
        case Value::Kind::UDiv:
        case Value::Kind::SRem:
        case Value::Kind::URem:
        case Value::Kind::Shl:
        case Value::Kind::Sar:
        case Value::Kind::Shr:
        case Value::Kind::And:
        case Value::Kind::Or:
        case Value::Kind::Xor:
        case Value::Kind::Eq:
        case Value::Kind::Ne:
        case Value::Kind::SLt:
        case Value::Kind::SLe:
        case Value::Kind::SGt:
        case Value::Kind::SGe:
        case Value::Kind::ULt:
        case Value::Kind::ULe:
        case Value::Kind::UGt:
        case Value::Kind::UGe:
            v->print();
            LCC_TODO("Sorry, not yet implemented");
    }

    LCC_UNREACHABLE();
}

auto wat_inst(Module& m, Inst* i) -> std::string {
    LCC_ASSERT(i);

    const auto binary = [&](std::string_view op) {
        auto b = as<BinaryInst>(i);
        return fmt::format(
            "({} {} {})",
            op,
            wat_value(m, b->lhs()),
            wat_value(m, b->rhs())
        );
    };

    switch (i->kind()) {
        case Value::Kind::IntegerConstant:
        case Value::Kind::GlobalVariable:
        case Value::Kind::Block:
        case Value::Kind::Parameter:
        case Value::Kind::ArrayConstant:
        case Value::Kind::Function:
        case Value::Kind::Poison:
            LCC_UNREACHABLE();

            // Local definitions handled in function definition.
        case Value::Kind::Alloca:
            return "";

        case Value::Kind::Call: {
            auto c = as<CallInst>(i);

            std::string o{};

            // Push argument values to stack
            for (auto a : c->args()) {
                o += wat_value(m, a);
                o += '\n';
            }

            // Perform call
            switch (c->callee()->kind()) {
                default: LCC_ASSERT(false, "WAT Unhandled callee kind");
                case Value::Kind::Function: {
                    o += fmt::format(
                        "call {}",
                        as<Function>((c->callee()))->names().at(0).name
                    );
                } break;
            }

            return o;
        }

        case Value::Kind::Store: {
            auto s = as<StoreInst>(i);
            // Store to local uses local.set
            if (auto a = cast<AllocaInst>(s->ptr())) {
                return fmt::format(
                    "(local.set {} ({}))",
                    wat_local_name(a),
                    wat_value(m, s->val())
                );
            }
            LCC_ASSERT(false, "WAT Unhandled store position");
        }

        case Value::Kind::Load: {
            auto l = as<LoadInst>(i);
            return fmt::format(
                "{}\n{}.load",
                wat_value(m, l->ptr()),
                wat_type(m, l->type())
            );
        }

        case Value::Kind::Return: {
            auto r = as<ReturnInst>(i);
            if (r->has_value())
                return fmt::format("(return ({}))", wat_value(m, r->val()));
            return "return";
        }

        case Value::Kind::Add: return binary("i32.add");
        case Value::Kind::Sub: return binary("i32.sub");
        case Value::Kind::Mul: return binary("i32.mul");
        case Value::Kind::SDiv: return binary("i32.div_s");
        case Value::Kind::UDiv: return binary("i32.div_u");
        case Value::Kind::And: return binary("i32.and");
        case Value::Kind::Or: return binary("i32.or");

        case Value::Kind::GetElementPtr:
        case Value::Kind::GetMemberPtr:
        case Value::Kind::Intrinsic:
        case Value::Kind::Phi:
        case Value::Kind::Branch:
        case Value::Kind::CondBranch:
        case Value::Kind::Unreachable:
        case Value::Kind::ZExt:
        case Value::Kind::SExt:
        case Value::Kind::Trunc:
        case Value::Kind::Bitcast:
        case Value::Kind::Neg:
        case Value::Kind::Copy:
        case Value::Kind::Compl:
        case Value::Kind::SRem:
        case Value::Kind::URem:
        case Value::Kind::Shl:
        case Value::Kind::Sar:
        case Value::Kind::Shr:
        case Value::Kind::Xor:
        case Value::Kind::Eq:
        case Value::Kind::Ne:
        case Value::Kind::SLt:
        case Value::Kind::SLe:
        case Value::Kind::SGt:
        case Value::Kind::SGe:
        case Value::Kind::ULt:
        case Value::Kind::ULe:
        case Value::Kind::UGt:
        case Value::Kind::UGe:
            i->print();
            LCC_TODO("Sorry, not yet implemented");
    }

    LCC_UNREACHABLE();
}

auto wat_function_signature(Module& m, Function* f) -> std::string {
    auto o = fmt::format("func {}", wat_function_name(f));

    for (auto n : f->names()) {
        if (IsExportedLinkage(n.linkage)) {
            o += fmt::format(
                " (export \"{}\")",
                n.name
            );
            break;
        }
    }

    for (auto p : f->params()) {
        o += fmt::format(
            " (param {} {})",
            wat_parameter_name(p),
            wat_type(m, p->type())
        );
    }
    if (not as<FunctionType>(f->type())->ret()->is_void()) {
        o += fmt::format(
            " (result {})",
            wat_type(m, as<FunctionType>(f->type())->ret())
        );
    }
    return o;
}

// DOES NOT HANDLE IMPORTED FUNCTIONS
auto wat_function(Module& m, Function* f) -> std::string {
    LCC_ASSERT(f);

    std::string o = fmt::format(
        "({}",
        wat_function_signature(m, f)
    );

    for (auto bb : f->blocks()) {
        for (auto i : bb->instructions()) {
            if (auto a = cast<AllocaInst>(i)) {
                o += fmt::format(
                    "\n  (local {} {})",
                    wat_local_name(a),
                    wat_type(m, a->allocated_type())
                );
            }
        }
    }

    std::unordered_map<Block*, std::string> block_names{};
    for (auto bb : f->blocks())
        block_names.emplace(bb, fmt::format("$bb{}", bb->id()));

    for (auto bb : f->blocks()) {
        auto block_wasm_text = fmt::format("(block {})\n", block_names.at(bb));

        // block instructions
        for (auto i : bb->instructions()) {
            auto i_wat = wat_inst(m, i);
            // Some IR instructions are no-ops in webassembly, and we return an empty
            // string for those.
            if (i_wat.size()) {
                // instruction indent
                block_wasm_text += "    ";
                block_wasm_text += i_wat;
                block_wasm_text += '\n';
            }
        }

        o += '\n';
        o += "  "; // block indent
        o += block_wasm_text;
    }

    // function closer
    o += ')';
    return o;
}

[[nodiscard]]
auto Module::as_wat() -> std::string {
    // TODO: What function is the "start" function? We have no way of knowing
    // this currently. It'd be nice if a language could give us a heads-up for
    // a specific function being "the" function. For Glint modules, would be
    // the initialisation function. For Glint programs, "main".

    std::string out{"(module\n"};

    // Imports must come before all non-import definitions.
    for (auto f : code()) {
        // Skip unused functions (why import something we don't use?)
        if (f->users().empty()) continue;

        bool imported{false};
        std::string_view imported_name{};
        for (auto n : f->names()) {
            if (IsImportedLinkage(n.linkage)) {
                imported = true;
                imported_name = n.name;
                break;
            }
        }

        if (not imported) continue;
        out += fmt::format(
            "(import \"{}\" \"{}\" {})\n",
            "env",
            imported_name,
            wat_function(*this, f)
        );
    }

    for (auto g : vars()) {
        out += fmt::format("({})\n", wat_value(*this, g));
    }

    for (auto f : code()) {
        bool imported{false};
        std::string_view imported_name{};
        for (auto n : f->names()) {
            if (IsImportedLinkage(n.linkage)) {
                imported = true;
                imported_name = n.name;
                break;
            }
        }
        if (imported) continue;

        out += fmt::format("{}\n", wat_function(*this, f));
    }

    // module closer
    out += ')';

    return out;
}

} // namespace lcc
