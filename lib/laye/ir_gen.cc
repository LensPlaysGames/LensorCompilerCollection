#include <intercept/ast.hh>
#include <laye/ir_gen.hh>
#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>
#include <memory>
#include <unordered_map>
#include <vector>

namespace layec = lcc::laye;

lcc::Type* layec::IRGen::Convert(const layec::Type* in) {
    switch (in->kind()) {
        default: {
            Diag::ICE("Unhandled IR type conversion for Laye type {}", ToString(in->kind()));
        }

        case Expr::Kind::TypeFunc: {
            const auto& t = as<FuncType>(in);
            std::vector<lcc::Type*> param_types{};
            for (const auto& p : t->param_types())
                param_types.push_back(Convert(p));
            return lcc::FunctionType::Get(_ctx, Convert(t->return_type()), std::move(param_types));
        }

        case Expr::Kind::TypeStruct: {
            const auto& t = as<StructType>(in);
            std::vector<lcc::Type*> member_types{};
            for (const auto& field : t->fields()) {
                auto field_type = Convert(field.type);
                member_types.push_back(field_type);
            }

            if (t->variants().size()) {
                LCC_ASSERT(false, "variants in struct types");
            }

            auto struct_type = lcc::StructType::Get(context(), member_types, t->name());
            return struct_type;
        }

        case Expr::Kind::TypeNoreturn:
        case Expr::Kind::TypeVoid: {
            return lcc::Type::VoidTy;
        }

        case Expr::Kind::TypeBool: {
            return lcc::IntegerType::Get(_ctx, in->size(_ctx));
        }

        case Expr::Kind::TypeInt: {
            return lcc::IntegerType::Get(_ctx, in->size(_ctx));
        }

        case Expr::Kind::TypeRawptr:
        case Expr::Kind::TypePointer:
        case Expr::Kind::TypeReference:
        case Expr::Kind::TypeBuffer: {
            return lcc::Type::PtrTy;
        }
    }
}

void layec::IRGen::GenerateModule(laye::Module* module) {
    for (auto& imported_module : module->imports()) {
        GenerateModule(imported_module.module);
    }

    for (auto& tld : module->top_level_decls()) {
        if (auto struct_decl = cast<StructDecl>(tld))
            CreateStructDeclType(struct_decl);
    }

    for (auto& tld : module->top_level_decls()) {
        if (auto f = cast<FunctionDecl>(tld))
            CreateIRFunctionValue(f);
    }

    for (auto& tld : module->top_level_decls()) {
        if (auto f = cast<FunctionDecl>(tld))
            GenerateIRFunctionBody(f);
    }
}

auto layec::IRGen::Generate(LayeContext* laye_context, laye::Module* module) -> lcc::Module* {
    auto ir_gen = IRGen{laye_context, module};
    ir_gen.GenerateModule(module);
    return ir_gen.mod();
}

void layec::IRGen::CreateStructDeclType(StructDecl* decl) {
    LCC_ASSERT(decl->type());
    _ir_types[decl] = Convert(decl->type());
}

void layec::IRGen::CreateIRFunctionValue(FunctionDecl* decl) {
    auto params = decl->params();

    std::vector<layec::Type*> param_types{};
    for (auto& param : params) {
        param_types.push_back(param->type);
    }

    auto func_type = new (*laye_mod()) layec::FuncType{
        decl->location(),
        decl->return_type(),
        param_types
    };

    auto linkage = decl->linkage();
    if (not decl->body()) {
        linkage = Linkage::Imported;
    }

    _ir_values[decl] = new (*mod()) Function(
        mod(),
        decl->mangled_name(),
        as<FunctionType>(Convert(func_type)),
        linkage,
        decl->calling_convention(),
        decl->location()
    );
}

void layec::IRGen::GenerateIRFunctionBody(FunctionDecl* decl) {
    tempset curr_func = as<Function>(_ir_values[decl]);
    if (decl->body()) {
        auto block = new (*mod()) lcc::Block(fmt::format("body.{}", total_block));
        UpdateBlock(block);

        for (auto [i, param] : vws::enumerate(decl->params())) {
            auto inst = curr_func->param(usz(i));

            auto alloca = new (*mod()) AllocaInst(inst->type(), param->location);
            Insert(alloca);

            auto store = new (*mod()) StoreInst(inst, alloca);
            Insert(store);

            _ir_params[param] = alloca;
        }

        GenerateStatement(as<BlockStatement>(decl->body()));

        if (not curr_block->terminator()) {
            if (decl->return_type()->is_void())
                Insert(new (*mod()) ReturnInst(nullptr, decl->location()));
            else Insert(new (*mod()) UnreachableInst(decl->location()));
        }
    }
}

void layec::IRGen::GenerateStatement(Statement* statement) {
    using Sk = Statement::Kind;
    switch (statement->kind()) {
        default: {
            LCC_ASSERT(false, "unhandled statement in Laye IR gen {}", ToString(statement->kind()));
        } break;

        case Sk::Block: {
            for (auto child : as<BlockStatement>(statement)->children()) {
                GenerateStatement(child);
            }
        } break;

        case Sk::DeclBinding: {
            auto s = as<BindingDecl>(statement);

            auto alloca = new (*mod()) AllocaInst(Convert(s->type()), s->location());
            Insert(alloca);

            _ir_values[s] = alloca;

            if (s->init()) {
                auto init_val = GenerateExpression(s->init());

                auto store = new (*mod()) StoreInst(init_val, alloca, s->init()->location());
                Insert(store);
            }
        } break;

        case Sk::Assign: {
            auto s = as<AssignStatement>(statement);
            auto target_value = GenerateExpression(s->target());
            LCC_ASSERT(target_value->type()->is_ptr());
            auto assign_value = GenerateExpression(s->value());
            auto store = new (*mod()) StoreInst(assign_value, target_value, s->target()->location());
            Insert(store);
        } break;

        case Sk::Return: {
            auto s = as<ReturnStatement>(statement);
            if (s->is_void_return()) {
                Insert(new (*mod()) ReturnInst(nullptr, statement->location()));
            } else {
                LCC_ASSERT(s->value());
                auto return_value = GenerateExpression(s->value());
                Insert(new (*mod()) ReturnInst(return_value, statement->location()));
            }
        } break;

        case Sk::Xyzzy: {
            // Nothing happened.
        } break;

        case Sk::If: {
            auto s = as<IfStatement>(statement);

            auto pass_block = new (*mod()) Block{fmt::format("if.pass.{}", total_if)};
            auto fail_block = new (*mod()) Block{fmt::format("if.fail.{}", total_if)};
            auto exit_block = new (*mod()) Block{fmt::format("if.exit.{}", total_if)};
            total_if += 1;

            auto cond_value = GenerateExpression(s->condition());
            Insert(new (*mod()) CondBranchInst(cond_value, pass_block, fail_block, s->location()));

            UpdateBlock(pass_block);
            GenerateStatement(s->pass());
            if (not curr_block->terminator())
                Insert(new (*mod()) BranchInst(exit_block, s->location()));

            UpdateBlock(fail_block);
            if (s->fail())
                GenerateStatement(s->fail());
            if (not curr_block->terminator())
                Insert(new (*mod()) BranchInst(exit_block, s->location()));

            UpdateBlock(exit_block);
        } break;

        case Sk::Expr: {
            auto s = as<ExprStatement>(statement);
            GenerateExpression(s->expr());
        } break;
    }
}

lcc::Value* layec::IRGen::GenerateExpression(Expr* expr) {
    using Ek = Expr::Kind;

    if (_ir_values[expr]) return _ir_values[expr];

    switch (expr->kind()) {
        default: {
            LCC_ASSERT(false, "unhandled expr in Laye IR gen {}", ToString(expr->kind()));
        } break;

        case Ek::Cast: {
            auto e = as<CastExpr>(expr);

            auto to = e->type();
            auto from = e->value()->type();

            auto value = GenerateExpression(e->value());

            if (e->cast_kind() == CastKind::LValueToRValueConv) {
                auto load = new (*mod()) LoadInst(Convert(to), value, expr->location());
                Insert(load);
                _ir_values[expr] = load;
                break;
            }

            if (Type::Equal(to, from)) {
                _ir_values[expr] = value;
                break;
            }

            auto IntLikeCast = [&]() {
                usz to_sz = to->size(context());
                usz from_sz = from->size(context());

                if (from_sz == to_sz) {
                    auto cast = new (*mod()) lcc::BitcastInst(value, Convert(to), e->location());
                    Insert(cast);
                    _ir_values[expr] = cast;
                } else if (from_sz < to_sz) {
                    if (from->is_signed_integer()) {
                        auto cast = new (*mod()) lcc::SExtInst(value, Convert(to), e->location());
                        Insert(cast);
                        _ir_values[expr] = cast;
                    } else {
                        auto cast = new (*mod()) lcc::ZExtInst(value, Convert(to), e->location());
                        Insert(cast);
                        _ir_values[expr] = cast;
                    }
                } else if (from_sz > to_sz) {
                    auto cast = new (*mod()) lcc::TruncInst(value, Convert(to), e->location());
                    Insert(cast);
                    _ir_values[expr] = cast;
                }
            };

            if ((from->is_integer() and to->is_integer()) or (from->is_integer() and to->is_rawptr()) or (from->is_rawptr() and to->is_integer())) {
                IntLikeCast();
            } else if (from->is_rawptr()) {
                auto cast = new (*mod()) lcc::BitcastInst(value, Convert(to), e->location());
                Insert(cast);
                _ir_values[expr] = cast;
            } else if ((from->is_pointer() or from->is_buffer()) and to->is_rawptr()) {
                auto cast = new (*mod()) lcc::BitcastInst(value, Convert(to), e->location());
                Insert(cast);
                _ir_values[expr] = cast;
            } else {
                Diag::Error(context(), e->location(), "here");
                LCC_ASSERT(false, "cast from {} to {}", from->string(), to->string());
            }
        } break;

        case Ek::LitInt: {
            auto e = as<LitIntExpr>(expr);
            LCC_ASSERT(e->type());

            auto value = e->value();
            auto type = Convert(e->type());

            _ir_values[expr] = new (*mod()) lcc::IntegerConstant{type, value};
        } break;

        case Ek::Constant: {
            auto e = as<ConstantExpr>(expr);
            LCC_ASSERT(e->type());

            auto value = e->value();
            auto type = Convert(e->type());

            if (value.is_i64()) {
                _ir_values[expr] = new (*mod()) lcc::IntegerConstant{type, uint64_t(value.as_i64())};
            } else if (value.is_string()) {
                auto& string_value = value.as_string()->value();
                auto it = string_literals.find(string_value);

                GlobalVariable* ir_value;
                if (it != string_literals.end()) {
                    ir_value = it->second;
                } else {
                    ir_value = GlobalVariable::CreateStringPtr(mod(), fmt::format(".str.{}", total_string++), string_value);
                    string_literals[string_value] = ir_value;
                }

                _ir_values[expr] = ir_value;
            } else {
                LCC_TODO();
            }
        } break;

        case Ek::Call: {
            auto e = as<CallExpr>(expr);

            auto target = e->target();
            auto target_value = GenerateExpression(target);

            auto target_function_type = as<FunctionType>(Convert(target->type()));

            std::vector<Value*> arg_values{};
            for (auto& arg : e->args()) {
                auto arg_value = GenerateExpression(arg);
                arg_values.push_back(arg_value);
            }

            auto call = new (*mod()) lcc::CallInst(target_value, target_function_type, std::move(arg_values), e->location());
            Insert(call);

            _ir_values[expr] = call;
        } break;

        case Ek::LookupName: {
            auto e = as<NameExpr>(expr);
            auto target_decl = e->target();

            auto target_value = _ir_values.at(target_decl);
            _ir_values[expr] = target_value;
        } break;

        case Ek::LookupPath: {
            auto e = as<PathExpr>(expr);
            auto target_decl = e->target();

            auto target_value = _ir_values.at(target_decl);
            _ir_values[expr] = target_value;
        } break;

        case Ek::Unary: {
            auto e = as<UnaryExpr>(expr);
            auto value = GenerateExpression(e->value());

            switch (e->operator_kind()) {
                default: {
                    LCC_ASSERT(false, "unhandled unary operator kind in Laye IR gen {}", ToString(e->operator_kind()));
                } break;

                case OperatorKind::Address: {
                    _ir_values[expr] = value;
                } break;

                case OperatorKind::Deref: {
                    auto load = new (*mod()) LoadInst(Convert(e->type()), value, expr->location());
                    Insert(load);
                    _ir_values[expr] = load;
                } break;
            }
        } break;

        case Ek::Binary: {
            auto e = as<BinaryExpr>(expr);
            auto lhs = GenerateExpression(e->lhs());
            auto rhs = GenerateExpression(e->rhs());

            switch (e->operator_kind()) {
                default: {
                    LCC_ASSERT(false, "unhandled binary operator kind in Laye IR gen {}", ToString(e->operator_kind()));
                } break;

                case OperatorKind::Add: {
                    auto arith = new (*mod()) AddInst{lhs, rhs, e->location()};
                    Insert(arith);
                    _ir_values[expr] = arith;
                } break;

                case OperatorKind::Sub: {
                    auto arith = new (*mod()) SubInst{lhs, rhs, e->location()};
                    Insert(arith);
                    _ir_values[expr] = arith;
                } break;

                case OperatorKind::Mul: {
                    auto arith = new (*mod()) MulInst{lhs, rhs, e->location()};
                    Insert(arith);
                    _ir_values[expr] = arith;
                } break;

                case OperatorKind::Div: {
                    Inst* arith;
                    if (e->type()->is_integer() && not as<IntType>(e->type())->is_signed()) {
                        arith = new (*mod()) UDivInst{lhs, rhs, e->location()};
                        Insert(arith);
                    } else {
                        arith = new (*mod()) SDivInst{lhs, rhs, e->location()};
                        Insert(arith);
                    }

                    _ir_values[expr] = arith;
                } break;

                case OperatorKind::Equal: {
                    auto compare = new (*mod()) EqInst{lhs, rhs, e->location()};
                    Insert(compare);
                    _ir_values[expr] = compare;
                } break;

                case OperatorKind::NotEqual: {
                    auto compare = new (*mod()) NeInst{lhs, rhs, e->location()};
                    Insert(compare);
                    _ir_values[expr] = compare;
                } break;
            }
        } break;

        case Ek::FieldIndex: {
            auto e = as<FieldIndexExpr>(expr);
            auto target_value = GenerateExpression(e->target());
            LCC_ASSERT(target_value->type()->is_ptr());
            const auto* target_struct_type = as<StructType>(e->target()->type()->strip_references());
            const auto& field_name = e->field_name();

            isz field_index = -1;
            // TODO(local): I guess figure out the C++ism to get the index of a thing, idr it
            for (auto [i, field] : vws::enumerate(target_struct_type->fields())) {
                if (field.name == field_name) {
                    field_index = (isz) i;
                    break;
                }
            }

            LCC_ASSERT(field_index >= 0);
            auto member_index = new (*mod()) IntegerConstant(lcc::IntegerType::Get(_ctx, 64), field_index);
            auto member_ptr = new (*mod()) GetMemberPtrInst(
                Convert(target_struct_type),
                target_value,
                member_index,
                expr->location()
            );

            Insert(member_ptr);
            _ir_values[expr] = member_ptr;
        } break;
    }

    LCC_ASSERT(_ir_values[expr]);
    return _ir_values[expr];
}
