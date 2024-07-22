#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/rtti.hh>

#include <glint/ast.hh>
#include <glint/ir_gen.hh>

#include <iterator>
#include <utility>
#include <vector>

namespace lcc::glint {

void IRGen::insert(lcc::Inst* inst) {
    LCC_ASSERT(inst, "Invalid argument");
    LCC_ASSERT(ctx, "Invalid context");
    LCC_ASSERT(function && block, "Invalid insert point");
    block->insert(inst);
}

lcc::Type* Convert(Context* ctx, Type* in) {
    switch (in->kind()) {
        case Type::Kind::Builtin:
            switch ((as<BuiltinType>(in))->builtin_kind()) {
                case BuiltinType::BuiltinKind::Bool:
                    return lcc::Type::I1Ty;
                case BuiltinType::BuiltinKind::Byte:
                case BuiltinType::BuiltinKind::Int:
                case BuiltinType::BuiltinKind::UInt:
                    return lcc::IntegerType::Get(ctx, in->size(ctx));
                case BuiltinType::BuiltinKind::Void:
                    return lcc::Type::VoidTy;
                case BuiltinType::BuiltinKind::OverloadSet:
                case BuiltinType::BuiltinKind::Unknown:
                    Diag::ICE("Invalid builtin kind present during IR generation");
            }
            LCC_UNREACHABLE();

        case Type::Kind::FFIType:
            return lcc::IntegerType::Get(ctx, in->size(ctx));

        case Type::Kind::Named:
            Diag::ICE("Sema failed to resolve named type");

        case Type::Kind::Pointer:
        case Type::Kind::Reference:
            return lcc::Type::PtrTy;

        case Type::Kind::Array: {
            const auto& t_array = as<ArrayType>(in);
            return lcc::ArrayType::Get(ctx, t_array->dimension(), Convert(ctx, t_array->element_type()));
        }

        case Type::Kind::Function: {
            const auto& t_function = as<FuncType>(in);

            std::vector<lcc::Type*> param_types{};
            for (const auto& p : t_function->params())
                param_types.push_back(Convert(ctx, p.type));

            return lcc::FunctionType::Get(ctx, Convert(ctx, t_function->return_type()), std::move(param_types));
        }

        case Type::Kind::DynamicArray: {
            const auto& t_dyn_array = as<DynamicArrayType>(in);
            auto struct_type = t_dyn_array->struct_type();
            if (not struct_type) {
                Diag::ICE(
                    "Glint Type-checker should have set DynamicArrayType's cached type (by calling struct_type()), but it appears to be nullptr at time of IRGen"
                );
            }
            std::vector<lcc::Type*> member_types{};
            for (const auto& m : struct_type->members())
                member_types.push_back(Convert(ctx, m.type));
            return lcc::StructType::Get(ctx, std::move(member_types));
        }

        case Type::Kind::Sum: {
            const auto& t_sum = as<SumType>(in);
            auto struct_type = t_sum->struct_type();
            if (not struct_type) {
                Diag::ICE(
                    "Glint Type-checker should have set SumType's cached type (by calling struct_type() or similar), but it appears to be nullptr at time of IRGen"
                );
            }
            std::vector<lcc::Type*> member_types{};
            for (const auto& m : struct_type->members())
                member_types.push_back(Convert(ctx, m.type));
            return lcc::StructType::Get(ctx, std::move(member_types));
        }

        case Type::Kind::Union: {
            const auto& t_union = as<UnionType>(in);
            auto array_type = t_union->array_type();
            if (not array_type) {
                Diag::ICE(
                    "Glint Type-checker should have set UnionType's cached type (by calling array_type()), but it appears to be nullptr at time of IRGen"
                );
            }
            return Convert(ctx, array_type);
        }

        case Type::Kind::Struct: {
            std::vector<lcc::Type*> member_types{};
            for (const auto& m : as<StructType>(in)->members())
                member_types.push_back(Convert(ctx, m.type));

            return lcc::StructType::Get(ctx, std::move(member_types));
        }

        case Type::Kind::Enum:
            return Convert(ctx, as<EnumType>(in)->underlying_type());

        case Type::Kind::Integer:
            return lcc::IntegerType::Get(ctx, in->size(ctx));
    }
    LCC_UNREACHABLE();
}

void glint::IRGen::create_function(glint::FuncDecl* f) {
    generated_ir[f] = new (*module) Function(
        module,
        f->mangled_name(),
        as<FunctionType>(Convert(ctx, f->type())),
        f->linkage(),
        f->call_conv(),
        f->location()
    );
}

/// NOTE: If you `new` an /instruction/ (of, or derived from, type `Inst`), you need to `insert()` it.
void glint::IRGen::generate_expression(glint::Expr* expr) {
    // Already generated
    if (generated_ir[expr]) return;

    // If we call `generate_expression` on an expression that hasn't already
    // been generated, *and* the block we would be trying to insert to is
    // closed, create a new block to insert into.
    // Possible FIXME: I think this is only ever used for early return. Maybe
    // we could just handle early returns specifically and not have this?
    if (block->closed()) {
        // TODO: Unique block name, or something.
        update_block(new (*module) lcc::Block(fmt::format("body.{}", total_block)));
    }

    using K = glint::Expr::Kind;
    switch (expr->kind()) {
        case K::Block: {
            for (auto e : as<BlockExpr>(expr)->children()) generate_expression(e);
        } break;

        // Will be inlined anywhere it is used; a no-op for actual generation.
        case K::IntegerLiteral: {
            auto* literal = new (*module) lcc::IntegerConstant(Convert(ctx, expr->type()), as<IntegerLiteral>(expr)->value());
            generated_ir[expr] = literal;
        } break;

        case K::VarDecl: {
            const auto& decl = as<VarDecl>(expr);
            switch (decl->linkage()) {
                case Linkage::LocalVar: {
                    auto* alloca = new (*module) AllocaInst(Convert(ctx, decl->type()), expr->location());
                    insert(alloca);

                    // Initialise dynamic arrays
                    // code
                    //     foo: [Byte];
                    //     ;; roughly equivalent to
                    //     foo_t : struct {
                    //         data: Byte.ptr;
                    //         size: int;
                    //         capacity: int;
                    //     };
                    //     foo: foo_t;
                    //     foo.capacity := 8;
                    //     foo.size := 0;
                    //     foo.data := malloc 8(sizeof Byte);
                    // endcode
                    if (auto* dynamic_array_t = cast<DynamicArrayType>(decl->type())) {
                        constexpr usz default_dynamic_array_capacity = 8;

                        Value* capacity_val;
                        lcc::Type* capacity_val_type = Convert(ctx, dynamic_array_t->struct_type()->members()[2].type);
                        if (dynamic_array_t->initial_size()) {
                            LCC_ASSERT(
                                is<NameRefExpr>(dynamic_array_t->initial_size()),
                                "Dynamic array initial size must be a variable reference"
                            );

                            generate_expression(dynamic_array_t->initial_size());

                            auto* initial_size = new (*module) LoadInst(
                                capacity_val_type,
                                generated_ir[dynamic_array_t->initial_size()]
                            );
                            insert(initial_size);

                            capacity_val = initial_size;
                        } else {
                            capacity_val = new (*module) IntegerConstant(
                                capacity_val_type,
                                default_dynamic_array_capacity
                            );
                        }

                        // TODO: Don't use hard-coded struct member index.
                        auto* capacity_ptr = new (*module) GetMemberPtrInst(
                            Convert(ctx, dynamic_array_t->struct_type()),
                            alloca,
                            new (*module) IntegerConstant(Convert(ctx, Type::UInt), 2)
                        );
                        auto capacity_store = new (*module) StoreInst(capacity_val, capacity_ptr);
                        insert(capacity_ptr);
                        insert(capacity_store);

                        auto* size_val = new (*module) IntegerConstant(
                            Convert(ctx, dynamic_array_t->struct_type()->members()[1].type),
                            0
                        );
                        auto* size_ptr = new (*module) GetMemberPtrInst(
                            Convert(ctx, dynamic_array_t->struct_type()),
                            alloca,
                            new (*module) IntegerConstant(Convert(ctx, Type::UInt), 1)
                        );
                        auto size_store = new (*module) StoreInst(size_val, size_ptr);
                        insert(size_ptr);
                        insert(size_store);

                        Value* alloc_size_bytes;
                        if (dynamic_array_t->initial_size()) {
                            auto* initial_size = new (*module) LoadInst(
                                Convert(ctx, dynamic_array_t->initial_size()->type()),
                                generated_ir[dynamic_array_t->initial_size()]
                            );
                            insert(initial_size);

                            alloc_size_bytes = initial_size;
                        } else {
                            alloc_size_bytes = new (*module) IntegerConstant(
                                Convert(ctx, Type::UInt),
                                default_dynamic_array_capacity * dynamic_array_t->element_type()->size_in_bytes(ctx)
                            );
                        }

                        auto malloc_func = module->function_by_name("malloc");
                        LCC_ASSERT(malloc_func, "Glint IRGen couldn't find `malloc'");
                        auto* malloc_call = new (*module) CallInst(
                            *malloc_func,
                            as<FunctionType>(malloc_func->type()),
                            {alloc_size_bytes}
                        );
                        insert(malloc_call);

                        auto* data_ptr = new (*module) GetMemberPtrInst(
                            Convert(ctx, dynamic_array_t->struct_type()),
                            alloca,
                            new (*module) IntegerConstant(Convert(ctx, Type::UInt), 0)
                        );
                        auto* data_store = new (*module) StoreInst(malloc_call, data_ptr);
                        insert(data_ptr);
                        insert(data_store);
                    }

                    if (auto* init_expr = decl->init()) {
                        generate_expression(init_expr);
                        // Store generated init_expr into above inserted declaration
                        auto* local_init = new (*module) StoreInst(generated_ir[init_expr], alloca, expr->location());
                        insert(local_init);
                    }
                    generated_ir[expr] = alloca;
                } break;

                case Linkage::Imported: {
                    auto* global = new (*module) GlobalVariable(module, Convert(ctx, decl->type()), decl->name(), decl->linkage(), nullptr);
                    generated_ir[expr] = global;
                } break;

                case Linkage::Reexported: {
                    LCC_TODO();
                } break;

                case Linkage::Internal:
                case Linkage::Used:
                case Linkage::Exported: {
                    Value* init = nullptr;
                    if (auto* init_expr = decl->init()) {
                        generate_expression(init_expr);
                        init = generated_ir[init_expr];
                    }
                    auto* global = new (*module) GlobalVariable(module, Convert(ctx, decl->type()), decl->name(), decl->linkage(), init);
                    generated_ir[expr] = global;
                } break;

                default:
                    fmt::print("Unhandled VarDecl linkage {} (WIP)\n", (int) decl->linkage());
                    break;
            }

        } break;

        case K::Unary: {
            const auto& unary_expr = as<UnaryExpr>(expr);

            generate_expression(unary_expr->operand());

            switch (unary_expr->op()) {
                case TokenKind::Exclam: {
                    auto* zero_imm = new (*module) lcc::IntegerConstant(lcc::IntegerType::Get(ctx, ctx->target()->size_of_pointer), 0);
                    auto zero_imm_bitcast = new (*module) lcc::BitcastInst(zero_imm, lcc::Type::PtrTy);
                    auto* eq = new (*module) EqInst(generated_ir[unary_expr->operand()], zero_imm_bitcast, expr->location());
                    generated_ir[expr] = eq;
                    insert(zero_imm_bitcast);
                    insert(eq);
                } break;
                case TokenKind::Minus: {
                    if (unary_expr->operand()->type()->is_dynamic_array()) {
                        auto* struct_t = as<DynamicArrayType>(unary_expr->operand()->type())->struct_type();
                        auto* data_ptr = new (*module) GetMemberPtrInst(
                            Convert(ctx, struct_t),
                            generated_ir[unary_expr->operand()],
                            new (*module) IntegerConstant(Convert(ctx, Type::UInt), 0)
                        );
                        auto* data_load = new (*module) LoadInst(lcc::Type::PtrTy, data_ptr);
                        insert(data_ptr);
                        insert(data_load);

                        auto free_func = module->function_by_name("free");
                        LCC_ASSERT(free_func, "Glint IRGen couldn't find `free'");
                        auto* free_call = new (*module) CallInst(
                            *free_func,
                            as<FunctionType>(free_func->type()),
                            {data_load}
                        );
                        insert(free_call);
                        break;
                    }

                    LCC_ASSERT(
                        unary_expr->operand()->type()->is_integer(),
                        "Cannot IRGen unary prefix '-' of non-integer type"
                    );

                    auto* neg = new (*module) NegInst(generated_ir[unary_expr->operand()], expr->location());
                    generated_ir[expr] = neg;
                    insert(neg);
                } break;
                case TokenKind::Tilde: {
                    auto* cmpl = new (*module) ComplInst(generated_ir[unary_expr->operand()], expr->location());
                    generated_ir[expr] = cmpl;
                    insert(cmpl);
                } break;
                case TokenKind::At: {
                    generated_ir[expr] = generated_ir[unary_expr->operand()];
                } break;
                case TokenKind::Ampersand: {
                    generated_ir[expr] = generated_ir[unary_expr->operand()];
                } break;

                case TokenKind::Has: {
                    // The following
                    //   has bar.x;
                    // should turn into
                    //   bar.tag = foo.tag.x;

                    auto* m = as<MemberAccessExpr>(unary_expr->operand());

                    auto* struct_type = as<SumType>(m->type())->struct_type();
                    auto* tag_type = Convert(ctx, struct_type->members().at(0).type);

                    // Get pointer to tag member of underlying struct of sum type.
                    auto* tag_ptr = new (*module) GetMemberPtrInst(
                        Convert(ctx, as<SumType>(m->type())->struct_type()),
                        generated_ir[m->object()],
                        new (*module) IntegerConstant(Convert(ctx, Type::UInt), 0),
                        m->location()
                    );
                    // Load tag from that pointer.
                    auto* load = new (*module) LoadInst(tag_type, tag_ptr);
                    // Compare expected tag from member expression to actual tag loaded from
                    // the sum type.
                    auto* expected = new (*module) IntegerConstant(tag_type, m->member() + 1);
                    auto* eq = new (*module) EqInst(load, expected);

                    insert(tag_ptr);
                    insert(load);
                    insert(eq);

                    generated_ir[expr] = eq;
                } break;

                // NOT a unary operator
                case TokenKind::Invalid:
                case TokenKind::Eof:
                case TokenKind::LParen:
                case TokenKind::RParen:
                case TokenKind::LBrack:
                case TokenKind::RBrack:
                case TokenKind::LBrace:
                case TokenKind::RBrace:
                case TokenKind::BangLBrace:
                case TokenKind::Comma:
                case TokenKind::Colon:
                case TokenKind::Semicolon:
                case TokenKind::Dot:
                case TokenKind::Plus:
                case TokenKind::Star:
                case TokenKind::Slash:
                case TokenKind::Percent:
                case TokenKind::Pipe:
                case TokenKind::Caret:
                case TokenKind::Hash:
                case TokenKind::Shl:
                case TokenKind::Shr:
                case TokenKind::Eq:
                case TokenKind::Ne:
                case TokenKind::Lt:
                case TokenKind::Gt:
                case TokenKind::Le:
                case TokenKind::Ge:
                case TokenKind::ColonEq:
                case TokenKind::ColonColon:
                case TokenKind::RightArrow:
                case TokenKind::Ident:
                case TokenKind::Number:
                case TokenKind::String:
                case TokenKind::If:
                case TokenKind::Else:
                case TokenKind::While:
                case TokenKind::Void:
                case TokenKind::Byte:
                case TokenKind::Bool:
                case TokenKind::External:
                case TokenKind::True:
                case TokenKind::False:
                case TokenKind::And:
                case TokenKind::Or:
                case TokenKind::Int:
                case TokenKind::UInt:
                case TokenKind::ArbitraryInt:
                case TokenKind::Sizeof:
                case TokenKind::Alignof:
                case TokenKind::For:
                case TokenKind::Return:
                case TokenKind::Export:
                case TokenKind::Struct:
                case TokenKind::Enum:
                case TokenKind::Union:
                case TokenKind::Sum:
                case TokenKind::Lambda:
                case TokenKind::CShort:
                case TokenKind::CUShort:
                case TokenKind::CInt:
                case TokenKind::CUInt:
                case TokenKind::CLong:
                case TokenKind::CULong:
                case TokenKind::CLongLong:
                case TokenKind::CULongLong:
                case TokenKind::Gensym:
                case TokenKind::MacroArg:
                case TokenKind::Expression:
                    LCC_ASSERT(false, "Sorry, but IRGen of unary operator {} has, apparently, not been implemented. Sorry about that.", ToString(unary_expr->op()));
            }
        } break;

        case K::Binary: {
            const auto& binary_expr = as<BinaryExpr>(expr);
            const auto& lhs_expr = binary_expr->lhs();
            const auto& rhs_expr = binary_expr->rhs();

            // Assignment
            if (binary_expr->op() == TokenKind::ColonEq) {
                // Special handling of assignment to sum type.
                if (auto* sum_type = cast<SumType>(lhs_expr->type())) {
                    LCC_ASSERT(is<MemberAccessExpr>(lhs_expr), "Cannot IRGen assignment to sum type when lhs is not a member access");
                    auto* m = cast<MemberAccessExpr>(lhs_expr);

                    // NOTE: Not lhs_expr, but m->object(). If we generated the member access
                    // we'd get the individual data member but we don't want that.
                    generate_expression(m->object());
                    auto* lhs = generated_ir[m->object()];

                    generate_expression(rhs_expr);
                    auto* rhs = generated_ir[rhs_expr];

                    // The following
                    //   foo : sum { x :cint 0, y :uint 0 };
                    // turns into
                    //   foo : struct { tag :enum { x:0 y:1 }; data :union { :cint :uint }; }
                    //
                    // bar :foo;
                    //
                    // The following
                    //   bar.x := 69;
                    // should turn into
                    //   bar.tag := foo.tag.x;
                    //   (:cint.ptr &bar.data) := 69;
                    auto tag_ptr = new (*module) GetMemberPtrInst(
                        Convert(ctx, sum_type->struct_type()),
                        generated_ir[m->object()],
                        new (*module) IntegerConstant(Convert(ctx, Type::UInt), 0),
                        m->location()
                    );
                    // NOTE: See how tag is member index + 1;
                    auto tag_val = new (*module) IntegerConstant(Convert(ctx, Type::UInt), m->member() + 1);
                    auto store_tag = new (*module) StoreInst(tag_val, tag_ptr, expr->location());

                    insert(tag_ptr);
                    insert(store_tag);

                    auto data_ptr = new (*module) GetMemberPtrInst(
                        Convert(ctx, sum_type->struct_type()),
                        generated_ir[m->object()],
                        new (*module) IntegerConstant(Convert(ctx, Type::UInt), 1),
                        m->location()
                    );
                    auto store_data = new (*module) StoreInst(rhs, data_ptr, expr->location());

                    insert(data_ptr);
                    insert(store_data);

                    generated_ir[expr] = lhs;
                    break;
                }

                generate_expression(lhs_expr);
                auto* lhs = generated_ir[lhs_expr];

                generate_expression(rhs_expr);
                auto* rhs = generated_ir[rhs_expr];

                auto* store = new (*module) StoreInst(rhs, lhs, expr->location());

                // Kind of confusing, but if the AST uses this node, it generally means it
                // wants the lvalue of the thing being assigned to; that means that we
                // need /that/ IR to be used by those users, not the store.
                generated_ir[expr] = lhs;
                insert(store);

                break;
            }

            // Subscript
            if (binary_expr->op() == TokenKind::LBrack) {
                generate_expression(lhs_expr);
                Value* lhs = generated_ir[lhs_expr];

                if (!lhs) LCC_ASSERT(false, "lvalue codegen for lhs of subscript didn't go as expected; sorry");

                generate_expression(rhs_expr);
                auto rhs = generated_ir[rhs_expr];

                Type* lhs_type_stripped = lhs_expr->type()->strip_references();
                if (lhs_type_stripped->is_pointer()) {
                    // Pointer subscript needs scaled by size of pointer base type
                    auto* type_to_scale_by = as<PointerType>(lhs_type_stripped)->element_type();
                    auto* gep = new (*module) GEPInst(Convert(ctx, type_to_scale_by), lhs, rhs, expr->location());
                    generated_ir[expr] = gep;
                    insert(gep);
                } else if (lhs_type_stripped->is_array()) {
                    Inst* gep{};

                    if (not lhs_expr->is_lvalue()) {
                        // array literal subscript (cry)
                        auto* element_type = as<ArrayType>(lhs_type_stripped)->element_type();
                        auto* alloca = new (*module) AllocaInst(Convert(ctx, lhs_type_stripped), expr->location());
                        auto* store = new (*module) StoreInst(lhs, alloca, expr->location());
                        gep = new (*module) GEPInst(Convert(ctx, element_type), alloca, rhs, expr->location());
                        insert(alloca);
                        insert(store);
                    } else {
                        gep = new (*module) GEPInst(
                            Convert(ctx, as<ArrayType>(lhs_type_stripped)->element_type()),
                            lhs,
                            rhs,
                            expr->location()
                        );
                    }
                    insert(gep);
                    generated_ir[expr] = gep;
                } else LCC_ASSERT(false, "Sorry, but the rhs of the subscript has an unexpected type");

                break;
            }

            generate_expression(binary_expr->lhs());
            generate_expression(binary_expr->rhs());

            auto lhs = generated_ir[binary_expr->lhs()];
            auto rhs = generated_ir[binary_expr->rhs()];

            switch (binary_expr->op()) {
                case TokenKind::Plus: {
                    // Arithmetic Addition
                    generated_ir[expr] = new (*module) AddInst(lhs, rhs, expr->location());
                } break;

                case TokenKind::Minus: {
                    // Arithmetic Subtraction
                    generated_ir[expr] = new (*module) SubInst(lhs, rhs, expr->location());
                } break;

                case TokenKind::Star: {
                    // Arithmetic Multiplication
                    generated_ir[expr] = new (*module) MulInst(lhs, rhs, expr->location());
                } break;

                case TokenKind::Slash: {
                    // Arithmetic Division
                    if (binary_expr->lhs()->type()->is_signed_int(ctx) || binary_expr->rhs()->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SDivInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) UDivInst(lhs, rhs, expr->location());
                } break;

                case TokenKind::Percent: {
                    // Arithmetic Modulus (remainder)
                    if (binary_expr->lhs()->type()->is_signed_int(ctx) || binary_expr->rhs()->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SRemInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) URemInst(lhs, rhs, expr->location());
                } break;

                // Comparisons
                case TokenKind::Eq: {
                    // Equality
                    generated_ir[expr] = new (*module) EqInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Ne: {
                    // NOT Equality
                    generated_ir[expr] = new (*module) NeInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Lt: {
                    // Less Than
                    if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SLtInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) ULtInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Gt: {
                    // Greater Than
                    if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SGtInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) UGtInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Le: {
                    // Less Than or Equal To
                    if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SLeInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) ULeInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Ge: {
                    // Greater Than or Equal To
                    if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SGeInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) UGeInst(lhs, rhs, expr->location());
                } break;

                // Binary bitwise operations
                case TokenKind::And:
                case TokenKind::Ampersand: {
                    // Bitwise AND
                    generated_ir[expr] = new (*module) AndInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Or:
                case TokenKind::Pipe: {
                    // Bitwise OR
                    generated_ir[expr] = new (*module) OrInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Shl: {
                    // Bitwise Shift Left
                    generated_ir[expr] = new (*module) ShlInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Shr: {
                    // Bitwise Shift Right
                    // FIXME: SAR or SHL?
                    generated_ir[expr] = new (*module) SarInst(lhs, rhs, expr->location());
                } break;

                // NOT binary operator tokens.
                case TokenKind::ArbitraryInt:
                case TokenKind::At:
                case TokenKind::Bool:
                case TokenKind::Byte:
                case TokenKind::BangLBrace:
                case TokenKind::Caret:
                case TokenKind::Colon:
                case TokenKind::ColonColon:
                case TokenKind::ColonEq: // handled above
                case TokenKind::Comma:
                case TokenKind::Dot:
                case TokenKind::RightArrow:
                case TokenKind::Else:
                case TokenKind::Eof:
                case TokenKind::Exclam:
                case TokenKind::Export:
                case TokenKind::External:
                case TokenKind::Expression:
                case TokenKind::Has:
                case TokenKind::For:
                case TokenKind::Gensym:
                case TokenKind::Hash:
                case TokenKind::Ident:
                case TokenKind::If:
                case TokenKind::CShort:
                case TokenKind::CUShort:
                case TokenKind::CInt:
                case TokenKind::CUInt:
                case TokenKind::CLong:
                case TokenKind::CULong:
                case TokenKind::CLongLong:
                case TokenKind::CULongLong:
                case TokenKind::Int:
                case TokenKind::UInt:
                case TokenKind::Invalid:
                case TokenKind::LBrace:
                case TokenKind::RBrace:
                case TokenKind::LBrack: // handled above
                case TokenKind::RBrack:
                case TokenKind::Sizeof:
                case TokenKind::Alignof:
                case TokenKind::LParen:
                case TokenKind::RParen:
                case TokenKind::Lambda:
                case TokenKind::MacroArg:
                case TokenKind::Number:
                case TokenKind::Return:
                case TokenKind::String:
                case TokenKind::Struct:
                case TokenKind::Enum:
                case TokenKind::Union:
                case TokenKind::Sum:
                case TokenKind::Semicolon:
                case TokenKind::Tilde:
                case TokenKind::Void:
                case TokenKind::True:
                case TokenKind::False:
                case TokenKind::While:
                    Diag(ctx, Diag::Kind::ICError, expr->location(), fmt::format("Unexpected operator {} in binary expression", ToString(binary_expr->op())));
                    break;
            }

            insert(as<Inst>(generated_ir[expr]));
        } break;

        case K::Cast: {
            auto* cast = as<CastExpr>(expr);
            generate_expression(cast->operand());

            lcc::Type* t_to{nullptr};
            if (cast->is_lvalue())
                t_to = lcc::Type::PtrTy;
            else t_to = Convert(ctx, cast->type());

            // Doesn't matter what we are casting from, we don't do anything to cast
            // it to the void.
            if (t_to->is_void()) return;

            lcc::Type* t_from{nullptr};
            if (cast->operand()->is_lvalue())
                t_from = lcc::Type::PtrTy;
            else t_from = generated_ir[cast->operand()]->type();

            /// No-op.
            if (cast->is_ref_to_lvalue() or cast->is_lvalue_to_ref()) {
                generated_ir[expr] = generated_ir[cast->operand()];
                return;
            }

            // FIXED ARRAY TO DYNAMIC ARRAY
            if (cast->operand()->type()->is_array() and cast->type()->is_dynamic_array()) {
                LCC_TODO("IRGen fixed array to dynamic array cast");
                // TODO: We have to create a whole temporary and everything. We probably
                // want to do at least part of this in sema to make our lives easier here.
            }

            // TODO: FIXED ARRAY TO ARRAY VIEW
            // TODO: DYNAMIC ARRAY TO ARRAY VIEW

            if (cast->is_lvalue_to_rvalue()) {
                // SUM TYPE ACCESS
                if (
                    auto* m = lcc::cast<MemberAccessExpr>(cast->operand());
                    m and m->object()->type()->is_sum_type()
                ) {
                    // TODO: This should become a builtin function we call, and eventually
                    // that function should be able to be defined in the Glint program itself
                    // for a given sum type; this way bad accesses can be handled by returning
                    // a default value, calling exit, or doing whatever weird and whacky shit
                    // users are wont to do.

                    // The following
                    //   foo : sum { x :cint 0, y :uint 0 };
                    // turns into
                    //   foo : struct { tag :enum { x:0 y:1 }; data :union { :cint :uint }; }
                    //
                    // bar :foo;
                    //
                    // The following
                    //   bar.x;
                    // should turn into (if tag, then access)
                    //   if (bar.tag = foo.tag.x)
                    //     @(:cint.ptr &bar.data);
                    //   else default_expression of foo.x;

                    auto* sum_type = as<SumType>(m->object()->type());
                    auto* struct_type = sum_type->struct_type();
                    auto* tag_type = Convert(ctx, struct_type->members().at(0).type);

                    auto it = rgs::find_if(sum_type->members(), [&](auto& member) { return member.name == m->name(); });
                    LCC_ASSERT(
                        it != sum_type->members().end(),
                        "Sum type {} has no member named '{}'",
                        sum_type->string(ctx->use_colour_diagnostics()),
                        m->name()
                    );
                    auto member_index = usz(std::distance(sum_type->members().begin(), it));

                    // Get pointer to tag member of underlying struct of sum type.
                    auto* tag_ptr = new (*module) GetMemberPtrInst(
                        Convert(ctx, struct_type),
                        generated_ir[m->object()],
                        new (*module) IntegerConstant(Convert(ctx, Type::UInt), 0),
                        m->location()
                    );
                    // Load tag from that pointer.
                    auto* load_tag = new (*module) LoadInst(tag_type, tag_ptr);
                    // Compare expected tag from member expression to actual tag loaded from
                    // the sum type.
                    auto* expected = new (*module) IntegerConstant(tag_type, member_index + 1);
                    auto* eq = new (*module) EqInst(load_tag, expected);

                    // If eq, load from data member of underlying struct. Otherwise, load
                    // member's default expression.

                    // Create Basic Blocks
                    static usz total_sum_access{0};
                    auto* then = new (*module) lcc::Block(fmt::format("sum.access.good.{}", total_sum_access));
                    auto* otherwise = new (*module) lcc::Block(fmt::format("sum.access.bad.{}", total_sum_access));
                    auto* exit = new (*module) lcc::Block(fmt::format("sum.access.exit.{}", total_sum_access));
                    total_sum_access += 1;

                    insert(tag_ptr);
                    insert(load_tag);
                    insert(eq);
                    insert(new (*module) CondBranchInst(eq, then, otherwise, expr->location()));

                    auto& member = sum_type->members().at(member_index);
                    auto* phi = new (*module) PhiInst(Convert(ctx, member.type), expr->location());

                    update_block(then);
                    auto* load = new (*module) LoadInst(Convert(ctx, member.type), generated_ir[m]);
                    insert(load);
                    insert(new (*module) BranchInst(exit, expr->location()));

                    update_block(otherwise);
                    { // Code generation for bad sum type access
                        // TODO: This is when we should "default construct" a value of the given
                        // type. For now, we just crash the program on bad access.

                        // TODO: Even if we do just crash, it'd be cool to call
                        // `puts("\nGLINT: Bad Sum Type Access\n");` before we do

                        constexpr u8 rc = 7;
                        auto exit_func = module->function_by_name("exit");
                        LCC_ASSERT(exit_func, "Glint IRGen couldn't find `exit'");
                        auto* exit_call = new (*module) CallInst(
                            *exit_func,
                            as<FunctionType>(exit_func->type()),
                            {new (*module) IntegerConstant(Convert(ctx, Type::UInt), rc)}
                        );
                        insert(exit_call);
                    }

                    // auto last_bad_block = block;

                    insert(new (*module) BranchInst(exit, expr->location()));

                    update_block(exit);

                    // NOTE: If you add basic blocks up above, make sure this one points to
                    // the last one in the chain of that control flow (like if's do).
                    phi->set_incoming(load, then);
                    // TODO: When we do default-construct the value for the bad access path,
                    // set incoming phi value
                    // phi->set_incoming(constant, last_bad_block);
                    insert(phi);
                    generated_ir[expr] = phi;
                    return;
                }

                auto* load = new (*module) LoadInst(t_to, generated_ir[cast->operand()], expr->location());
                generated_ir[expr] = load;
                insert(load);
                return;
            }

            if (t_to == t_from) {
                generated_ir[expr] = generated_ir[cast->operand()];
                return;
            }

            usz to_sz = t_to->bits();
            usz from_sz = t_from->bits();

            bool from_signed = cast->operand()->type()->is_signed_int(ctx);

            if (from_sz == to_sz) {
                auto* bitcast = new (*module) BitcastInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                generated_ir[expr] = bitcast;
                insert(bitcast);
            } else if (from_sz < to_sz) {
                // smaller to larger: sign extend if needed, otherwise zero extend.
                if (from_signed) {
                    auto* sign_extend = new (*module) SExtInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                    generated_ir[expr] = sign_extend;
                    insert(sign_extend);
                } else {
                    auto* zero_extend = new (*module) ZExtInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                    generated_ir[expr] = zero_extend;
                    insert(zero_extend);
                }
            } else if (from_sz > to_sz) {
                // larger to smaller: truncate.
                // but not for bools. bools need != 0 emitted instead.
                if (cast->type() == Type::Bool) {
                    auto* zero_imm = new (*module) IntegerConstant(generated_ir[cast->operand()]->type(), 0);
                    auto* ne = new (*module) NeInst(generated_ir[cast->operand()], zero_imm, cast->location());
                    generated_ir[expr] = ne;
                    insert(ne);
                } else {
                    auto* truncate = new (*module) TruncInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                    generated_ir[expr] = truncate;
                    insert(truncate);
                }
            }

        } break;

        case K::EvaluatedConstant: {
            auto* constant = as<ConstantExpr>(expr);
            EvalResult result = constant->value();
            if (result.is_null()) {
                // Zero constant... I dunno
                generated_ir[expr] = new (*module) IntegerConstant(Convert(ctx, constant->type()), 0);
            } else if (result.is_int()) {
                generated_ir[expr] = new (*module) IntegerConstant(Convert(ctx, constant->type()), result.as_int());
            } else if (result.is_string()) {
                LCC_ASSERT(false, "TODO: IR generation of constant strings");
            }
        } break;

        case K::NameRef: {
            auto* name_ref = as<NameRefExpr>(expr);

            if (is<ObjectDecl>(name_ref->target()) and as<ObjectDecl>(name_ref->target())->linkage() == Linkage::Imported)
                generate_expression(name_ref->target());

            LCC_ASSERT(generated_ir[name_ref->target()], "NameRef {} references non-IRGenned expression...", fmt::ptr(name_ref));
            generated_ir[expr] = generated_ir[name_ref->target()];
        } break;

        case K::Return: {
            auto* ret_expr = as<ReturnExpr>(expr);
            if (ret_expr->value()) generate_expression(ret_expr->value());
            auto* ret = new (*module) ReturnInst(generated_ir[ret_expr->value()], expr->location());
            generated_ir[expr] = ret;
            insert(ret);
        } break;

        case K::While: {
            // +---------+
            // | current |
            // +---------+        ,---------+
            //      |             |         |
            // +--------------------+       |
            // | compute condition  |       |
            // | conditional branch |       |
            // +--------------------+       |
            //      |             |         |
            //      |      +------------+   |
            //      |      | body       |   |
            //      |      +------------+   |
            //      |             |         |
            //      |            ...        |
            //      |             |         |
            //  +----------+      `---------+
            //  | exit     |
            //  +----------+

            const auto& while_expr = as<WhileExpr>(expr);

            // TODO: could number whiles to make this easier I guess, for any poor
            // soul who has to debug the IR.

            auto* body = new (*module) lcc::Block(fmt::format("while.body.{}", total_while));
            auto* conditional = new (*module) lcc::Block(fmt::format("while.conditional.{}", total_while));
            auto* exit = new (*module) lcc::Block(fmt::format("while.exit.{}", total_while));
            total_while += 1;

            insert(new (*module) BranchInst(conditional, expr->location()));

            update_block(conditional);
            generate_expression(while_expr->condition());
            insert(new (*module) CondBranchInst(generated_ir[while_expr->condition()], body, exit, expr->location()));

            update_block(body);
            generate_expression(while_expr->body());
            insert(new (*module) BranchInst(conditional, expr->location()));

            update_block(exit);
        } break;

        case K::For: {
            // +------------------+
            // | current          |
            // | emit initialiser |
            // +------------------+
            //      |
            //      |             ,-------------+
            //      |             |             |
            // +--------------------+           |
            // | conditional branch |           |
            // +--------------------+           |
            //      |             |             |
            //      |      +----------------+   |
            //      |      | body           |   |
            //      |      | emit iterator  |   |
            //      |      +----------------+   |
            //      |             |             |
            //      |            ...            |
            //      |             |             |
            //  +----------+      `-------------+
            //  | exit     |
            //  +----------+
            const auto& for_expr = as<ForExpr>(expr);

            auto* conditional = new (*module) lcc::Block(fmt::format("for.conditional.{}", total_for));
            auto* body = new (*module) lcc::Block(fmt::format("for.body.{}", total_for));
            auto* exit = new (*module) lcc::Block(fmt::format("for.exit.{}", total_for));
            total_for += 1;

            generate_expression(for_expr->init());
            insert(new (*module) BranchInst(conditional, expr->location()));

            update_block(conditional);
            generate_expression(for_expr->condition());
            insert(new (*module) CondBranchInst(generated_ir[for_expr->condition()], body, exit, expr->location()));

            update_block(body);
            generate_expression(for_expr->body());
            generate_expression(for_expr->increment());
            insert(new (*module) BranchInst(conditional, expr->location()));

            update_block(exit);
        } break;

        case K::If: {
            ///         +---------+         |
            ///         | current |         |
            ///         +---------+         |
            ///        //         \\        |
            ///    +------+    +------+     |
            ///    | then |    | else |     |
            ///    +------+    +------+     |
            ///           \\  //            |
            ///          +------+           |
            ///          | exit |           |
            ///          +------+           |
            ///                             |
            const auto& if_expr = as<IfExpr>(expr);

            if (not if_expr->otherwise()) {
                auto* then = new (*module) lcc::Block(fmt::format("if.then.{}", total_if));
                // TODO: exit block not needed if then is noreturn.
                auto* exit = new (*module) lcc::Block(fmt::format("if.exit.{}", total_if));
                total_if += 1;

                generate_expression(if_expr->condition());
                insert(new (*module) CondBranchInst(generated_ir[if_expr->condition()], then, exit, expr->location()));

                update_block(then);
                generate_expression(if_expr->then());

                update_block(exit);
                break;
            }

            auto* then = new (*module) lcc::Block(fmt::format("if.then.{}", total_if));
            auto* otherwise = new (*module) lcc::Block(fmt::format("if.else.{}", total_if));
            // TODO: exit block not needed when both then and else are noreturn.
            auto* exit = new (*module) lcc::Block(fmt::format("if.exit.{}", total_if));
            total_if += 1;

            generate_expression(if_expr->condition());
            insert(new (*module) CondBranchInst(generated_ir[if_expr->condition()], then, otherwise, expr->location()));

            auto* phi = new (*module) PhiInst(Convert(ctx, if_expr->type()), expr->location());

            update_block(then);
            generate_expression(if_expr->then());
            auto last_then_block = block;
            insert(new (*module) BranchInst(exit, expr->location()));

            update_block(otherwise);
            generate_expression(if_expr->otherwise());
            auto last_else_block = block;
            insert(new (*module) BranchInst(exit, expr->location()));

            update_block(exit);
            // If the type of an if isn't void, it must return a value, so generate
            // Phi goodness.
            if (not if_expr->type()->is_void()) {
                LCC_ASSERT(if_expr->otherwise(), "IfExpr with non-void type has no otherwise; for an IfExpr to return a value, both then and otherwise expressions need to exist");
                LCC_ASSERT(generated_ir[if_expr->otherwise()], "IfExpr with non-void type has no IR generated for otherwise expression");

                phi->set_incoming(generated_ir[if_expr->then()], last_then_block);
                phi->set_incoming(generated_ir[if_expr->otherwise()], last_else_block);
                insert(phi);
                generated_ir[expr] = phi;
                break;
            }

        } break;

        case K::StringLiteral:
            generated_ir[expr] = string_literals[as<StringLiteral>(expr)->string_index()];
            break;

        case K::Call: {
            const auto& call = as<CallExpr>(expr);

            generate_expression(call->callee());

            auto function_type = as<FunctionType>(Convert(ctx, call->callee_type()));

            std::vector<Value*> args{};
            for (const auto& [i, arg] : vws::enumerate(call->args())) {
                generate_expression(arg);
                if (auto alloca = cast<AllocaInst>(generated_ir[arg])) {
                    if (alloca->allocated_type() == function_type->params().at(usz(i))) {
                        auto* load = new (*module) LoadInst(function_type->params().at(usz(i)), alloca);
                        generated_ir[arg] = load;
                        insert(load);
                    }
                }
                if (not(generated_ir[arg]->type() == function_type->params().at(usz(i)))) {
                    Diag::ICE(
                        ctx,
                        arg->location(),
                        "Glint IRGen: Argument type {} is not equal to expected parameter {} with type {} (from function {})",
                        *generated_ir[arg]->type(),
                        i,
                        *function_type->params().at(usz(i)),
                        *generated_ir[call->callee()]->type()
                    );
                }
                args.push_back(generated_ir[arg]);
            }

            auto ir_call = new (*module) CallInst(generated_ir[call->callee()], function_type, std::move(args), expr->location());

            generated_ir[expr] = ir_call;
            insert(ir_call);
        } break;

        case K::IntrinsicCall: {
            auto intrinsic = as<IntrinsicCallExpr>(expr);
            switch (intrinsic->intrinsic_kind()) {
                /// Handled by sema.
                case IntrinsicKind::BuiltinFilename:
                case IntrinsicKind::BuiltinLine:
                    LCC_UNREACHABLE();

                case IntrinsicKind::BuiltinDebugtrap: {
                    LCC_ASSERT(intrinsic->args().empty(), "No arguments to Debug Trap Builtin");
                    LCC_ASSERT(false, "TODO: Implement debug trap IR instruction, as it needs to make it all the way to MIR");
                } break;

                case IntrinsicKind::BuiltinInline: {
                    LCC_ASSERT(intrinsic->args().empty(), "No arguments to Inline Builtin");
                } break;
                case IntrinsicKind::BuiltinMemCopy: {
                    LCC_ASSERT(intrinsic->args().size() == 3, "Exactly three arguments to Memory Copy Builtin: (destination, source, amountOfBytesToCopy)");
                    LCC_ASSERT(false, "TODO: memcopy ir generation");
                } break;
                case IntrinsicKind::BuiltinMemSet: {
                    LCC_ASSERT(intrinsic->args().size() == 3, "Exactly three arguments to Memory Set Builtin");
                    LCC_ASSERT(false, "TODO: memset ir generation");
                } break;
                case IntrinsicKind::BuiltinSyscall: {
                    LCC_ASSERT(intrinsic->args().empty(), "No arguments to Syscall Builtin");
                } break;
            }
        } break;

        case K::MemberAccess: {
            auto member_access = as<MemberAccessExpr>(expr);

            generate_expression(member_access->object());

            // For member accesses that produce an lvalue sum type, return an lvalue
            // to the sum type's underlying struct. This allows this member access
            // node to act like a selector on the sum type if an operation were to
            // apply to it (like an assignment), but not actually do the calculation
            // to load a value from the underlying struct's data member.
            if (is<SumType>(member_access->type())) {
                generated_ir[expr] = generated_ir[member_access->object()];
                break;
            }

            LCC_ASSERT(
                member_access->struct_type(),
                "MemberAccessExpr should have struct type finalised by the type-checker, but it is NULL at time of IRGen"
            );
            auto struct_t = Convert(ctx, member_access->struct_type());
            auto instance_pointer = generated_ir[member_access->object()];
            auto index = new (*module) IntegerConstant(lcc::IntegerType::Get(ctx, 64), member_access->member());
            auto gmp = new (*module) GetMemberPtrInst(struct_t, instance_pointer, index);
            generated_ir[expr] = gmp;
            insert(gmp);
        } break;

        case K::EnumeratorDecl: {
            auto enumerator = as<EnumeratorDecl>(expr);
            generated_ir[expr] = new (*module) IntegerConstant(Convert(ctx, enumerator->type()), enumerator->value());
        } break;

        case K::CompoundLiteral: {
            // TODO: I need help with this. What IR does it make?
            LCC_ASSERT(false, "TODO: I'm blanking on how to implement compound literal IRGen, so I'm going to wait until I can talk to somebody smarter than me.");
        } break;

        // no-op/handled elsewhere
        case K::Module:
        case K::Sizeof:
        case K::Alignof:
        case K::Type:
        case K::TypeDecl:
        case K::TypeAliasDecl:
        case K::FuncDecl:
        case K::OverloadSet:
            break;
    }
}

void IRGen::generate_function(glint::FuncDecl* f) {
    function = as<Function>(generated_ir[f]);

    if (f->linkage() != Linkage::Imported and f->linkage() != Linkage::Reexported) {
        // Hard to generate code for a function without a body.
        if (auto* expr = f->body()) {
            block = new (*module) lcc::Block(fmt::format("body.{}", total_block));
            update_block(block);

            // Bind param instructions.
            for (auto [i, param] : vws::enumerate(f->param_decls())) {
                auto inst = function->param(usz(i));
                auto alloca = new (*module) AllocaInst(inst->type(), param->location());
                auto store = new (*module) StoreInst(inst, alloca);
                insert(alloca);
                insert(store);
                generated_ir[param] = alloca;
            }

            generate_expression(expr);
        }
    }
}

auto IRGen::Generate(Context* context, glint::Module& mod) -> lcc::Module* {
    auto ir_gen = IRGen(context, mod);

    /// TODO: Move this into a function?
    for (const auto& str : mod.strings) {
        auto* var = GlobalVariable::CreateStringPtr(
            ir_gen.module,
            fmt::format(".str.{}", ir_gen.total_string++),
            str
        );
        ir_gen.string_literals.push_back(var);
    }

    // We must /create/ *all* functions first, before generating the IR for
    // *any* of them. This is because a NameRef in one function may reference
    // another function, and we want all functions to be resolveable.
    for (auto* f : mod.functions())
        ir_gen.create_function(f);

    // BUILT-INS
    // For example, you could read files and parse LCC IR built-ins if you
    // wanted to do that. Or write it in strings, but I don't think that'd be
    // very nice as compared to building it in memory.
    // TODO: Add builtins here like malloc, free, exit, whatever else we
    // insert a ton of up there. Also need to abstract the dynamic array
    // initialisation stuff into a builtin probably oh and definitely the
    // "grow" functionality and stuff like that.

    { // malloc
        auto* malloc_ty = FunctionType::Get(
            context,
            lcc::Type::PtrTy,
            {lcc::IntegerType::Get(context, context->target()->ffi.size_of_long_long)}
        );
        // Registers in module, so, yes this does something.
        new (*ir_gen.module) Function(
            ir_gen.module,
            "malloc",
            malloc_ty,
            Linkage::Imported,
            CallConv::C,
            {}
        );
    }

    { // free
        auto* free_ty = FunctionType::Get(
            context,
            lcc::Type::PtrTy,
            {lcc::IntegerType::Get(context, context->target()->ffi.size_of_long_long)}
        );
        new (*ir_gen.module) Function(
            ir_gen.module,
            "free",
            free_ty,
            Linkage::Imported,
            CallConv::C,
            {}
        );
    }

    { // exit
        auto* exit_ty = FunctionType::Get(
            context,
            lcc::Type::VoidTy,
            {lcc::IntegerType::Get(context, context->target()->ffi.size_of_int)}
        );
        new (*ir_gen.module) Function(
            ir_gen.module,
            "exit",
            exit_ty,
            Linkage::Imported,
            CallConv::C,
            {}
        );
    }

    // Generate IR for functions defined in the Glint module.
    for (auto* f : mod.functions())
        ir_gen.generate_function(f);

    if (mod.is_module()) {
        Section metadata_blob{};
        metadata_blob.name = metadata_section_name;
        metadata_blob.contents() = mod.serialise();

        // TODO: If we are given a CLI option to save module metadata(s) in a
        // separate file in a specific directory, do that. For now just always do
        // it.
        fs::path p{"./"};
        p.replace_filename(mod.name());
        p.replace_extension(metadata_file_extension);
        (void) File::Write(
            metadata_blob.contents().data(),
            metadata_blob.contents().size(),
            p
        );

        // Tell LCC to emit this section in the output code.
        ir_gen.mod()->add_extra_section(metadata_blob);
    }

    return ir_gen.mod();
}

} // namespace lcc::glint
