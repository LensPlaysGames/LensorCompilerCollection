#include <glint/ir_gen.hh>

#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/rtti.hh>

#include <glint/ast.hh>

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

                // binary32
                case BuiltinType::BuiltinKind::Float:
                    return lcc::FractionalType::Get(ctx, 32);

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
            Diag::ICE(
                "Sema failed to resolve named type {}",
                as<NamedType>(in)->name()
            );

        case Type::Kind::Pointer:
        case Type::Kind::Reference:
            return lcc::Type::PtrTy;

        case Type::Kind::Array: {
            const auto& t_array = as<ArrayType>(in);
            return lcc::ArrayType::Get(
                ctx,
                t_array->dimension(),
                Convert(ctx, t_array->element_type())
            );
        }

        case Type::Kind::Function: {
            const auto& t_function = as<FuncType>(in);

            std::vector<lcc::Type*> param_types{};
            for (const auto& p : t_function->params())
                param_types.push_back(Convert(ctx, p.type));

            return lcc::FunctionType::Get(
                ctx,
                Convert(ctx, t_function->return_type()),
                std::move(param_types)
            );
        }

        case Type::Kind::DynamicArray: {
            const auto& t_dyn_array = as<DynamicArrayType>(in);
            auto struct_type = t_dyn_array->struct_type();
            if (not struct_type) {
                Diag::ICE(
                    "Glint Type-checker should have set DynamicArrayType's cached type (by calling struct_type()), but it appears to be nullptr at time of IRGen"
                );
            }
            if (not struct_type->ok()) {
                Diag::ICE(
                    "Glint Type-checker should have analysed DynamicArrayType's cached type (by calling Analyse on struct_type()), but it appears to not be analysed at time of IRGen"
                );
            }
            std::vector<lcc::Type*> member_types{};
            for (const auto& m : struct_type->members())
                member_types.push_back(Convert(ctx, m.type));

            return lcc::StructType::Get(
                ctx,
                std::move(member_types),
                t_dyn_array->align(ctx)
            );
        }

        case Type::Kind::ArrayView: {
            const auto& t_view = as<ArrayViewType>(in);
            auto struct_type = t_view->struct_type();
            if (not struct_type) {
                Diag::ICE(
                    "Glint Type-checker should have set ArrayViewType's cached type "
                    "(by calling struct_type()), but it appears to be nullptr at time of IRGen"
                );
            }
            if (not struct_type->ok()) {
                Diag::ICE(
                    "Glint Type-checker should have analysed ArrayViewType's cached type "
                    "(by calling Analyse on struct_type()), but it appears to not be analysed at time of IRGen"
                );
            }
            std::vector<lcc::Type*> member_types{};
            for (const auto& m : struct_type->members())
                member_types.push_back(Convert(ctx, m.type));
            return lcc::StructType::Get(ctx, std::move(member_types), t_view->align(ctx));
        }

        case Type::Kind::Sum: {
            const auto& t_sum = as<SumType>(in);
            auto* struct_type = t_sum->struct_type();
            if (not struct_type) {
                Diag::ICE(
                    "Glint Type-checker should have set SumType's cached type "
                    "(by calling struct_type() or similar), but it appears to be nullptr at time of IRGen"
                );
            }
            if (not struct_type->ok()) {
                Diag::ICE(
                    "Glint Type-checker should have analysed SumType's cached type "
                    "(by calling Analyse on struct_type()), but it appears to not be analysed at time of IRGen"
                );
            }
            std::vector<lcc::Type*> member_types{};
            member_types.reserve(struct_type->members().size());
            for (const auto& m : struct_type->members())
                member_types.emplace_back(Convert(ctx, m.type));
            return lcc::StructType::Get(ctx, std::move(member_types), t_sum->align(ctx));
        }

        case Type::Kind::Union: {
            const auto& t_union = as<UnionType>(in);
            auto* array_type = t_union->array_type();
            if (not array_type) {
                Diag::ICE(
                    "Glint Type-checker should have set UnionType's cached type (by calling array_type()), but it appears to be nullptr at time of IRGen"
                );
            }
            if (not array_type->ok()) {
                Diag::ICE(
                    "Glint Type-checker should have analysed UnionType's cached type (by calling Analyse on array_type()), but it appears to not be analysed at time of IRGen"
                );
            }
            return Convert(ctx, array_type);
        }

        case Type::Kind::Struct: {
            std::vector<lcc::Type*> member_types{};
            for (const auto& m : as<StructType>(in)->members())
                member_types.push_back(Convert(ctx, m.type));

            return lcc::StructType::Get(ctx, std::move(member_types), as<StructType>(in)->align(ctx));
        }

        case Type::Kind::Enum:
            return Convert(ctx, as<EnumType>(in)->underlying_type());

        case Type::Kind::Integer:
            return lcc::IntegerType::Get(ctx, in->size(ctx));

        // A type isn't represented as anything in the final code (for now).
        // Eventually, we /could/ have this be a struct with `bits`, `align`, etc.
        // members.
        case Type::Kind::Type:
            return lcc::Type::VoidTy;

        // A templated struct is an incomplete type; the closest thing to an
        // incomplete type in the IR is void, I'd say.
        case Type::Kind::TemplatedStruct:
            return lcc::Type::VoidTy;

        case Type::Kind::Typeof: {
            {
                Diag::Note(ctx, in->location(), "Here");
            }
            Diag::ICE("Sema should replace TypeofType with the type of it's containing expression");
        }
    }
    LCC_UNREACHABLE();
}

void glint::IRGen::create_function(glint::FuncDecl* f) {
    // no-op
    if (is<TemplatedFuncDecl>(f))
        return;

    std::string name{};

    if (f->function_type()->has_attr(FuncAttr::NoMangle))
        name = f->name();
    else
        name = f->mangled_name();

    generated_ir[f] = new (*module) Function(
        module,
        std::move(name),
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
        update_block(
            new (*module) lcc::Block(fmt::format("body.{}", total_block))
        );
    }

    using K = glint::Expr::Kind;
    switch (expr->kind()) {
        case K::Block: {
            auto* b = as<BlockExpr>(expr);
            if (b->children().empty()) return;
            for (auto* e : b->children())
                generate_expression(e);

            LCC_ASSERT(b->last_expr());
            generated_ir[expr] = generated_ir[*b->last_expr()];
        } break;

        case K::Group: {
            auto* g = as<GroupExpr>(expr);
            if (g->expressions().empty()) return;
            for (auto* e : g->expressions())
                generate_expression(e);
            LCC_ASSERT(not g->expressions().empty());
            generated_ir[expr] = generated_ir[g->expressions().back()];
        } break;

        case K::IntegerLiteral: {
            auto* literal = new (*module) lcc::IntegerConstant(
                Convert(ctx, expr->type()),
                as<IntegerLiteral>(expr)->value()
            );
            generated_ir[expr] = literal;
        } break;

        case K::FractionalLiteral: {
            auto* literal = new (*module) lcc::FractionalConstant(
                Convert(ctx, expr->type()),
                as<FractionalLiteral>(expr)->value()
            );
            generated_ir[expr] = literal;
        } break;

        case K::VarDecl: {
            const auto& decl = as<VarDecl>(expr);

            // Named template is a no-op
            if (decl->init() and is<TemplateExpr>(decl->init())) {
                return;
            }

            // Named struct template is a no-op
            if (decl->init() and is<TemplatedStructType>(decl->init()->type())) {
                return;
            }

            // Type is a no-op
            if (
                is<TypeType>(decl->type())
                or (decl->init() and is<TypeType>(decl->init()->type()))
            ) {
                return;
            }

            switch (decl->linkage()) {
                case Linkage::LocalVar: {
                    auto* alloca = new (*module) AllocaInst(
                        Convert(ctx, decl->type()),
                        expr->location()
                    );
                    insert(alloca);

                    // If present, store the init expression into above generated lvalue.
                    if (auto* init_expr = decl->init()) {
                        if (auto* c = cast<CompoundLiteral>(init_expr)) {
                            // Compound Literal Initialising Expression of a Struct Type
                            if (auto* s = cast<StructType>(decl->type())) {
                                LCC_ASSERT(
                                    c->values().size() == s->members().size(),
                                    "Glint:IRGen: Compound literal initialiser for struct type has invalid amount of members"
                                );
                                for (usz i = 0; i < c->values().size(); ++i) {
                                    auto member = c->values().at(i);
                                    // Confidence check on member name
                                    if (not member.name.empty() and s->members().at(i).name != member.name) {
                                        Diag::ICE(
                                            ctx,
                                            member.value->location(),
                                            "IRGen:Glint: Compound literal member name {} mismatches struct member name {}\n"
                                            "Semantic analysis should have reordered them properly\n",
                                            member.name,
                                            s->members().at(i).name
                                        );
                                    }
                                    generate_expression(member.value);

                                    auto* member_index = new (*module) IntegerConstant(
                                        Convert(ctx, Type::UInt),
                                        i
                                    );
                                    auto* gmp = new (*module) GetMemberPtrInst(
                                        Convert(ctx, s),
                                        alloca,
                                        member_index
                                    );
                                    auto* store = new (*module) StoreInst(
                                        generated_ir[member.value],
                                        gmp
                                    );
                                    insert(gmp);
                                    insert(store);
                                }
                            }
                            // Compound Literal Initialising Expression of a Fixed Array Type
                            else if (auto* a = cast<ArrayType>(decl->type())) {
                                LCC_ASSERT(
                                    c->values().size() == a->dimension(),
                                    "Glint:IRGen: Compound literal initialiser for array type {} has invalid amount of members",
                                    *decl->type()
                                );
                                for (usz i = 0; i < c->values().size(); ++i) {
                                    auto member = c->values().at(i);
                                    generate_expression(member.value);

                                    auto* element_index = new (*module) IntegerConstant(
                                        Convert(ctx, Type::UInt),
                                        i
                                    );
                                    auto* gep = new (*module) GEPInst(
                                        Convert(ctx, a->elem()),
                                        alloca,
                                        element_index
                                    );
                                    auto* store = new (*module) StoreInst(
                                        generated_ir[member.value],
                                        gep
                                    );
                                    insert(gep);
                                    insert(store);
                                }
                            } else LCC_TODO(
                                "IRGen:Glint: Compound literal initialising expression of type {} should be unwrapped into multiple GMP + STOREs",
                                *decl->type()
                            );
                        } else {
                            generate_expression(init_expr);
                            // Store generated init_expr into above inserted declaration
                            auto* local_init = new (*module) StoreInst(
                                generated_ir[init_expr],
                                alloca,
                                expr->location()
                            );
                            insert(local_init);
                        }
                    } else {
                        // TODO: init with zero
                    }
                    generated_ir[expr] = alloca;
                } break;

                case Linkage::Imported: {
                    auto* global = new (*module) GlobalVariable(
                        module,
                        Convert(ctx, decl->type()),
                        decl->name(),
                        decl->linkage(),
                        nullptr
                    );
                    generated_ir[expr] = global;
                } break;

                case Linkage::Reexported: {
                    // Probably the same or very similar to imported...
                    LCC_TODO();
                } break;

                case Linkage::Internal:
                case Linkage::Used:
                case Linkage::Exported: {
                    Value* init{nullptr};
                    if (auto* init_expr = decl->init()) {
                        generate_expression(init_expr);
                        init = generated_ir[init_expr];
                    }
                    auto* global = new (*module) GlobalVariable(
                        module,
                        Convert(ctx, decl->type()),
                        decl->name(),
                        decl->linkage(),
                        init
                    );

                    generated_ir[expr] = global;
                } break;

                default:
                    fmt::print("Unhandled VarDecl linkage {} (WIP)\n", (int) decl->linkage());
                    break;
            }

        } break;

        case K::Unary: {
            const auto& unary_expr = as<UnaryExpr>(expr);

            // // Early handling to prevent sum type member access from inserting tag
            // // checking... Basically, `has` uses a member access expression in a
            // way that alters the semantics of a member access expression. We are not
            // actually accessing any member, it is simply the only real way to
            // reference a specific member of a sum type. Think of the difference
            // between T::x and T.x in C++; we are just using `.` for both.
            switch (unary_expr->op()) {
                default: break; // (!)

                case TokenKind::Has: {
                    // The following
                    //   has bar.x;
                    // should turn into
                    //   bar.tag = foo.tag.x;

                    LCC_ASSERT(is<MemberAccessExpr>(unary_expr->operand()));
                    LCC_ASSERT(
                        is<SumType>(
                            as<MemberAccessExpr>(unary_expr->operand())
                                ->object()
                                ->type()
                        )
                    );

                    auto* m = as<MemberAccessExpr>(unary_expr->operand());
                    auto* struct_type = as<SumType>(m->object()->type())->struct_type();
                    auto* tag_type = Convert(ctx, struct_type->members().at(0).type);

                    generate_expression(m->object());

                    // Get pointer to tag member of underlying struct of sum type.
                    auto* tag_ptr = new (*module) GetMemberPtrInst(
                        Convert(ctx, struct_type),
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
                    return;
                }
            }

            generate_expression(unary_expr->operand());

            switch (unary_expr->op()) {
                case TokenKind::Exclam: {
                    auto* zero_imm = new (*module) lcc::IntegerConstant(
                        lcc::IntegerType::Get(ctx, unary_expr->operand()->type()->size(ctx)),
                        0
                    );
                    auto* zero_imm_bitcast = new (*module) lcc::BitcastInst(
                        zero_imm,
                        Convert(ctx, unary_expr->operand()->type())
                    );
                    auto* eq = new (*module) EqInst(
                        generated_ir[unary_expr->operand()],
                        zero_imm_bitcast,
                        expr->location()
                    );
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
                case TokenKind::BitNOT: {
                    auto* cmpl = new (*module) ComplInst(generated_ir[unary_expr->operand()], expr->location());
                    generated_ir[expr] = cmpl;
                    insert(cmpl);
                } break;

                case TokenKind::Ampersand:
                case TokenKind::At: {
                    generated_ir[expr] = generated_ir[unary_expr->operand()];
                } break;

                case TokenKind::Has: {
                    Diag::ICE("Should have been handled above");
                }

                case TokenKind::StarStar:
                    LCC_TODO("IRGen for unary operator {}", ToString(unary_expr->op()));

                case TokenKind::PlusPlus:
                case TokenKind::MinusMinus:
                    Diag::ICE("IRGen:Glint: Increment and decrement unary expressions should have been lowered to assignment and suchlike during semantic analysis");

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
                case TokenKind::Integer:
                case TokenKind::Fractional:
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
                case TokenKind::Float:
                case TokenKind::ArbitraryInt:
                case TokenKind::Sizeof:
                case TokenKind::Alignof:
                case TokenKind::Match:
                case TokenKind::Switch:
                case TokenKind::Print:
                case TokenKind::For:
                case TokenKind::RangedFor:
                case TokenKind::Return:
                case TokenKind::Export:
                case TokenKind::Struct:
                case TokenKind::Enum:
                case TokenKind::Union:
                case TokenKind::Sum:
                case TokenKind::Lambda:
                case TokenKind::Supplant:
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
                case TokenKind::PlusEq:
                case TokenKind::MinusEq:
                case TokenKind::StarEq:
                case TokenKind::SlashEq:
                case TokenKind::PercentEq:
                case TokenKind::AmpersandEq:
                case TokenKind::PipeEq:
                case TokenKind::CaretEq:
                case TokenKind::TildeEq:
                case TokenKind::LBrackEq:
                case TokenKind::ByteLiteral:
                case TokenKind::Template:
                case TokenKind::Typeof:
                case TokenKind::Tilde:
                case TokenKind::BitAND:
                case TokenKind::BitOR:
                case TokenKind::BitXOR:
                case TokenKind::Apply:
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
                // if (lhs is member access AND member access object is of sum type)
                if (
                    auto* m = cast<MemberAccessExpr>(lhs_expr);
                    m and is<SumType>(m->object()->type())
                ) {
                    auto sum_type = as<SumType>(m->object()->type());

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
                    auto* tag_ptr = new (*module) GetMemberPtrInst(
                        Convert(ctx, sum_type->struct_type()),
                        generated_ir[m->object()],
                        new (*module) IntegerConstant(Convert(ctx, Type::UInt), 0),
                        m->location()
                    );
                    // NOTE: See how tag is member index + 1;
                    auto* tag_val = new (*module) IntegerConstant(Convert(ctx, Type::UInt), m->member() + 1);
                    auto* store_tag = new (*module) StoreInst(tag_val, tag_ptr, expr->location());

                    insert(tag_ptr);
                    insert(store_tag);

                    auto* data_ptr = new (*module) GetMemberPtrInst(
                        Convert(ctx, sum_type->struct_type()),
                        generated_ir[m->object()],
                        new (*module) IntegerConstant(Convert(ctx, Type::UInt), 1),
                        m->location()
                    );
                    auto* store_data = new (*module) StoreInst(rhs, data_ptr, expr->location());

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

                if (not lhs) LCC_ASSERT(false, "lvalue codegen for lhs of subscript didn't go as expected; sorry");

                generate_expression(rhs_expr);
                auto* rhs = generated_ir[rhs_expr];

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

            auto* lhs = generated_ir[binary_expr->lhs()];
            auto* rhs = generated_ir[binary_expr->rhs()];

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
                    if (
                        binary_expr->lhs()->type()->is_signed_int(ctx)
                        or Type::Equal(Type::Float, binary_expr->lhs()->type())
                        or binary_expr->rhs()->type()->is_signed_int(ctx)
                        or Type::Equal(Type::Float, binary_expr->rhs()->type())
                    ) generated_ir[expr] = new (*module) SDivInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) UDivInst(lhs, rhs, expr->location());
                } break;

                case TokenKind::Percent: {
                    // Arithmetic Modulus (remainder)
                    if (
                        binary_expr->lhs()->type()->is_signed_int(ctx)
                        or binary_expr->rhs()->type()->is_signed_int(ctx)
                    )
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
                    if (lhs_expr->type()->is_signed_int(ctx) or rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SLtInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) ULtInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Gt: {
                    // Greater Than
                    if (lhs_expr->type()->is_signed_int(ctx) or rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SGtInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) UGtInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Le: {
                    // Less Than or Equal To
                    if (lhs_expr->type()->is_signed_int(ctx) or rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SLeInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) ULeInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Ge: {
                    // Greater Than or Equal To
                    if (lhs_expr->type()->is_signed_int(ctx) or rhs_expr->type()->is_signed_int(ctx))
                        generated_ir[expr] = new (*module) SGeInst(lhs, rhs, expr->location());
                    else generated_ir[expr] = new (*module) UGeInst(lhs, rhs, expr->location());
                } break;

                // Binary bitwise operations
                case TokenKind::And:
                case TokenKind::BitAND: {
                    // Bitwise AND
                    generated_ir[expr] = new (*module) AndInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::Or:
                case TokenKind::BitOR: {
                    // Bitwise OR
                    generated_ir[expr] = new (*module) OrInst(lhs, rhs, expr->location());
                } break;
                case TokenKind::BitXOR: {
                    // Bitwise XOR
                    generated_ir[expr] = new (*module) XorInst(lhs, rhs, expr->location());
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
                case TokenKind::Colon:
                case TokenKind::ColonColon:
                // handled above
                case TokenKind::ColonEq:
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
                case TokenKind::RangedFor:
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
                case TokenKind::Float:
                case TokenKind::Invalid:
                case TokenKind::LBrace:
                case TokenKind::RBrace:
                case TokenKind::LBrack: // handled above
                case TokenKind::RBrack:
                case TokenKind::Match:
                case TokenKind::Switch:
                case TokenKind::Print:
                case TokenKind::Sizeof:
                case TokenKind::Alignof:
                case TokenKind::LParen:
                case TokenKind::RParen:
                case TokenKind::Lambda:
                case TokenKind::MacroArg:
                case TokenKind::Integer:
                case TokenKind::Fractional:
                case TokenKind::Return:
                case TokenKind::String:
                case TokenKind::Struct:
                case TokenKind::Enum:
                case TokenKind::Union:
                case TokenKind::Sum:
                case TokenKind::Supplant:
                case TokenKind::Semicolon:
                case TokenKind::Tilde:
                case TokenKind::Void:
                case TokenKind::True:
                case TokenKind::False:
                case TokenKind::While:
                case TokenKind::PlusPlus:
                case TokenKind::MinusMinus:
                case TokenKind::StarStar:
                case TokenKind::ByteLiteral:
                case TokenKind::Template:
                case TokenKind::Typeof:
                case TokenKind::BitNOT:
                case TokenKind::Ampersand:
                case TokenKind::Pipe:
                case TokenKind::Caret:
                case TokenKind::Apply:
                    Diag::ICE(
                        ctx,
                        expr->location(),
                        "Unexpected operator {} in binary expression",
                        ToString(binary_expr->op())
                    );

                case TokenKind::PlusEq:
                case TokenKind::MinusEq:
                case TokenKind::StarEq:
                case TokenKind::SlashEq:
                case TokenKind::PercentEq:
                case TokenKind::AmpersandEq:
                case TokenKind::PipeEq:
                case TokenKind::CaretEq:
                case TokenKind::TildeEq:
                case TokenKind::LBrackEq:
                    LCC_TODO(
                        "Binary operator {} in IRGen should have been lowered by semantic analysis",
                        ToString(binary_expr->op())
                    );
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

            // FIXED ARRAY TO ARRAY VIEW
            bool from_array_ref = (cast->operand()->type()->is_pointer() or cast->operand()->type()->is_reference()) and cast->operand()->type()->elem()->is_array();
            if ((from_array_ref or cast->operand()->type()->is_array()) and cast->type()->is_view()) {
                // FIXME: This should be caught during sema with an error.
                LCC_ASSERT(
                    cast->operand()->is_lvalue() or cast->operand()->type()->is_reference(),
                    "Sorry, but to cast an rvalue fixed size array to an array view would require making a temporary for the array (to get an lvalue for the view), and /then/ making a temporary for the view. Because of this, it's not handled, so, make a variable and then a view from that. Thanks."
                );

                // We have to create a whole temporary and everything.

                // alloca underlying struct of view
                auto* view_struct = as<ArrayViewType>(cast->type());
                auto* alloca = new (*module) AllocaInst(Convert(ctx, view_struct));

                // copy operand ptr and store that into view ptr member.
                // TODO: get member by name
                auto* gmp_ptr = new (*module) GetMemberPtrInst(
                    Convert(ctx, view_struct),
                    alloca,
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        0
                    )
                );
                // TODO: If we were to handle non-lvalue case, this would need to copy the
                // pointer to the temporary.
                auto* copy_ptr = new (*module) CopyInst(generated_ir[cast->operand()]);
                auto* store_ptr = new (*module) StoreInst(
                    copy_ptr,
                    gmp_ptr
                );

                // store array dimension into view size member.
                // TODO: get member by name
                auto* gmp_size = new (*module) GetMemberPtrInst(
                    Convert(ctx, view_struct),
                    alloca,
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        1
                    )
                );
                auto* array_type = cast->operand()->type();
                if (from_array_ref) array_type = array_type->elem();
                auto* store_size = new (*module) StoreInst(
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        as<ArrayType>(array_type)->dimension()
                    ),
                    gmp_size
                );

                // load from alloca to get array view value.
                auto* load = new (*module) LoadInst(Convert(ctx, view_struct), alloca);

                insert(alloca);

                insert(gmp_ptr);
                insert(copy_ptr);
                insert(store_ptr);

                insert(gmp_size);
                insert(store_size);

                insert(load);

                generated_ir[expr] = load;

                return;
            }

            // DYNAMIC ARRAY TO ARRAY VIEW
            if (cast->operand()->type()->is_dynamic_array() and cast->type()->is_view()) {
                LCC_ASSERT(
                    cast->operand()->is_lvalue() or cast->operand()->type()->is_reference(),
                    "Sorry, but to cast an rvalue dynamic array to an array view would require making a temporary for the dynamic array (to get an lvalue to copy from for the view), and /then/ making a temporary for the view and storing into that. Because of this, it's not handled, so, make a variable and then a view from that. Thanks."
                );

                // We have to create a whole temporary and everything.

                // IR PSEUDOCODE
                // %0 == cast operand
                //
                //   %1 = alloca @view
                //   %2 = gmp @dynarray from %0 at ".data"
                //   %3 = load ptr %2
                //   %4 = gmp @view from %1 at ".data"
                //   store ptr %3 into %4
                //   %5 = gmp @dynarray from %0 at ".size"
                //   %6 = load i32 %5
                //   %7 = zext i64 %6
                //   %8 = gmp @view from %1 at ".size"
                //   store ptr %7 into %8
                //
                // TODO: It sure would be cool to be able to write snippets to insert like
                // this with the ability to somehow parameterise/templatise on given
                // type and value inputs.

                auto* dynarray = as<DynamicArrayType>(cast->operand()->type());

                // alloca underlying struct of view
                auto* view_struct = as<ArrayViewType>(cast->type());
                auto* alloca = new (*module) AllocaInst(Convert(ctx, view_struct));

                // load ptr from ".data" dynamic array and store that into view ptr member.
                auto* gmp_data_ptr = new (*module) GetMemberPtrInst(
                    Convert(ctx, dynarray->struct_type()),
                    generated_ir[cast->operand()],
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        0
                    )
                );
                auto* load_data_ptr = new (*module) LoadInst(
                    lcc::Type::PtrTy,
                    gmp_data_ptr
                );

                auto* gmp_ptr = new (*module) GetMemberPtrInst(
                    Convert(ctx, view_struct),
                    alloca,
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        0
                    )
                );
                auto* store_ptr = new (*module) StoreInst(
                    load_data_ptr,
                    gmp_ptr
                );

                // Store dynamic array size into view size member.

                // load integer from ".size" dynamic array member and store that into view size member.
                auto* gmp_data_size = new (*module) GetMemberPtrInst(
                    Convert(ctx, dynarray->struct_type()),
                    generated_ir[cast->operand()],
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        1
                    )
                );
                auto* load_data_size = new (*module) LoadInst(
                    Convert(ctx, dynarray->struct_type()->members()[1].type),
                    gmp_data_size
                );
                // TODO: Do a cast type thing if the loaded data size isn't the same type
                // as the size of the view size member we are storing it into.

                // TODO: get member by name
                auto* gmp_size = new (*module) GetMemberPtrInst(
                    Convert(ctx, view_struct),
                    alloca,
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        1
                    )
                );
                auto* store_size = new (*module) StoreInst(
                    load_data_size,
                    gmp_size
                );

                // load from alloca to get array view value.
                auto* load = new (*module) LoadInst(Convert(ctx, view_struct), alloca);

                insert(alloca);

                insert(gmp_data_ptr);
                insert(load_data_ptr);
                insert(gmp_ptr);
                insert(store_ptr);

                insert(gmp_data_size);
                insert(load_data_size);
                insert(gmp_size);
                insert(store_size);

                insert(load);

                generated_ir[expr] = load;

                return;
            }

            if (cast->is_lvalue_to_rvalue()) {
                if (not generated_ir[cast->operand()]) {
                    expr->print(true);
                    Diag::ICE("Cast cannot insert load when operand has not been IRGenned");
                }

                auto* load = new (*module) LoadInst(
                    t_to,
                    generated_ir[cast->operand()],
                    expr->location()
                );
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

            LCC_ASSERT(
                name_ref->target(),
                "Sema needs to set the target of NameRefExpr"
            );

            if (
                is<ObjectDecl>(name_ref->target())
                and as<ObjectDecl>(name_ref->target())->linkage() == Linkage::Imported
            ) generate_expression(name_ref->target());

            LCC_ASSERT(
                generated_ir[name_ref->target()],
                "NameRef {} ({}) references non-IRGenned expression...",
                name_ref->name(),
                fmt::ptr(name_ref)
            );
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
            const auto copy = generated_ir;
            generate_expression(while_expr->body());
            insert(new (*module) BranchInst(conditional, expr->location()));

            // Instructions generated in the while loop must not be used by
            // instructions in the exit block (in case the while loop never runs).
            // This resets the cache of AST Node -> IR Instruction to what it was
            // before we generated the while body, which means if a node that was
            // referenced for the first time in the while body is referenced after
            // this, it will have new instructions generated for it (as it should be).
            generated_ir = copy;

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
            const auto& if_expr = as<IfExpr>(expr);

            if (not if_expr->otherwise()) {
                ///         +---------+         |
                ///         | current |         |
                ///         +---------+         |
                ///        //      \\           |
                ///    +------+     \\          |
                ///    | then |      |          |
                ///    +------+     //          |
                ///           \\   //           |
                ///          +------+           |
                ///          | exit |           |
                ///          +------+           |
                ///                             |
                auto* then = new (*module) lcc::Block(fmt::format("if.then.{}", total_if));
                // TODO: exit block not needed if then is noreturn.
                auto* exit = new (*module) lcc::Block(fmt::format("if.exit.{}", total_if));
                total_if += 1;

                generate_expression(if_expr->condition());
                insert(new (*module) CondBranchInst(
                    generated_ir[if_expr->condition()],
                    then,
                    exit,
                    expr->location()
                ));

                update_block(then);
                auto then_copy = generated_ir;
                generate_expression(if_expr->then());
                auto then_ir = generated_ir[if_expr->then()];
                // Basically, if the if expression is a 'return' or something like that,
                // the block is already closed, and we do not need to (automatically)
                // branch out, since the user did explicitly...
                auto last_then_block = block;
                if (not last_then_block->closed())
                    insert(new (*module) BranchInst(exit, expr->location()));

                // If anything outside of the then branch references an AST node that was
                // used in this then branch, it needs to re-generate the IR for that
                // node (as it wasn't in the control flow of this branch).
                then_copy[if_expr->then()] = then_ir;
                generated_ir = then_copy;

                update_block(exit);
                break;
            }

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
            auto* then = new (*module) lcc::Block(fmt::format("if.then.{}", total_if));
            auto* otherwise = new (*module) lcc::Block(fmt::format("if.else.{}", total_if));
            // TODO: exit block not needed when both then and else are noreturn.
            auto* exit = new (*module) lcc::Block(fmt::format("if.exit.{}", total_if));
            total_if += 1;

            generate_expression(if_expr->condition());
            insert(new (*module) CondBranchInst(
                generated_ir[if_expr->condition()],
                then,
                otherwise,
                expr->location()
            ));

            auto* phi = new (*module) PhiInst(
                Convert(ctx, if_expr->type()),
                expr->location()
            );

            update_block(then);
            auto then_copy = generated_ir;
            generate_expression(if_expr->then());
            auto then_ir = generated_ir[if_expr->then()];
            auto* last_then_block = block;
            if (not last_then_block->closed())
                insert(new (*module) BranchInst(exit, expr->location()));

            // Now that we've generated the then expression, we go on to generate the
            // otherwise expression. The weirdness with generated_ir here is due to
            // the fact that our AST is actually a DAG; there may be multiple
            // appearances of a single node at different points.
            // The issue with that, in this context, is that if generated_ir already
            // contains an entry for a node, it will not generate anything.
            // This is an issue if a single tree node is used /for the first time/ in
            // each of these then/otherwise branches. We need each of these branches
            // to "reset" the nodes they have generated, such that, if they are
            // encountered again, they get codegenned.
            then_copy[if_expr->then()] = then_ir;
            generated_ir = then_copy;

            update_block(otherwise);
            generate_expression(if_expr->otherwise());
            auto* last_else_block = block;
            if (not last_else_block->closed())
                insert(new (*module) BranchInst(exit, expr->location()));

            update_block(exit);
            // If the type of an if isn't void, it must return a value, so generate
            // Phi goodness.
            if (not if_expr->type()->is_void()) {
                LCC_ASSERT(
                    if_expr->otherwise(),
                    "IfExpr with non-void type has no otherwise; for an"
                    " IfExpr to return a value, both then and otherwise expressions need to exist"
                );
                LCC_ASSERT(
                    generated_ir[if_expr->otherwise()],
                    "IfExpr with non-void type {} has no IR generated for otherwise expression",
                    *if_expr->type()
                );

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

            auto* function_type = as<FunctionType>(Convert(ctx, call->callee_type()));

            std::vector<Value*> args{};
            for (const auto& [i, arg] : vws::enumerate(call->args())) {
                generate_expression(arg);
                if (auto* alloca = cast<AllocaInst>(generated_ir[arg])) {
                    if (alloca->allocated_type() == function_type->params().at(usz(i))) {
                        auto* load = new (*module) LoadInst(function_type->params().at(usz(i)), alloca);
                        generated_ir[arg] = load;
                        insert(load);
                    }
                }
                if (not (generated_ir[arg]->type() == function_type->params().at(usz(i)))) {
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

            auto* ir_call = new (*module) CallInst(
                generated_ir[call->callee()],
                function_type,
                std::move(args),
                expr->location()
            );

            generated_ir[expr] = ir_call;
            insert(ir_call);
        } break;

        case K::IntrinsicCall: {
            auto* intrinsic = as<IntrinsicCallExpr>(expr);
            switch (intrinsic->intrinsic_kind()) {
                /// Handled by sema.
                case IntrinsicKind::BuiltinFilename:
                case IntrinsicKind::BuiltinLine:
                    LCC_UNREACHABLE();

                case IntrinsicKind::BuiltinDebugtrap: {
                    LCC_ASSERT(
                        intrinsic->args().empty(),
                        "No arguments to Debug Trap Builtin"
                    );
                    LCC_ASSERT(
                        false,
                        "TODO: Implement debug trap IR instruction, as it needs to make it all the way to MIR"
                    );
                } break;

                case IntrinsicKind::BuiltinInline: {
                    LCC_ASSERT(
                        intrinsic->args().empty(),
                        "No arguments to Inline Builtin"
                    );
                } break;
                case IntrinsicKind::BuiltinMemCopy: {
                    LCC_ASSERT(
                        intrinsic->args().size() == 3,
                        "Exactly three arguments to Memory Copy Builtin: (destination, source, amountOfBytesToCopy)"
                    );
                    LCC_ASSERT(false, "TODO: memcopy ir generation");
                } break;
                case IntrinsicKind::BuiltinMemSet: {
                    LCC_ASSERT(
                        intrinsic->args().size() == 3,
                        "Exactly three arguments to Memory Set Builtin"
                    );
                    LCC_ASSERT(false, "TODO: memset ir generation");
                } break;
                case IntrinsicKind::BuiltinSyscall: {
                    LCC_ASSERT(intrinsic->args().empty(), "No arguments to Syscall Builtin");
                } break;
            }
        } break;

        case K::MemberAccess: {
            auto* member_access = as<MemberAccessExpr>(expr);

            generate_expression(member_access->object());

            // For member access of a sum type, return an lvalue to where the data is
            // actually stored.
            if (is<SumType>(member_access->object()->type())) {
                auto m = member_access;
                auto* sum_type = as<SumType>(m->object()->type());
                auto* struct_type = sum_type->struct_type();
                auto* tag_type = Convert(ctx, struct_type->members().at(0).type);

                auto it = rgs::find_if(
                    sum_type->members(),
                    [&](auto& member) { return member.name == m->name(); }
                );
                LCC_ASSERT(
                    it != sum_type->members().end(),
                    "Sum type {} has no member named '{}'",
                    sum_type->string(ctx->option_use_colour()),
                    m->name()
                );
                auto member_index = usz(
                    std::distance(sum_type->members().begin(), it)
                );

                // Get pointer to tag member of underlying struct of sum type.
                auto* tag_ptr = new (*module) GetMemberPtrInst(
                    Convert(ctx, struct_type),
                    generated_ir[m->object()],
                    new (*module) IntegerConstant(
                        Convert(ctx, Type::UInt),
                        0
                    ),
                    m->location()
                );
                // Load tag from that pointer.
                auto* load_tag = new (*module) LoadInst(tag_type, tag_ptr);
                // Compare expected tag from member expression to actual tag loaded from
                // the sum type.
                auto* expected = new (*module) IntegerConstant(
                    tag_type,
                    member_index + 1
                );
                auto* eq = new (*module) EqInst(load_tag, expected);

                // If eq, load from data member of underlying struct.
                // Otherwise, crash.

                // Create Basic Blocks
                static usz total_sum_access{0};
                auto* then = new (*module) lcc::Block(
                    fmt::format("sum.access.good.{}", total_sum_access)
                );
                auto* otherwise = new (*module) lcc::Block(
                    fmt::format("sum.access.bad.{}", total_sum_access)
                );
                auto* exit = new (*module) lcc::Block(
                    fmt::format("sum.access.exit.{}", total_sum_access)
                );
                total_sum_access += 1;

                insert(tag_ptr);
                insert(load_tag);
                insert(eq);
                insert(new (*module) CondBranchInst(
                    eq,
                    then,
                    otherwise,
                    expr->location()
                ));

                // ptr type because member access results in an lvalue. Load happens
                // during lvalue to rvalue conversion.
                auto* phi = new (*module) PhiInst(lcc::Type::PtrTy, expr->location());

                update_block(then);
                auto* data_ptr = new (*module) GetMemberPtrInst(
                    Convert(ctx, struct_type),
                    generated_ir[m->object()],
                    // NOTE: `1` magic number is index of "data" member of sum type's underlying struct.
                    new (*module) IntegerConstant(Convert(ctx, Type::UInt), 1),
                    m->location()
                );
                insert(data_ptr);
                insert(new (*module) BranchInst(exit, expr->location()));

                update_block(otherwise);
                { // Code generation for bad sum type access

                    // TODO:
                    // It'd be cool to call
                    //   `puts("GLINT: Bad Sum Type Access");`
                    // before we crash.

                    constexpr u8 rc = 7;
                    auto exit_func = module->function_by_name("exit");
                    LCC_ASSERT(exit_func, "Glint IRGen couldn't find `exit'");
                    auto* exit_call = new (*module) CallInst(
                        *exit_func,
                        as<FunctionType>(exit_func->type()),
                        {new (*module) IntegerConstant(
                            Convert(ctx, Type::UInt),
                            rc
                        )}
                    );
                    insert(exit_call);
                }

                insert(new (*module) BranchInst(exit, expr->location()));

                update_block(exit);

                // NOTE: If you add basic blocks up above, make sure this one points to
                // the last one in the chain of that control flow (like if's do).
                phi->set_incoming(data_ptr, then);
                insert(phi);
                generated_ir[expr] = phi;
                break;
            }

            LCC_ASSERT(
                member_access->struct_type(),
                "MemberAccessExpr should have struct type finalised by the type-checker, but it is NULL at time of IRGen"
            );
            auto* struct_t = Convert(ctx, member_access->struct_type());
            auto* instance_pointer = generated_ir[member_access->object()];
            auto* index = new (*module) IntegerConstant(
                lcc::IntegerType::Get(ctx, 64),
                member_access->member()
            );
            auto* gmp = new (*module) GetMemberPtrInst(
                struct_t,
                instance_pointer,
                index
            );
            generated_ir[expr] = gmp;
            insert(gmp);
        } break;

        case K::EnumeratorDecl: {
            auto* enumerator = as<EnumeratorDecl>(expr);
            generated_ir[expr] = new (*module) IntegerConstant(Convert(ctx, enumerator->type()), enumerator->value());
        } break;

        case K::CompoundLiteral: {
            Diag::ICE(
                "I'm blanking on how to implement compound literal IRGen, so I'm going to wait until I can talk to somebody smarter than me."
            );
        } break;

        // no-op/handled elsewhere
        case K::Module:
        case K::Sizeof:
        case K::Alignof:
        case K::Match:
        case K::Switch:
        case K::Type:
        case K::TypeDecl:
        case K::TypeAliasDecl:
        case K::FuncDecl:
        case K::TemplatedFuncDecl:
        case K::OverloadSet:
        case K::Template:
        case K::Apply:
            break;
    }
}

void IRGen::generate_function(glint::FuncDecl* f) {
    if (is<TemplatedFuncDecl>(f))
        return;

    function = as<Function>(generated_ir[f]);

    if (not IsImportedLinkage(f->linkage())) {
        // Hard to generate code for a function without a body.
        if (auto* expr = f->body()) {
            block = new (*module) lcc::Block(fmt::format("body.{}", total_block));
            update_block(block);

            // Bind param instructions.
            for (auto [i, param] : vws::enumerate(f->param_decls())) {
                auto* inst = function->param(usz(i));
                auto* alloca = new (*module) AllocaInst(inst->type(), param->location());
                auto* store = new (*module) StoreInst(inst, alloca);
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

    LCC_ASSERT(
        not mod.function("malloc").empty(),
        "malloc does not exist, shouldn't Glint sema have created it?"
    );
    LCC_ASSERT(
        not mod.function("free").empty(),
        "free does not exist, shouldn't Glint sema have created it?"
    );

    LCC_ASSERT(
        not mod.function("exit").empty(),
        "exit does not exist, shouldn't Glint sema have created it?"
    );

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
