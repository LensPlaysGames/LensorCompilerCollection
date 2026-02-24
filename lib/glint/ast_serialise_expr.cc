#include <glint/ast.hh>
#include <glint/module_description.hh>

#include <lcc/utils.hh>

#include <concepts>

namespace lcc::glint {

auto Module::serialise_expr(
    std::vector<u8>& out,
    std::vector<Expr*>& cache,
    std::vector<Expr*>& current,
    std::unordered_map<Expr*, u16> indices,
    Module::TypeSerialisationContext& type_context,
    Expr* expr
) -> lcc::u16 {
    using lcc::utils::to_bytes;

    auto found = rgs::find(cache, expr);
    if (found != cache.end())
        return u16(found - cache.begin());

    // Handle self-referencing expression (expression referenced somewhere
    // within it's own children).
    auto found_current = rgs::find(current, expr);
    if (found_current != current.end())
        return ModuleDescription::ExprIndex(indices.at(expr));

    LCC_ASSERT(
        cache.size() < 0xffff,
        "Too many expressions, cannot serialise in binary metadata format version 1"
    );
    current.push_back(expr);

    // For later confidence check
    bool tag_written = false;

    // Helper that should be called before serialising "this" expression (i.e.
    // "expr" parameter). We can't do it now due to post-order traversal
    // possibly serialising children first.
    const auto write_tag = [&]() {
        u8 tag = u8(expr->kind());
        // fmt::print("Writing expression tag at offset {}\n", out.size());
        out.push_back(tag);
        tag_written = true;
    };

    // Given a range/container, write each element to 'out' directly.
    // Mostly useful for strings.
    const auto write_each = [&]<typename T>(T v) {
        for (auto c : v) {
            out.insert(
                out.end(),
                (decltype(*out.data())) c
            );
        }
    };

    // Given an arbitrary type T, convert it to a binary representation by
    // splitting it into individual bytes, and then write those bytes to
    // 'out'.
    // NOTE: Trying to use this on strings will, in general, cause a horrible
    // time, so we've disallowed that using concepts.
    const auto write_t = [&]<typename T>
    requires (not std::derived_from<std::string, T> and not std::derived_from<std::string_view, T>)
    (T v) {
        auto arr = to_bytes(v);
        out.insert(out.end(), arr.begin(), arr.end());
    };

    const auto type_at = [&](Type* t) -> ModuleDescription::TypeIndex {
        LCC_ASSERT(t, "Cannot get index of NULL type");

        if (t->is_unknown())
            return ModuleDescription::TypeIndex(-1);

        LCC_ASSERT(
            type_context.indices.contains(t),
            "Type {} is not present within AoT-calculated indices",
            *t
        );

        auto t_index = serialise(
            type_context.out,
            type_context.cache,
            type_context.current,
            type_context.indices,
            t
        );
        LCC_ASSERT(
            t_index == type_context.indices.at(t),
            "Mismatch in AoT index calculation and actual serialisation"
        );

        return type_context.indices.at(t);
    };

    const auto recurse = [&](Expr* e) -> void {
        LCC_ASSERT(
            not tag_written,
            "You called write_tag() before traversing children; this is /not/ post-order traversal"
        );
        LCC_ASSERT(e, "Cannot serialise NULL expression");
        (void) serialise_expr(out, cache, current, indices, type_context, e);
    };

    switch (expr->kind()) {
        // NameRefExpr:
        //     type   :TypeIndex
        //     length :u16
        //     name   :u8[length]
        case Expr::Kind::NameRef: {
            auto n = as<NameRefExpr>(expr);

            write_tag();

            // type :TypeIndex
            write_t(type_at(expr->type()));

            // length :u16
            write_t(u16(n->name().size()));

            // name :u8[length]
            auto str = std::string_view(n->name());
            write_each(str);
        } break;

        // TypeExpr:
        //     type :TypeIndex
        case Expr::Kind::Type: {
            write_tag();

            // type :TypeIndex
            write_t(type_at(
                as<TypeExpr>(expr)->contained_type()
            ));
        } break;

        // IntegerLiteral
        //     type :TypeIndex
        //     value :u64
        case Expr::Kind::IntegerLiteral: {
            auto i = as<IntegerLiteral>(expr);
            write_tag();
            write_t(type_at(expr->type()));
            write_t(u64(i->value().value()));
        } break;

        // FractionalLiteral
        //     value :FixedPointNumber
        case Expr::Kind::FractionalLiteral: {
            auto i = as<FractionalLiteral>(expr);
            write_tag();
            write_t(i->value());
        } break;

        // StringLiteral
        //     length   :u32
        //     contents :u8[length]
        case Expr::Kind::StringLiteral: {
            auto s = as<StringLiteral>(expr);
            auto& str = strings.at(s->string_index());
            write_tag();
            write_t(u32(str.size()));
            write_each(std::string_view(str));
        } break;

        case Expr::Kind::EvaluatedConstant: {
            auto c = as<ConstantExpr>(expr);
            // TODO: static assert for types of constant expression
            if (c->value().is_int()) {
                auto i = c->value().as_int().value();
                // Encode as Integer Literal
                u8 tag = u8(Expr::Kind::IntegerLiteral);
                out.push_back(tag);
                tag_written = true;
                write_t(type_at(expr->type()));
                write_t(u64(i));
            } else if (c->value().is_string()) {
                auto s = c->value().as_string();
                auto& str = strings.at(s->string_index());
                // Encode as String Literal
                u8 tag = u8(Expr::Kind::StringLiteral);
                out.push_back(tag);
                tag_written = true;
                write_t(u32(str.size()));
                write_each(std::string_view(str));
            } else if (c->value().is_null()) {
                LCC_TODO("Serialise constant expression containing NULL");
            } else Diag::ICE("Unhandled constant expression value kind");
        } break;

        // ReturnExpr
        //     value :ExprIndex (-1 if not present)
        case Expr::Kind::Return: {
            auto r = as<ReturnExpr>(expr);
            if (r->value())
                recurse(r->value());

            auto value_index = ModuleDescription::bad_expr_index;
            if (r->value())
                value_index = indices.at(r->value());

            write_tag();
            write_t(value_index);
        } break;

        // TemplateExpr
        //     body :ExprIndex
        //     param_count :u8
        //     param_data  :(type : TypeIndex, name_length :u16, name :u8[name_length])[param_count]
        case Expr::Kind::Template: {
            auto t = as<TemplateExpr>(expr);
            recurse(t->body());

            write_tag();

            write_t(ModuleDescription::ExprIndex(indices.at(t->body())));

            write_t(u8(t->params().size()));

            for (auto p : t->params()) {
                write_t(type_at(p.type));
                write_t(u16(p.name.size()));
                write_each(std::string_view(p.name));
            }
        } break;

        // ApplyExpr
        //     function :ExprIndex
        //     arglists_count :u8
        //     arglists :ExprIndex[arglists_count]
        case Expr::Kind::Apply: {
            auto a = as<ApplyExpr>(expr);
            recurse(a->function());
            for (auto e : a->argument_lists())
                recurse(e);

            write_tag();
            write_t(indices.at(a->function()));
            write_t(u8(a->argument_lists().size()));
            for (auto e : a->argument_lists())
                write_t(indices.at(e));
        } break;

        // MemberAccessExpr
        //     type :TypeIndex
        //     object :ExprIndex
        //     name_length :u16
        //     name :u8[name_length]
        case Expr::Kind::MemberAccess: {
            auto m = as<MemberAccessExpr>(expr);
            recurse(m->object());

            write_tag();
            write_t(type_at(m->type()));
            write_t(indices.at(m->object()));
            write_t(u16(m->name().size()));
            write_each(m->name());
        } break;

        // CastExpr
        //     cast_kind :u8
        //     to :TypeIndex
        //     from :ExprIndex
        case Expr::Kind::Cast: {
            auto c = as<CastExpr>(expr);
            recurse(c->operand());

            write_tag();
            write_t(u8(+c->cast_kind()));
            write_t(type_at(c->type()));
            write_t(indices.at(c->operand()));
        } break;

        // SizeofExpr
        //     operand :ExprIndex
        case Expr::Kind::Sizeof: {
            auto s = as<SizeofExpr>(expr);
            recurse(s->expr());

            write_tag();
            write_t(indices.at(s->expr()));
        } break;

        // AlignofExpr
        //     operand :ExprIndex
        case Expr::Kind::Alignof: {
            auto a = as<AlignofExpr>(expr);
            recurse(a->expr());

            write_tag();
            write_t(indices.at(a->expr()));
        } break;

        // UnaryExpr
        //     operator :u8
        //     operand :ExprIndex
        case Expr::Kind::Unary: {
            auto u = as<UnaryExpr>(expr);
            recurse(u->operand());

            write_tag();
            write_t(u8(+u->op()));
            write_t(indices.at(u->operand()));
        } break;

        // Expressions that have multiple children.

        // BinaryExpr
        //     operator :u8
        //     operands :ExprIndex[2]
        case Expr::Kind::Binary: {
            auto b = as<BinaryExpr>(expr);
            recurse(b->lhs());
            recurse(b->rhs());

            write_tag();
            write_t(u8(+b->op()));
            write_t(indices.at(b->lhs()));
            write_t(indices.at(b->rhs()));
        } break;

        // IfExpr
        //     condition :ExprIndex
        //     then :ExprIndex
        //     else :ExprIndex
        case Expr::Kind::If: {
            auto i = as<IfExpr>(expr);
            recurse(i->condition());
            recurse(i->then());
            if (i->otherwise())
                recurse(i->otherwise());

            auto else_expr = ModuleDescription::bad_expr_index;
            if (i->otherwise())
                else_expr = indices.at(i->otherwise());

            write_tag();
            write_t(indices.at(i->condition()));
            write_t(indices.at(i->then()));
            write_t(else_expr);
        } break;

        // WhileExpr
        //     condition :ExprIndex
        //     body :ExprIndex
        case Expr::Kind::While: {
            auto w = as<WhileExpr>(expr);
            recurse(w->condition());
            recurse(w->body());

            write_tag();
            write_t(indices.at(w->condition()));
            write_t(indices.at(w->body()));
        } break;

        // ForExpr
        //     init :ExprIndex
        //     condition :ExprIndex
        //     increment :ExprIndex
        //     body :ExprIndex
        case Expr::Kind::For: {
            auto f = as<ForExpr>(expr);
            recurse(f->init());
            recurse(f->condition());
            recurse(f->increment());
            recurse(f->body());

            write_tag();
            write_t(indices.at(f->init()));
            write_t(indices.at(f->condition()));
            write_t(indices.at(f->increment()));
            write_t(indices.at(f->body()));
        } break;

        // Expressions that have a variable amount of children.

        // CallExpr
        //     type :TypeIndex
        //     callee :ExprIndex
        //     arg_count :u16
        //     args :ExprIndex[arg_count]
        case Expr::Kind::Call: {
            auto c = as<CallExpr>(expr);
            recurse(c->callee());
            for (auto e : c->args())
                recurse(e);

            write_tag();
            write_t(type_at(c->type()));
            write_t(indices.at(c->callee()));
            write_t(u16(c->args().size()));
            for (auto e : c->args())
                write_t(indices.at(e));
        } break;

        case Expr::Kind::IntrinsicCall: {
            expr->print(true);
            LCC_TODO("Implement serialisation of intrinsic call");
        } break;

        // CompoundLiteral
        //     type :TypeIndex
        //     count :u16
        //     expressions :ExprIndex[count]
        case Expr::Kind::CompoundLiteral: {
            auto c = as<CompoundLiteral>(expr);
            for (auto e : c->children())
                recurse(e);

            write_tag();
            write_t(type_at(c->type()));
            write_t(u16(c->children().size()));
            for (auto e : c->children())
                write_t(indices.at(e));
        } break;

        // BlockExpr
        //     count :u16
        //     expressions :ExprIndex[count]
        case Expr::Kind::Block: {
            auto b = as<BlockExpr>(expr);
            for (auto e : b->children())
                recurse(e);

            write_tag();
            write_t(u16(b->children().size()));
            for (auto e : b->children())
                write_t(indices.at(e));
        } break;

        // GroupExpr
        //     count       :u16
        //     expressions :ExprIndex[count]
        case Expr::Kind::Group: {
            auto g = as<GroupExpr>(expr);
            for (auto e : g->expressions())
                recurse(e);

            write_tag();
            write_t(u16(g->expressions().size()));
            for (auto e : g->expressions())
                write_t(indices.at(e));
        } break;

        // MatchExpr
        //     object :ExprIndex
        //     body_count :u16
        //     bodies :(name_length :u16, name :u8[name_length], body :ExprIndex)[body_count]
        case Expr::Kind::Match: {
            auto m = as<MatchExpr>(expr);
            recurse(m->object());

            for (auto e : m->bodies())
                recurse(e);

            write_tag();

            write_t(indices.at(m->object()));

            write_t(u16(m->bodies().size()));

            for (auto [name, body] : vws::zip(m->names(), m->bodies())) {
                write_t(u16(name.length()));
                write_each(name);
                write_t(indices.at(body));
            }

        } break;

        // SwitchExpr
        //     object :ExprIndex
        //     body_count :u16
        //     bodies :(name_length :u16, name :u8[name_length], body :ExprIndex)[body_count]
        case Expr::Kind::Switch: {
            auto sw = as<SwitchExpr>(expr);
            recurse(sw->object());

            for (auto e : sw->bodies())
                recurse(e);

            write_tag();

            write_t(indices.at(sw->object()));

            write_t(u16(sw->bodies().size()));

            for (auto [name, body] : vws::zip(sw->names(), sw->bodies())) {
                write_t(u16(name.length()));
                write_each(name);
                write_t(indices.at(body));
            }

        } break;

        // VarDecl
        //     type :TypeIndex
        //     name_length :u16
        //     name :u8[name_length]
        //     linkage :u8
        //     init :ExprIndex
        case Expr::Kind::VarDecl: {
            auto v = as<VarDecl>(expr);
            if (v->init())
                recurse(v->init());

            write_tag();

            write_t(type_at(v->type()));

            write_t(u16(v->name().size()));
            write_each(v->name());

            write_t(u8(+v->linkage()));

            if (v->init())
                write_t(indices.at(v->init()));
            else write_t(ModuleDescription::bad_expr_index);
        } break;

        case Expr::Kind::Module:
        case Expr::Kind::EnumeratorDecl:
        case Expr::Kind::FuncDecl:
        case Expr::Kind::TemplatedFuncDecl:
        case Expr::Kind::OverloadSet:
        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::TypeDecl:
            expr->print(true);
            LCC_TODO("Implement serialisation of preceding expression AST\n");
    }

    LCC_ASSERT(
        tag_written,
        "You forgot to call write_tag() when implementing serialisation of a new expression, most likely"
    );

    std::erase(current, expr);

    auto expr_index = ModuleDescription::ExprIndex(cache.size());

    LCC_ASSERT(
        indices.at(expr) == expr_index,
        "Mismatch in AoT index calculation and actual serialisation"
    );

    cache.emplace_back(expr);

    return expr_index;
}

} // namespace lcc::glint
