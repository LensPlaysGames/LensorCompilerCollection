#include "lcc/utils.hh"
#include <concepts>
#include <glint/ast.hh>
#include <glint/module_description.hh>

namespace lcc::glint {

auto Module::serialise_expr(
    std::vector<u8>& out,
    std::vector<Expr*>& cache,
    std::vector<Expr*>& current,
    std::unordered_map<Expr*, u16> indices,
    std::unordered_map<Type*, u16> type_indices,
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
            type_indices.contains(t),
            "Type {} is not present within AoT-calculated indices",
            *t
        );
        return type_indices.at(t);
    };

    const auto recurse = [&](Expr* e) -> void {
        LCC_ASSERT(
            not tag_written,
            "You called write_tag() before traversing children; this is /not/ post-order traversal"
        );
        LCC_ASSERT(e, "Cannot serialise NULL expression");
        (void) serialise_expr(out, cache, current, indices, type_indices, e);
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
            write_t(ModuleDescription::TypeIndex(type_at(expr->type())));

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
            write_t(ModuleDescription::TypeIndex(type_at(expr->type())));
        } break;

        // IntegerLiteral
        //     type :TypeIndex
        //     value :u64
        case Expr::Kind::IntegerLiteral: {
            auto i = as<IntegerLiteral>(expr);
            write_tag();
            write_t(ModuleDescription::TypeIndex(type_at(expr->type())));
            write_t(u64(i->value().value()));
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
            // auto c = as<ConstantExpr>(expr);
            // TODO: static assert for types of constant expression
            LCC_TODO("Serialise constant expression");
        } break;

        // ReturnExpr
        //     value :ExprIndex (-1 if not present)
        case Expr::Kind::Return: {
            auto r = as<ReturnExpr>(expr);
            if (r->value())
                recurse(r->value());

            write_tag();
            write_t(ModuleDescription::ExprIndex(indices.at(r->value())));
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

        case Expr::Kind::Apply: {
            auto a = as<ApplyExpr>(expr);
            recurse(a->function());
            for (auto e : a->argument_lists())
                recurse(e);
        } break;

        case Expr::Kind::MemberAccess: {
            auto m = as<MemberAccessExpr>(expr);
            recurse(m->object());
        } break;

        case Expr::Kind::Cast: {
            auto c = as<CastExpr>(expr);
            recurse(c->operand());
        } break;

        case Expr::Kind::Sizeof: {
            auto s = as<SizeofExpr>(expr);
            recurse(s->expr());
        } break;

        case Expr::Kind::Alignof: {
            auto a = as<AlignofExpr>(expr);
            recurse(a->expr());
        } break;

        case Expr::Kind::Unary: {
            auto u = as<UnaryExpr>(expr);
            recurse(u->operand());
        } break;

        // Expressions that have multiple children.
        case Expr::Kind::Binary: {
            auto b = as<BinaryExpr>(expr);
            recurse(b->lhs());
            recurse(b->rhs());
        } break;

        case Expr::Kind::If: {
            auto i = as<IfExpr>(expr);
            recurse(i->condition());
            recurse(i->then());
            if (i->otherwise())
                recurse(i->otherwise());
        } break;

        case Expr::Kind::While: {
            auto w = as<WhileExpr>(expr);
            recurse(w->condition());
            recurse(w->body());
        } break;

        case Expr::Kind::For: {
            auto f = as<ForExpr>(expr);
            recurse(f->init());
            recurse(f->condition());
            recurse(f->increment());
            recurse(f->body());
        } break;

        // Expressions that have a variable amount of children.
        case Expr::Kind::Call: {
            auto c = as<CallExpr>(expr);
            recurse(c->callee());
            for (auto e : c->args())
                recurse(e);
        } break;

        case Expr::Kind::IntrinsicCall: {
            expr->print(true);
            LCC_TODO("Implement serialisation of intrinsic call");
        } break;

        case Expr::Kind::CompoundLiteral: {
            auto c = as<CompoundLiteral>(expr);
            for (auto e : c->children())
                recurse(e);
        } break;

        case Expr::Kind::Block: {
            auto b = as<BlockExpr>(expr);
            for (auto e : b->children())
                recurse(e);
        } break;

        case Expr::Kind::Group: {
            auto g = as<GroupExpr>(expr);
            for (auto e : g->expressions())
                recurse(e);
        } break;

        case Expr::Kind::Match: {
            auto m = as<MatchExpr>(expr);
            recurse(m->object());

            for (auto e : m->bodies())
                recurse(e);

        } break;

        case Expr::Kind::Module:
        case Expr::Kind::EnumeratorDecl:
        case Expr::Kind::FuncDecl:
        case Expr::Kind::TemplatedFuncDecl:
        case Expr::Kind::OverloadSet:
        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::TypeDecl:
        case Expr::Kind::VarDecl:
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
