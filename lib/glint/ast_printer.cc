// This file implements the print-out used by `--ast`, more or less.

#include <lcc/utils/ast_printer.hh>
#include <lcc/utils/rtti.hh>

#include <glint/ast.hh>

#include <string_view>
#include <unordered_set>

namespace {
using lcc::as;
using lcc::cast;
using lcc::is;

struct ASTPrinter : lcc::utils::ASTPrinter<ASTPrinter, lcc::glint::Expr, lcc::glint::Type> {
    // NOTE: Can use to override name_colour from ast_printer.hh for Glint names.
    // static constexpr lcc::utils::Colour name_colour{Green};

    // Used to highlight key details, like binary/unary operators, integer literal values, etc.
    static constexpr lcc::utils::Colour key_detail_colour{Red};

    std::unordered_set<const lcc::glint::FuncDecl*> printed_functions{};
    bool print_children_of_children = true;

    void PrintLValue(const lcc::glint::Expr* e) {
        if (e->is_lvalue()) out += fmt::format(" {}lvalue", C(ASTPrinter::base_colour));
    };

    void PrintBasicGlintNode(std::string_view name, const lcc::glint::Expr* node, lcc::glint::Type* t) {
        PrintBasicNode(name, node, t, false);
        PrintLValue(node);
        out += '\n';
    };

    /// Print the header (name + location + type) of a node.
    void PrintHeader(const lcc::glint::Expr* e) {
        using K = lcc::glint::Expr::Kind;
        switch (e->kind()) {
            case K::FuncDecl: {
                auto* f = as<lcc::glint::FuncDecl>(e);
                PrintLinkage(f->linkage());
                PrintBasicHeader("FuncDecl", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(name_colour),
                    f->name(),
                    f->type()->string(use_colour)
                );
                return;
            }

            case K::TemplatedFuncDecl: {
                auto* t = as<lcc::glint::TemplatedFuncDecl>(e);
                PrintLinkage(t->linkage());
                PrintBasicHeader("TemplatedFuncDecl", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(name_colour),
                    t->name(),
                    t->type()->string(use_colour)
                );
                return;
            }

            case K::VarDecl: {
                auto* v = as<lcc::glint::VarDecl>(e);
                PrintLinkage(v->linkage());
                PrintBasicHeader("VarDecl", e);
                out += fmt::format(
                    " {}{} {}",
                    C(name_colour),
                    v->name(),
                    v->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::EnumeratorDecl: {
                auto* v = as<lcc::glint::EnumeratorDecl>(e);
                PrintBasicHeader("EnumeratorDecl", e);
                out += fmt::format(
                    " {}{} {}{}\n",
                    C(name_colour),
                    v->name(),
                    C(key_detail_colour),
                    v->ok() ? v->value().str() : "?"
                );
                return;
            }
            case K::Binary: {
                const auto* b = as<lcc::glint::BinaryExpr>(e);
                PrintBasicHeader("BinaryExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(key_detail_colour),
                    lcc::glint::ToString(b->op()),
                    b->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::Unary: {
                auto* u = as<lcc::glint::UnaryExpr>(e);
                PrintBasicHeader("UnaryExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(key_detail_colour),
                    lcc::glint::ToString(u->op()),
                    u->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::Apply: {
                // auto* a = as<lcc::glint::ApplyExpr>(e);
                PrintBasicHeader("ApplyExpr", e);
                out += '\n';
                return;
            }

            case K::IntegerLiteral: {
                auto* i = as<lcc::glint::IntegerLiteral>(e);
                PrintBasicHeader("IntegerLiteral", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(key_detail_colour),
                    i->value(),
                    i->type()->string(use_colour)
                );
                return;
            }

            case K::FractionalLiteral: {
                auto* f = as<lcc::glint::FractionalLiteral>(e);
                PrintBasicHeader("FractionalLiteral", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(key_detail_colour),
                    f->value(),
                    f->type()->string(use_colour)
                );
                return;
            }

            case K::NameRef: {
                auto* n = as<lcc::glint::NameRefExpr>(e);
                PrintBasicHeader("NameRefExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(name_colour),
                    n->name(),
                    n->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::Cast: {
                auto* c = as<lcc::glint::CastExpr>(e);
                PrintBasicHeader("CastExpr", e);
                switch (c->cast_kind()) {
                    case lcc::glint::CastKind::SoftCast: out += fmt::format(" {}Soft ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::HardCast: out += fmt::format(" {}Hard ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::ImplicitCast: out += fmt::format(" {}Implicit ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::LValueToRValueConv: out += fmt::format(" {}LValueToRValue ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::LValueToReference: out += fmt::format(" {}LValueToReference ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::ReferenceToLValue: out += fmt::format(" {}ReferenceToLValue ", C(key_detail_colour)); break;
                }
                out += e->type()->string(use_colour);
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::If: {
                PrintBasicHeader("IfExpr", e);
                if (not e->type()->is_void())
                    out += fmt::format(" {}", e->type()->string(use_colour));
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::Match: {
                PrintBasicHeader("MatchExpr", e);
                const auto* m = as<lcc::glint::MatchExpr>(e);
                out += fmt::format(
                    " on {}",
                    m->object()->type()->string(use_colour)
                );
                out += '\n';
                return;
            }

            case K::Switch: {
                PrintBasicHeader("SwitchExpr", e);
                const auto* m = as<lcc::glint::SwitchExpr>(e);
                out += fmt::format(
                    " on {}",
                    m->object()->type()->string(use_colour)
                );
                out += '\n';
                return;
            }

            case K::Template: {
                PrintBasicHeader("TemplateExpr", e);
                const auto* t = as<lcc::glint::TemplateExpr>(e);
                out += fmt::format("{}(", C(Reset));
                out += fmt::format(
                    "{})",
                    fmt::join(
                        lcc::vws::transform(t->params(), [](const auto& p) {
                            return fmt::format("{}:{}", p.name, *p.type);
                        }),
                        ", "
                    )
                );
                out += '\n';
                return;
            }

            case K::EvaluatedConstant: {
                auto c = as<lcc::glint::ConstantExpr>(e);
                PrintBasicNode("ConstantExpr", e, e->type(), false);
                if (c->value().is_null()) out += " NULL";
                else if (c->value().is_int()) out += fmt::format(" INT {}", c->value().as_int());
                else if (c->value().is_string()) out += fmt::format(" STR.IDX {}", c->value().as_string()->string_index());
                out += '\n';
                return;
            }

            case K::OverloadSet: PrintBasicGlintNode("OverloadSet", e, e->type()); return;
            case K::Type:
                PrintBasicGlintNode("TypeExpr", e, as<lcc::glint::TypeExpr>(e)->contained_type());
                return;
            case K::TypeDecl: PrintBasicGlintNode("TypeDecl", e, e->type()); return;
            case K::TypeAliasDecl: PrintBasicGlintNode("TypeAliasDecl", e, e->type()); return;
            case K::StringLiteral: PrintBasicGlintNode("StringLiteral", e, e->type()); return;
            case K::CompoundLiteral: PrintBasicGlintNode("CompoundLiteral", e, e->type()); return;
            case K::MemberAccess:
                PrintBasicHeader("MemberAccessExpr", e);

                // Member identifier
                out += fmt::format(
                    " {}.{}",
                    C(name_colour),
                    as<lcc::glint::MemberAccessExpr>(e)->name()
                );

                // Type + lvalue
                out += fmt::format(" {}", e->type()->string(use_colour));
                PrintLValue(e);

                out += '\n';
                return;
            case K::Group: PrintBasicGlintNode("GroupExpr", e, e->type()); return;
            case K::While: PrintBasicGlintNode("WhileExpr", e, nullptr); return;
            case K::For: PrintBasicGlintNode("ForExpr", e, nullptr); return;
            case K::Block: PrintBasicGlintNode("BlockExpr", e, e->type()); return;
            case K::Return: PrintBasicGlintNode("ReturnExpr", e, nullptr); return;
            case K::Continue: PrintBasicGlintNode("ContinueExpr", e, nullptr); return;
            case K::Break: PrintBasicGlintNode("BreakExpr", e, nullptr); return;
            case K::Call: PrintBasicGlintNode("CallExpr", e, e->type()); return;
            case K::IntrinsicCall: PrintBasicGlintNode("IntrinsicCallExpr", e, e->type()); return;
            case K::Module: PrintBasicGlintNode("ModuleExpr", e, nullptr); return;
            case K::Sizeof: PrintBasicGlintNode("SizeofExpr", e, lcc::glint::Type::Int); return;
            case K::Alignof: PrintBasicGlintNode("AlignofExpr", e, lcc::glint::Type::Int); return;
        }

        PrintBasicGlintNode(R"(<???>)", e, e->type());
    }

    void PrintNodeChildren(const lcc::glint::Expr* e, std::string leading_text = "") {
        if (not print_children_of_children) return;

        // Only print function bodies at the top level.
        if (e->kind() == lcc::glint::Expr::Kind::FuncDecl) return;

        if (auto* n = cast<lcc::glint::NameRefExpr>(e)) {
            // Don't print children of children, but restore the setting to whatever
            // it was before we touched it, after we don't need it anymore.
            auto old_print_children_of_children = print_children_of_children;
            print_children_of_children = false;
            PrintChildren(n->children(), leading_text);
            print_children_of_children = old_print_children_of_children;
            return;
        }

        /// Print the children of a node.
        PrintChildren(e->children(), leading_text);

        return;
    }

    /// Print a top-level node.
    void PrintTopLevelNode(const lcc::glint::Expr* e) {
        PrintHeader(e);
        if (const auto* f = cast<lcc::glint::FuncDecl>(e)) {
            printed_functions.insert(f);
            if (auto* body = const_cast<lcc::glint::FuncDecl*>(f)->body()) {
                if (auto* block = cast<lcc::glint::BlockExpr>(body)) {
                    PrintChildren(block->children(), "");
                } else {
                    lcc::glint::Expr* children[] = {const_cast<lcc::glint::FuncDecl*>(f)->body()};
                    PrintChildren(children, "");
                }
            }
        } else {
            PrintNodeChildren(e);
        }
    }

    /// Print a node.
    void operator()(const lcc::glint::Expr* e, std::string leading_text = "") {
        PrintHeader(e);
        PrintNodeChildren(e, std::move(leading_text));
    }

    void print(const lcc::glint::Module* mod) {
        LCC_ASSERT(mod, "Cannot print NULL module");

        if (mod->imports().size()) {
            out += "Imports:\n";
            for (auto import_ref : mod->imports()) {
                out += fmt::format("- {}", import_ref.name);
                if (import_ref.aliased_name.size())
                    out += fmt::format(" aliased as {}", import_ref.aliased_name);
                else out += " directly";
                out += '\n';
            }
        }

        if (mod->exports().size()) {
            out += "Exports:\n";
            for (auto exported_decl : mod->exports())
                out += fmt::format(
                    "- {} : {}\n",
                    exported_decl->name(),
                    *exported_decl->type()
                );
        }

        printed_functions.insert(mod->top_level_function());
        PrintTopLevelNode(mod->top_level_function());

        for (auto* f : mod->functions()) {
            if (
                not printed_functions.contains(f)
                and f->name() != "format"
            ) PrintTopLevelNode(f);
        }
    }
};
} // namespace

auto lcc::glint::Module::string(bool use_colour) const -> std::string {
    auto a = ASTPrinter{use_colour};
    a.print(this);
    auto result = std::move(a.out);
    return result;
}

void lcc::glint::Module::print(bool use_colour) {
    ASTPrinter{use_colour}.print(this);
}

auto lcc::glint::Expr::string(bool use_colour) const -> std::string {
    auto a = ASTPrinter{use_colour};
    a(this);
    auto result = std::move(a.out);
    return result;
}

void lcc::glint::Expr::print(bool use_colour) const {
    ASTPrinter{use_colour}(this);
}

auto lcc::glint::Type::string(bool use_colours) const -> std::string {
    static constexpr lcc::utils::Colour type_colour{lcc::utils::Colour::Cyan};

    lcc::utils::Colours C{use_colours};
    using enum lcc::utils::Colour;

    switch (kind()) {
        case Kind::Named:
            return fmt::format(
                "{}NamedType {}{}{}",
                C(White),
                C(type_colour),
                as<NamedType>(this)->name(),
                C(Reset)
            );

        case Kind::Pointer: {
            /// If the element type of this pointer contains an array or
            /// function type, we need to use parentheses here to preserve
            /// precedence.
            bool has_arr_or_func = false;
            auto* el = elem();
            for (;;) {
                switch (el->kind()) {
                    default: break;

                    case Kind::Pointer:
                    case Kind::Reference:
                        el = el->elem();
                        continue;

                    case Kind::Array:
                    case Kind::Function:
                        has_arr_or_func = true;
                        break;
                }
                break;
            }

            return fmt::format(
                "{}{}{}{}{}.ptr{}{}{}",
                C(ASTPrinter::base_colour),
                has_arr_or_func ? "(" : "",
                C(type_colour),
                as<PointerType>(this)->element_type()->string(use_colours),
                C(type_colour),
                C(ASTPrinter::base_colour),
                has_arr_or_func ? ")" : "",
                C(Reset)
            );
        }
        case Kind::Reference: {
            bool has_func = false;
            auto* el = elem();
            for (;;) {
                switch (el->kind()) {
                    default: break;

                    case Kind::Pointer:
                    case Kind::Reference:
                        el = el->elem();
                        continue;

                    case Kind::Function:
                        has_func = true;
                        break;
                }
                break;
            }

            return fmt::format(
                "{}{}{}{}{}.ref{}{}{}",
                C(ASTPrinter::base_colour),
                has_func ? "(" : "",
                C(type_colour),
                as<ReferenceType>(this)->element_type()->string(use_colours),
                C(type_colour),
                C(ASTPrinter::base_colour),
                has_func ? ")" : "",
                C(Reset)
            );
        }

        case Kind::Integer: {
            auto* i = as<IntegerType>(this);
            return fmt::format("{}{}{}{}", C(type_colour), i->is_signed() ? "s" : "u", i->bit_width(), C(Reset));
        }

        case Kind::Struct: {
            auto* decl = as<StructType>(this)->decl();
            if (not decl or decl->name().empty()) {
                auto members = as<StructType>(this)->members();
                std::vector<std::string> members_strings{};
                for (auto m : members) {
                    members_strings.emplace_back(
                        fmt::format("{}", *m.type)
                    );
                }
                return fmt::format(
                    "{}struct {}{{{}}}{}",
                    C(type_colour),
                    C(Reset),
                    fmt::join(members_strings, ", "),
                    C(Reset)
                );
            }
            return fmt::format(
                "{}struct {}{}",
                C(type_colour),
                decl->name(),
                C(Reset)
            );
        }

        case Kind::TemplatedStruct: {
            auto* decl = as<TemplatedStructType>(this)->decl();
            if (not decl or decl->name().empty()) {
                auto members = as<TemplatedStructType>(this)->members();
                std::vector<std::string> members_strings{};
                for (auto m : members) {
                    members_strings.emplace_back(
                        fmt::format("{}", *m.type)
                    );
                }
                return fmt::format(
                    "{}templated_struct {}{{{}}}{}",
                    C(type_colour),
                    C(Reset),
                    fmt::join(members_strings, ", "),
                    C(Reset)
                );
            }
            return fmt::format(
                "{}templated_struct {}{}",
                C(type_colour),
                decl->name(),
                C(Reset)
            );
        }

        case Kind::Sum: {
            auto* decl = as<SumType>(this)->decl();
            return fmt::format(
                "{}sum {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Union: {
            auto* decl = as<UnionType>(this)->decl();
            return fmt::format(
                "{}union {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Enum: {
            auto* decl = as<EnumType>(this)->decl();
            return fmt::format(
                "{}enum {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::DynamicArray: {
            auto* arr = as<DynamicArrayType>(this);
            return fmt::format(
                "{}[{}{}]{}",
                C(type_colour),
                arr->element_type()->string(use_colours),
                C(type_colour),
                C(Reset)
            );
        }

        case Kind::ArrayView: {
            auto* arr = as<ArrayViewType>(this);
            return fmt::format(
                "{}[{}{} view]{}",
                C(type_colour),
                arr->element_type()->string(use_colours),
                C(type_colour),
                C(Reset)
            );
        }

        case Kind::Array: {
            auto* arr = as<ArrayType>(this);
            LCC_ASSERT(arr->size(), "ArrayType has NULL size expression");
            if (auto* sz = cast<ConstantExpr>(arr->size())) {
                return fmt::format(
                    "{}[{} {}{}{}]{}",
                    C(type_colour),
                    arr->element_type()->string(use_colours),
                    C(ASTPrinter::name_colour),
                    sz->value().as_int(),
                    C(type_colour),
                    C(Reset)
                );
            }
            return fmt::format(
                "{}[{}{}]{}",
                C(type_colour),
                arr->element_type()->string(use_colours),
                C(type_colour),
                C(Reset)
            );
        }

        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return fmt::format("{}bool{}", C(type_colour), C(Reset));
                case K::Byte: return fmt::format("{}byte{}", C(type_colour), C(Reset));
                case K::Int: return fmt::format("{}int{}", C(type_colour), C(Reset));
                case K::UInt: return fmt::format("{}uint{}", C(type_colour), C(Reset));
                case K::Float: return fmt::format("{}float{}", C(type_colour), C(Reset));
                case K::Unknown: return fmt::format("{}?{}", C(type_colour), C(Reset));
                case K::Void: return fmt::format("{}void{}", C(type_colour), C(Reset));
                case K::OverloadSet: return fmt::format("{}<overload set>{}", C(type_colour), C(Reset));
            }
            LCC_UNREACHABLE();

        case Kind::FFIType:
            switch (as<FFIType>(this)->ffi_kind()) {
                using K = FFIType::FFIKind;
                case K::CChar: return fmt::format("{}__c_char{}", C(type_colour), C(Reset));
                case K::CSChar: return fmt::format("{}__c_schar{}", C(type_colour), C(Reset));
                case K::CUChar: return fmt::format("{}__c_uchar{}", C(type_colour), C(Reset));
                case K::CShort: return fmt::format("{}__c_short{}", C(type_colour), C(Reset));
                case K::CUShort: return fmt::format("{}__c_ushort{}", C(type_colour), C(Reset));
                case K::CInt: return fmt::format("{}__c_int{}", C(type_colour), C(Reset));
                case K::CUInt: return fmt::format("{}__c_uint{}", C(type_colour), C(Reset));
                case K::CLong: return fmt::format("{}__c_long{}", C(type_colour), C(Reset));
                case K::CULong: return fmt::format("{}__c_ulong{}", C(type_colour), C(Reset));
                case K::CLongLong: return fmt::format("{}__c_longlong{}", C(type_colour), C(Reset));
                case K::CULongLong: return fmt::format("{}__c_ulonglong{}", C(type_colour), C(Reset));
            }
            LCC_UNREACHABLE();

        case Kind::Type: {
            return fmt::format("{}type{}", C(type_colour), C(Reset));
        }

        case Kind::Typeof: {
            auto t_typeof = as<TypeofType>(this);
            if (t_typeof->expression() and t_typeof->expression()->type()) {
                return fmt::format(
                    "{}typeof {}{}",
                    C(type_colour),
                    *t_typeof->expression()->type(),
                    C(Reset)
                );
            }
            return fmt::format("{}typeof ?{}", C(type_colour), C(Reset));
        }

        case Kind::Function: {
            const auto* f = as<FuncType>(this);
            std::string out = fmt::format("{}{}(", f->return_type()->string(use_colours), C(ASTPrinter::base_colour));
            for (const auto& arg : f->params()) {
                if (&arg != &f->params().front()) out += fmt::format("{}, ", C(ASTPrinter::base_colour));
                out += fmt::format("{}{}{}", C(ASTPrinter::name_colour), arg.name, C(ASTPrinter::base_colour));
                if (not arg.name.empty()) out += " : ";
                else out += ":";
                out += arg.type->string(use_colours);
            }
            out += fmt::format("{}){}", C(ASTPrinter::base_colour), C(Reset));
            return out;
        }
    }

    LCC_UNREACHABLE();
}
