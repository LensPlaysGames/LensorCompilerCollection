#include <laye/ast.hh>
#include <laye/parser.hh>
#include <lcc/utils/ast_printer.hh>
#include <lcc/utils/rtti.hh>

namespace layec = lcc::laye;

void* layec::Scope::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax scopes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->scopes.push_back(static_cast<Scope*>(ptr));
    return ptr;
}

void* layec::Statement::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax nodes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->statements.push_back(static_cast<Statement*>(ptr));
    return ptr;
}

void* layec::Expr::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax nodes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->exprs.push_back(static_cast<Expr*>(ptr));
    return ptr;
}

auto layec::Scope::declare(
    Parser* parser,
    std::string name,
    Decl* decl
) -> Result<Decl*> {
    /// If the symbol already exists, then this is an error, unless
    /// that symbol is a function declaration, and this is also a
    /// function declaration.
    if (
        auto it = symbols.find(name);
        it != symbols.end() and
        not is<FunctionDecl>(it->second) and
        not is<FunctionDecl>(decl)
    ) return Diag::Error(parser->context, decl->location(), "Redeclaration of '{}'", name);

    /// Otherwise, add the symbol.
    symbols.emplace(std::move(name), decl);
    return decl;
}

std::string layec::ToString(layec::TokenKind kind) {
    using TokenKind = layec::TokenKind;
    switch (kind) {
        case TokenKind::Eof: return "eof";
        case TokenKind::Tilde: return "~";
        case TokenKind::Bang: return "!";
        case TokenKind::Percent: return "%";
        case TokenKind::Ampersand: return "&";
        case TokenKind::Star: return "*";
        case TokenKind::OpenParen: return "(";
        case TokenKind::CloseParen: return ")";
        case TokenKind::Minus: return "-";
        case TokenKind::Equal: return "=";
        case TokenKind::Plus: return "+";
        case TokenKind::OpenBracket: return "[";
        case TokenKind::CloseBracket: return "]";
        case TokenKind::OpenBrace: return "{";
        case TokenKind::CloseBrace: return "}";
        case TokenKind::Pipe: return "|";
        case TokenKind::SemiColon: return ";";
        case TokenKind::Colon: return ":";
        case TokenKind::Comma: return ",";
        case TokenKind::Less: return "<";
        case TokenKind::Greater: return ">";
        case TokenKind::Dot: return ".";
        case TokenKind::Slash: return "/";
        case TokenKind::Question: return "?";
        case TokenKind::Ident: return "<ident>";
        case TokenKind::LitInt: return "<literal int>";
        case TokenKind::LitFloat: return "<literal float>";
        case TokenKind::LitString: return "<literal string>";
        case TokenKind::LitRune: return "<literal rune>";
        case TokenKind::LessLess: return "<<";
        case TokenKind::GreaterGreater: return ">>";
        case TokenKind::EqualEqual: return "==";
        case TokenKind::BangEqual: return "!=";
        case TokenKind::PlusEqual: return "+=";
        case TokenKind::MinusEqual: return "-=";
        case TokenKind::SlashEqual: return "/=";
        case TokenKind::StarEqual: return "*=";
        case TokenKind::PercentEqual: return "%=";
        case TokenKind::LessEqual: return "<=";
        case TokenKind::GreaterEqual: return ">=";
        case TokenKind::AmpersandEqual: return "&=";
        case TokenKind::PipeEqual: return "|=";
        case TokenKind::TildeEqual: return "~=";
        case TokenKind::LessLessEqual: return "<<=";
        case TokenKind::GreaterGreaterEqual: return ">>=";
        case TokenKind::EqualGreater: return "=>";
        case TokenKind::ColonColon: return "::";
        case TokenKind::Bool: return "bool";
        case TokenKind::Int: return "int";
        case TokenKind::UInt: return "uint";
        case TokenKind::Float: return "float";
        // case TokenKind::Context: return "context";
        case TokenKind::True: return "true";
        case TokenKind::False: return "false";
        case TokenKind::Nil: return "nil";
        case TokenKind::Global: return "global";
        case TokenKind::If: return "if";
        case TokenKind::Else: return "else";
        case TokenKind::For: return "for";
        case TokenKind::Do: return "do";
        case TokenKind::Switch: return "switch";
        case TokenKind::Case: return "case";
        case TokenKind::Default: return "default";
        case TokenKind::Return: return "return";
        case TokenKind::Break: return "break";
        case TokenKind::Continue: return "continue";
        case TokenKind::Goto: return "goto";
        case TokenKind::Struct: return "struct";
        case TokenKind::Variant: return "variant";
        case TokenKind::Enum: return "enum";
        case TokenKind::Alias: return "alias";
        case TokenKind::Test: return "test";
        case TokenKind::Import: return "import";
        case TokenKind::Export: return "export";
        case TokenKind::From: return "from";
        case TokenKind::As: return "as";
        case TokenKind::Operator: return "operator";
        case TokenKind::Readonly: return "readonly";
        case TokenKind::Writeonly: return "writeonly";
        case TokenKind::New: return "new";
        case TokenKind::Delete: return "delete";
        case TokenKind::Cast: return "cast";
        case TokenKind::Try: return "try";
        case TokenKind::Catch: return "catch";
        // case TokenKind::Discard: return "discard";
        case TokenKind::Sizeof: return "sizeof";
        case TokenKind::Alignof: return "alignof";
        case TokenKind::Offsetof: return "offsetof";
        case TokenKind::Not: return "not";
        case TokenKind::And: return "and";
        case TokenKind::Or: return "or";
        case TokenKind::Xor: return "xor";
        case TokenKind::Varargs: return "varargs";
        case TokenKind::Const: return "const";
        case TokenKind::Foreign: return "foreign";
        case TokenKind::Inline: return "inline";
        case TokenKind::Callconv: return "callconv";
        // case TokenKind::Impure: return "impure";
        case TokenKind::Void: return "void";
        case TokenKind::Var: return "var";
        case TokenKind::Noreturn: return "noreturn";
        case TokenKind::Rawptr: return "rawptr";
        case TokenKind::String: return "string";
        case TokenKind::CChar: return "c_char";
        case TokenKind::CSChar: return "c_schar";
        case TokenKind::CUChar: return "c_uchar";
        case TokenKind::CString: return "c_string";
        case TokenKind::CShort: return "c_short";
        case TokenKind::CUShort: return "c_ushort";
        case TokenKind::CInt: return "c_int";
        case TokenKind::CUInt: return "c_uint";
        case TokenKind::CLong: return "c_long";
        case TokenKind::CULong: return "c_ulong";
        case TokenKind::CLongLong: return "c_longlong";
        case TokenKind::CULongLong: return "c_ulonglong";
        case TokenKind::CSizeT: return "c_sizet";
        case TokenKind::CISizeT: return "c_isizet";
        case TokenKind::CPtrDiffT: return "c_ptrdifft";
        case TokenKind::CFloat: return "c_float";
        case TokenKind::CDouble: return "c_double";
        case TokenKind::CLongDouble: return "c_longdouble";
        case TokenKind::CBool: return "c_bool";
        default: return "<unknown>";
    }
}

std::string layec::ToString(layec::OperatorKind kind) {
    switch (kind) {
        case OperatorKind::Invalid: return "<invalid>";
        case OperatorKind::Add: return "+";
        case OperatorKind::Sub: return "-";
        case OperatorKind::Mul: return "*";
        case OperatorKind::Div: return "/";
        case OperatorKind::Mod: return "%";
        case OperatorKind::Greater: return ">";
        case OperatorKind::Less: return "<";
        case OperatorKind::Equal: return "=";
        case OperatorKind::NotEqual: return "!=";
        case OperatorKind::Compl: return "~";
        case OperatorKind::And: return "&";
        case OperatorKind::Or: return "|";
        case OperatorKind::Xor: return "~";
        case OperatorKind::Lsh: return "<<";
        case OperatorKind::Rsh: return ">>";
        case OperatorKind::AddEqual: return "+=";
        case OperatorKind::SubEqual: return "-=";
        case OperatorKind::DivEqual: return "/=";
        case OperatorKind::MulEqual: return "*/";
        case OperatorKind::ModEqual: return "%=";
        case OperatorKind::LessEqual: return "<=";
        case OperatorKind::GreaterEqual: return ">=";
        case OperatorKind::AndEqual: return "&=";
        case OperatorKind::OrEqual: return "|=";
        case OperatorKind::XorEqual: return "~=";
        case OperatorKind::LshEqual: return "<<=";
        case OperatorKind::RshEqual: return ">>=";
        case OperatorKind::Index: return "[]";
        default: return "<unknown>";
    }
}

auto layec::Type::string(bool use_colours) const -> std::string {
    lcc::utils::Colours C{use_colours};
    using enum lcc::utils::Colour;

    switch (kind()) {
        default: LCC_ASSERT(false, "unhandled type in type->string()");

        case Kind::TypeInfer: return fmt::format("{}var", C(Cyan));
        case Kind::TypeNilable: return fmt::format("{}?", as<NilableType>(this)->elem_type()->string(use_colours));
        case Kind::TypeErrUnion: {
            auto e = as<ErrUnionType>(this);
            return fmt::format("{}!{}", e->error_name(), e->value_type()->string(use_colours));
        }

        case Kind::TypeLookupName: return fmt::format("{}{}", C(White), as<NameType>(this)->name());
        case Kind::TypeLookupPath: {
            std::string path{};
            auto path_names = as<PathType>(this)->names();
            for (lcc::usz i = 0; i < path_names.size(); i++) {
                if (i > 0) path += fmt::format("{}::", C(White));
                path += fmt::format("{}{}", C(White), path);
            }
            return path;
        }

        case Kind::TypeArray: return fmt::format("{}[{} array]", as<ArrayType>(this)->elem_type()->string(use_colours), as<ArrayType>(this)->rank_lengths().size());
        case Kind::TypeSlice: return fmt::format("{}[]", as<SliceType>(this)->elem_type()->string(use_colours));
        case Kind::TypePointer: return fmt::format("{}*", as<PointerType>(this)->elem_type()->string(use_colours));
        case Kind::TypeBuffer: return fmt::format("{}[*]", as<BufferType>(this)->elem_type()->string(use_colours));

        case Kind::TypeFunc: {
            auto f = as<FuncType>(this);
            std::string params_string{};
            auto params = f->param_types();
            for (lcc::usz i = 0; i < params.size(); i++) {
                if (i > 0) params_string += fmt::format("{}, ", C(White));
                params_string += fmt::format("{}", params[i]->string(use_colours));
            }
            return fmt::format("{}({})", f->return_type()->string(use_colours), params_string);
        }

        case Kind::TypeNoreturn: return fmt::format("{}noreturn", C(Cyan));
        case Kind::TypeRawptr: return fmt::format("{}rawptr", C(Cyan));
        case Kind::TypeVoid: return fmt::format("{}void", C(Cyan));
        case Kind::TypeString: {
            auto access = as<StringType>(this)->access();
            return fmt::format(
                "{}{}string",
                C(Cyan),
                access == TypeAccess::ReadOnly ? "readonly " : (access == TypeAccess::ReadOnly ? "writeonly " : "")
            );
        }
        case Kind::TypeBool: {
            auto w = as<BoolType>(this)->bit_width();
            if (w == 0) return fmt::format("{}bool", C(Cyan));
            return fmt::format("{}b{}", C(Cyan), w);
        }

        case Kind::TypeInt: {
            auto i = as<IntType>(this);
            if (i->bit_width() == 0) return fmt::format("{}{}int", C(Cyan), i->is_signed() ? "" : "u");
            return fmt::format("{}{}{}", C(Cyan), i->is_signed() ? "i" : "u", i->bit_width());
        }

        case Kind::TypeFloat: {
            auto w = as<FloatType>(this)->bit_width();
            if (w == 0) return fmt::format("{}float", C(Cyan));
            return fmt::format("{}f{}", C(Cyan), w);
        }

        case Kind::TypeC: {
            auto c = as<CType>(this);
            auto access = c->access();
            return fmt::format(
                "{}{}{}",
                C(Cyan),
                access == TypeAccess::ReadOnly ? "readonly " : (access == TypeAccess::ReadOnly ? "writeonly " : ""),
                ToString(c->kind())
            );
        }
    }
}

namespace {
using lcc::as;
using lcc::cast;
using lcc::is;

using namespace lcc;

struct ASTPrinter : lcc::utils::ASTPrinter<ASTPrinter, layec::BaseNode, layec::Type> {
    void PrintStatementHeader(const layec::Statement* s) {
        using K = layec::Statement::Kind;
        switch (s->kind()) {
            default: {
                PrintBasicHeader("<??? Statement>", s);
                out += fmt::format(" {}{}\n", C(Magenta), +s->kind());
            } break;

            case K::DeclBinding: {
                auto n = cast<layec::BindingDecl>(s);
                PrintLinkage(n->linkage());
                PrintBasicHeader("BindingDecl", n);
                out += fmt::format(
                    " {} {}{}\n",
                    n->type()->string(use_colour),
                    C(Green),
                    n->name()
                );
            } break;

            case K::DeclFunction: {
                auto n = cast<layec::FunctionDecl>(s);
                PrintLinkage(n->linkage());
                PrintBasicHeader("FunctionDecl", n);
                out += fmt::format(
                    " {} {}{}{}(",
                    n->return_type()->string(use_colour),
                    C(Green),
                    n->name(),
                    C(White)
                );
                auto params = n->params();
                for (lcc::usz i = 0; i < params.size(); i++) {
                    if (i > 0) out += fmt::format("{}, ", C(White));
                    out += fmt::format("{} {}{}", params[i].type->string(use_colour), C(White), params[i].name);
                }
                out += fmt::format("{})\n", C(White));
            } break;

            case K::DeclStruct: {
                auto n = cast<layec::StructDecl>(s);
                PrintBasicHeader("StructDecl", n);
                out += fmt::format(
                    " {}{}\n",
                    C(Green),
                    n->name()
                );
            } break;

            case K::DeclEnum: {
                auto n = cast<layec::EnumDecl>(s);
                PrintBasicHeader("EnumDecl", n);
                out += fmt::format(
                    " {}{}\n",
                    C(Green),
                    n->name()
                );
            } break;

            case K::DeclAlias: {
                auto n = cast<layec::AliasDecl>(s);
                PrintBasicHeader("AliasDecl", n);
                out += "\n";
            } break;

            case K::DeclImport: {
                auto n = cast<layec::ImportHeader>(s);
                PrintBasicHeader("ImportHeader", n);
                out += "\n";
            } break;

            case K::Block: {
                auto n = cast<layec::BlockStatement>(s);
                PrintBasicHeader("BlockStatement", n);
                out += "\n";
            } break;

            case K::Assign: {
                auto n = cast<layec::AssignStatement>(s);
                PrintBasicHeader("AssignStatement", n);
                out += "\n";
            } break;

            case K::Delete: {
                auto n = cast<layec::DeleteStatement>(s);
                PrintBasicHeader("DeleteStatement", n);
                out += "\n";
            } break;

            case K::Expr: {
                auto n = cast<layec::ExprStatement>(s);
                PrintExprHeader(n->expr());
            } break;

            case K::Empty: {
                auto n = cast<layec::EmptyStatement>(s);
                PrintBasicHeader("EmptyStatement", n);
                out += "\n";
            } break;

            case K::If: {
                auto n = cast<layec::IfStatement>(s);
                PrintBasicHeader("IfStatement", n);
                out += "\n";
            } break;

            case K::For: {
                auto n = cast<layec::ForStatement>(s);
                PrintBasicHeader("ForStatement", n);
                out += "\n";
            } break;

            case K::ForEach: {
                auto n = cast<layec::ForEachStatement>(s);
                PrintBasicHeader("ForEachStatement", n);
                out += "\n";
            } break;

            case K::DoFor: {
                auto n = cast<layec::DoForStatement>(s);
                PrintBasicHeader("DoForStatement", n);
                out += "\n";
            } break;

            case K::Switch: {
                auto n = cast<layec::SwitchStatement>(s);
                PrintBasicHeader("SwitchStatement", n);
                out += "\n";
            } break;

            case K::Return: {
                auto n = cast<layec::ReturnStatement>(s);
                PrintBasicHeader("ReturnStatement", n);
                out += "\n";
            } break;

            case K::Break: {
                auto n = cast<layec::BreakStatement>(s);
                PrintBasicHeader("BreakStatement", n);
                out += "\n";
            } break;

            case K::Continue: {
                auto n = cast<layec::ContinueStatement>(s);
                PrintBasicHeader("ContinueStatement", n);
                out += "\n";
            } break;

            case K::Fallthrough: {
                auto n = cast<layec::FallthroughStatement>(s);
                PrintBasicHeader("FallthroughStatement", n);
                out += "\n";
            } break;

            case K::Defer: {
                auto n = cast<layec::DeferStatement>(s);
                PrintBasicHeader("DeferStatement", n);
                out += "\n";
            } break;

            case K::Goto: {
                auto n = cast<layec::GotoStatement>(s);
                PrintBasicHeader("GotoStatement", n);
                out += "\n";
            } break;
        }
    }

    void PrintExprHeader(const layec::Expr* e) {
        using K = layec::Expr::Kind;
        switch (e->kind()) {
            default: {
                PrintBasicHeader(R"(<??? Expr>)", e);
                out += fmt::format(" {}{}\n", C(Magenta), +e->kind());
            } break;

            case K::Unary: {
                auto n = cast<layec::UnaryExpr>(e);
                PrintBasicHeader("UnaryExpr", n);
                out += "\n";
            } break;

            case K::Binary: {
                auto n = cast<layec::BinaryExpr>(e);
                PrintBasicHeader("BinaryExpr", n);
                out += fmt::format(" {}{}\n", C(White), ToString(n->operator_kind()));
            } break;

            case K::And: {
                auto n = cast<layec::AndExpr>(e);
                PrintBasicHeader("AndExpr", n);
                out += "\n";
            } break;

            case K::Or: {
                auto n = cast<layec::OrExpr>(e);
                PrintBasicHeader("OrExpr", n);
                out += "\n";
            } break;

            case K::Xor: {
                auto n = cast<layec::XorExpr>(e);
                PrintBasicHeader("XorExpr", n);
                out += "\n";
            } break;

            case K::UnwrapNilable: {
                auto n = cast<layec::UnwrapNilableExpr>(e);
                PrintBasicHeader("UnwrapNilableExpr", n);
                out += "\n";
            } break;

            case K::LookupName: {
                auto n = cast<layec::NameExpr>(e);
                PrintBasicHeader("NameExpr", e);
                out += fmt::format(" {}{}\n", C(Green), n->name());
            } break;

            case K::LookupPath: {
                auto n = cast<layec::PathExpr>(e);
                PrintBasicHeader("PathExpr", e);
                out += " ";
                for (usz i = 0; i < n->names().size(); i++) {
                    if (i > 0) out += "::";
                    out += fmt::format("{}{}", C(Green), n->names()[i]);
                }
                out += "\n";
            } break;

            case K::FieldIndex: {
                auto n = cast<layec::FieldIndexExpr>(e);
                PrintBasicHeader("FieldIndexExpr", n);
                out += "\n";
            } break;

            case K::ValueIndex: {
                auto n = cast<layec::ValueIndexExpr>(e);
                PrintBasicHeader("ValueIndexExpr", n);
                out += "\n";
            } break;

            case K::Slice: {
                auto n = cast<layec::SliceExpr>(e);
                PrintBasicHeader("SliceExpr", n);
                out += "\n";
            } break;

            case K::Call: {
                auto n = cast<layec::CallExpr>(e);
                PrintBasicHeader("CallExpr", n);
                out += "\n";
            } break;

            case K::Ctor: {
                auto n = cast<layec::CtorExpr>(e);
                PrintBasicHeader("CtorExpr", n);
                out += "\n";
            } break;

            case K::Not: {
                auto n = cast<layec::NotExpr>(e);
                PrintBasicHeader("NotExpr", n);
                out += "\n";
            } break;

            case K::Cast: {
                auto n = cast<layec::CastExpr>(e);
                PrintBasicHeader("CastExpr", n);
                out += "\n";
            } break;

            case K::New: {
                auto n = cast<layec::NewExpr>(e);
                PrintBasicHeader("NewExpr", n);
                out += "\n";
            } break;

            case K::Try: {
                auto n = cast<layec::TryExpr>(e);
                PrintBasicHeader("TryExpr", n);
                out += "\n";
            } break;

            case K::Catch: {
                auto n = cast<layec::CatchExpr>(e);
                PrintBasicHeader("CatchExpr", n);
                if (not n->error_name().empty())
                    out += fmt::format(" {}{}\n", C(Green), n->error_name());
            } break;

            case K::Do: {
                auto n = cast<layec::DoExpr>(e);
                PrintBasicHeader("DoExpr", n);
                out += "\n";
            } break;

            case K::Sizeof: {
                auto n = cast<layec::SizeofExpr>(e);
                PrintBasicHeader("SizeofExpr", n);
                out += "\n";
            } break;

            case K::Offsetof: {
                auto n = cast<layec::OffsetofExpr>(e);
                PrintBasicHeader("OffsetofExpr", n);
                out += "\n";
            } break;

            case K::Alignof: {
                auto n = cast<layec::AlignofExpr>(e);
                PrintBasicHeader("AlignofExpr", n);
                out += "\n";
            } break;

            case K::LitNil: {
                PrintBasicHeader("LitNilExpr", e);
                out += "\n";
            } break;

            case K::LitBool: {
                auto n = cast<layec::LitBoolExpr>(e);
                PrintBasicHeader("LitBoolExpr", n);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;

            case K::LitString: {
                auto n = cast<layec::LitStringExpr>(e);
                PrintBasicHeader("LitStringExpr", n);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;

            case K::LitInt: {
                auto n = cast<layec::LitIntExpr>(e);
                PrintBasicHeader("LitIntExpr", n);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;

            case K::LitFloat: {
                auto n = cast<layec::LitFloatExpr>(e);
                PrintBasicHeader("LitFloatExpr", n);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;
        }
    }

    void PrintHeader(const layec::BaseNode* b) {
        if (b->is_statement())
            PrintStatementHeader(static_cast<const layec::Statement*>(b));
        else PrintExprHeader(static_cast<const layec::Expr*>(b));
    }

    void PrintStatement(const layec::Statement* s, std::string leading_text) {
        using K = layec::Statement::Kind;
        switch (s->kind()) {
            default: break;

            case K::DeclBinding: {
                auto n = as<layec::BindingDecl>(s);
                if (n->init()) {
                    layec::Expr* children[] = {n->init()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::DeclFunction: {
                auto n = as<layec::FunctionDecl>(s);
                if (not n->body()) break;
                if (auto block = cast<layec::BlockStatement>(n->body())) {
                    PrintChildren(block->children(), leading_text);
                } else {
                    layec::Statement* children[] = {n->body()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::DeclStruct: {
                auto n = as<layec::StructDecl>(s);
                auto fields = n->fields();
                auto variants = n->variants();

                std::vector<layec::Decl*> children{};
                children.insert(children.end(), fields.begin(), fields.end());
                children.insert(children.end(), variants.begin(), variants.end());
                PrintChildren(children, leading_text);
            } break;

            case K::DeclEnum: {
                auto n = as<layec::EnumDecl>(s);
                auto variants = n->variants();

                for (lcc::usz i = 0; i < variants.size(); i++) {
                    const bool last = i == variants.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto variant = variants[i];
                    PrintBasicHeader("EnumVariant", n);

                    out += fmt::format(
                        " {}{}\n",
                        C(Green),
                        variant.name
                    );

                    if (auto init = variant.init) {
                        layec::BaseNode* children[] = {init};
                        PrintChildren(children, leading_text + (last ? "  " : "│ "));
                    }
                }
            } break;

            case K::Block: {
                auto n = cast<layec::BlockStatement>(s);
                PrintChildren(n->children(), leading_text);
            } break;

            case K::Assign: {
                auto n = as<layec::AssignStatement>(s);
                layec::Expr* children[] = {n->target(), n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Delete: {
                auto n = as<layec::DeleteStatement>(s);
                layec::Expr* children[] = {n->expr()};
                PrintChildren(children, leading_text);
            } break;

            case K::Expr: {
                auto n = cast<layec::ExprStatement>(s);
                PrintExpr(n->expr(), leading_text);
            } break;

            case K::If: {
                auto n = as<layec::IfStatement>(s);
                layec::BaseNode* children[] = {n->condition(), n->pass(), n->fail()};
                PrintChildren(children, leading_text);
            } break;

            case K::For: {
                auto n = as<layec::ForStatement>(s);
                if (n->init()) {
                    layec::BaseNode* children[] = {n->init(), n->condition(), n->increment(), n->pass(), n->fail()};
                    PrintChildren(children, leading_text);
                } else {
                    layec::BaseNode* children[] = {n->condition(), n->pass(), n->fail()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::ForEach: {
                auto n = as<layec::ForEachStatement>(s);
                layec::BaseNode* children[] = {n->sequence(), n->pass(), n->fail()};
                PrintChildren(children, leading_text);
            } break;

            case K::DoFor: {
                auto n = as<layec::DoForStatement>(s);
                layec::BaseNode* children[] = {n->body(), n->condition()};
                PrintChildren(children, leading_text);
            } break;

            case K::Switch: {
                auto n = as<layec::SwitchStatement>(s);
                auto cases = n->cases();

                for (lcc::usz i = 0; i < cases.size(); i++) {
                    const bool last = i == cases.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto case_ = cases[i];

                    if (case_.is_default()) {
                        PrintBasicHeader("Default", n);
                        layec::BaseNode* children[] = {case_.body};
                        PrintChildren(children, leading_text + (last ? "  " : "│ "));
                    } else {
                        PrintBasicHeader("Case", n);
                        layec::BaseNode* children[] = {case_.value, case_.body};
                        PrintChildren(children, leading_text + (last ? "  " : "│ "));
                    }
                }
            } break;

            case K::Return: {
                auto n = as<layec::ReturnStatement>(s);
                if (n->value()) {
                    layec::BaseNode* children[] = {n->value()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Defer: {
                auto n = as<layec::DeferStatement>(s);
                layec::BaseNode* children[] = {n->statement()};
                PrintChildren(children, leading_text);
            } break;
        }
    }

    void PrintExpr(const layec::Expr* e, std::string leading_text) {
        using K = layec::Expr::Kind;
        switch (e->kind()) {
            default: break;

            case K::Unary: {
                auto n = as<layec::UnaryExpr>(e);
                layec::BaseNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Binary: {
                auto n = as<layec::BinaryExpr>(e);
                layec::BaseNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::And: {
                auto n = as<layec::AndExpr>(e);
                layec::BaseNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::Or: {
                auto n = as<layec::OrExpr>(e);
                layec::BaseNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::Xor: {
                auto n = as<layec::XorExpr>(e);
                layec::BaseNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::UnwrapNilable: {
                auto n = as<layec::UnwrapNilableExpr>(e);
                layec::BaseNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::LookupName: {
                auto n = as<layec::NameExpr>(e);
            } break;

            case K::LookupPath: {
                auto n = as<layec::PathExpr>(e);
            } break;

            case K::FieldIndex: {
                auto n = as<layec::FieldIndexExpr>(e);
                layec::BaseNode* children[] = {n->target()};
                PrintChildren(children, leading_text);
            } break;

            case K::ValueIndex: {
                auto n = as<layec::ValueIndexExpr>(e);
                auto indices = n->indices();
                if (indices.empty()) {
                    layec::Expr* children[] = {n->target()};
                    PrintChildren(children, leading_text);
                } else {
                    std::vector<layec::Expr*> children{};
                    children.push_back(n->target());
                    children.insert(children.end(), indices.begin(), indices.end());
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Slice: {
                auto n = as<layec::SliceExpr>(e);
                std::vector<layec::Expr*> children{};
                children.push_back(n->target());
                if (n->offset()) children.push_back(n->offset());
                if (n->length()) children.push_back(n->length());
                PrintChildren(children, leading_text);
            } break;

            case K::Call: {
                auto n = as<layec::CallExpr>(e);
                auto args = n->args();
                if (args.empty()) {
                    layec::Expr* children[] = {n->target()};
                    PrintChildren(children, leading_text);
                } else {
                    std::vector<layec::Expr*> children{};
                    children.push_back(n->target());
                    children.insert(children.end(), args.begin(), args.end());
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Ctor: {
                auto n = as<layec::CtorExpr>(e);
                auto inits = n->inits();

                for (lcc::usz i = 0; i < inits.size(); i++) {
                    const bool last = i == inits.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto init = inits[i];
                    PrintBasicHeader("CtorFieldInit", n);

                    layec::BaseNode* children[] = {init.value};
                    PrintChildren(children, leading_text + (last ? "  " : "│ "));
                }
            } break;

            case K::Not: {
                auto n = as<layec::NotExpr>(e);
                layec::BaseNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Cast: {
                auto n = as<layec::CastExpr>(e);
                layec::BaseNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::New: {
                auto n = as<layec::NewExpr>(e);
                auto inits = n->inits();

                for (lcc::usz i = 0; i < inits.size(); i++) {
                    const bool last = i == inits.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto init = inits[i];
                    PrintBasicHeader("CtorFieldInit", n);

                    layec::BaseNode* children[] = {init.value};
                    PrintChildren(children, leading_text + (last ? "  " : "│ "));
                }
            } break;

            case K::Try: {
                auto n = as<layec::TryExpr>(e);
                layec::Expr* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Catch: {
                auto n = as<layec::CatchExpr>(e);
                layec::BaseNode* children[] = {n->value(), n->body()};
                PrintChildren(children, leading_text);
            } break;

            case K::Do: {
                auto n = as<layec::DoExpr>(e);
                PrintChildren(n->statements(), leading_text);
            } break;
        }
    }

    void operator()(const layec::BaseNode* b, std::string leading_text = "") {
        PrintHeader(b);
        if (b->is_statement())
            PrintStatement(static_cast<const layec::Statement*>(b), leading_text);
        else PrintExpr(static_cast<const layec::Expr*>(b), leading_text);
    }
};
} // namespace

void layec::Module::print() {
    ASTPrinter p{true};
    for (auto* node : top_level_decls) p(node);
}
