#include "laye/ast.hh"

#include <laye/parser.hh>

namespace lcc::laye {
using Tk = TokenKind;

static std::unordered_map<Tk, int> operator_precedence{
    {Tk::Or, 5},
    {Tk::Xor, 5},
    {Tk::And, 6},

    {Tk::EqualEqual, 10},
    {Tk::BangEqual, 10},

    {Tk::Less, 20},
    {Tk::Greater, 20},
    {Tk::LessEqual, 20},
    {Tk::GreaterEqual, 20},

    {Tk::Ampersand, 30},
    {Tk::Pipe, 30},
    {Tk::Tilde, 30},
    {Tk::LessLess, 30},
    {Tk::GreaterGreater, 30},

    {Tk::Plus, 40},
    {Tk::Minus, 40},

    {Tk::Star, 50},
    {Tk::Slash, 50},
    {Tk::Percent, 50},
};

static std::unordered_map<Tk, OperatorKind> assign_operator_kinds{
    {Tk::AmpersandEqual, OperatorKind::AndEqual},
    {Tk::PipeEqual, OperatorKind::OrEqual},
    {Tk::TildeEqual, OperatorKind::XorEqual},
    {Tk::LessLessEqual, OperatorKind::LshEqual},
    {Tk::GreaterGreaterEqual, OperatorKind::RshEqual},

    {Tk::PlusEqual, OperatorKind::AddEqual},
    {Tk::MinusEqual, OperatorKind::SubEqual},

    {Tk::StarEqual, OperatorKind::MulEqual},
    {Tk::SlashEqual, OperatorKind::DivEqual},
    {Tk::PercentEqual, OperatorKind::ModEqual},
};

static std::unordered_map<Tk, OperatorKind> binary_operator_kinds{
    {Tk::EqualEqual, OperatorKind::Equal},
    {Tk::BangEqual, OperatorKind::NotEqual},

    {Tk::Less, OperatorKind::Less},
    {Tk::Greater, OperatorKind::Greater},
    {Tk::LessEqual, OperatorKind::LessEqual},
    {Tk::GreaterEqual, OperatorKind::GreaterEqual},

    {Tk::Ampersand, OperatorKind::And},
    {Tk::Pipe, OperatorKind::Or},
    {Tk::Tilde, OperatorKind::Compl},
    {Tk::LessLess, OperatorKind::Lsh},
    {Tk::GreaterGreater, OperatorKind::Rsh},

    {Tk::Plus, OperatorKind::Add},
    {Tk::Minus, OperatorKind::Sub},

    {Tk::Star, OperatorKind::Mul},
    {Tk::Slash, OperatorKind::Div},
    {Tk::Percent, OperatorKind::Mod},
};

OperatorKind Parser::BinaryOperatorKind(TokenKind tokenKind) {
    if (auto result = assign_operator_kinds.find(tokenKind); result != assign_operator_kinds.end()) {
        return result->second;
    }

    return OperatorKind::Invalid;
}

std::unique_ptr<Module> Parser::Parse(Context* context, File& file) {
    auto result = new Module;

    Parser parser{context, &file, result};
    parser.NextToken();

    while (not parser.At(Tk::Eof)) {
        bool is_export = false;
        if (parser.At(Tk::Export) and parser.PeekAt(1, Tk::Import)) {
            is_export = true;
            parser.NextToken();
            goto parse_import;
        }

        if (parser.At(Tk::Import)) {
        parse_import:;
            auto import_header = parser.ParseImportDecl(is_export);
            if (import_header) result->headers.push_back(*import_header);
        } else {
            break;
        }
    }

    while (not parser.At(Tk::Eof)) {
        auto top_level = parser.ParseTopLevel();
        if (top_level) result->top_level_decls.push_back(*top_level);
    }

    return std::unique_ptr<Module>(result);
}

void Parser::Synchronise() {
    LCC_ASSERT(not IsInSpeculativeParse(), "The Synchronise function is not intended to be called from within a speculative parse state since it is for recovering from nasty errors");
    while (not At(Tk::Eof, Tk::SemiColon, Tk::CloseBrace))
        NextToken();
}

auto Parser::ParseTopLevel() -> Result<Decl*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto decl = ParseDecl();
    // TODO(local): any additional error checking for top level decls?
    return decl;
}

auto Parser::TryParseDecl() -> Result<Decl*> {
    auto GetModifiers = [&](bool allocate) {
        std::vector<DeclModifier> modifiers{};
        while (At(Tk::Inline, Tk::Export, Tk::Const, Tk::Foreign, Tk::Callconv)) {
            if (std::find_if(modifiers.begin(), modifiers.end(), [&](const auto& m) { return m.decl_kind == tok.kind; }) != modifiers.end()) {
                if (allocate) Error("Duplicate modifier for declaration");
            }

            if (At(Tk::Inline, Tk::Export, Tk::Const)) {
                modifiers.push_back(DeclModifier{tok.kind});
                NextToken();
            } else if (At(Tk::Foreign)) {
                NextToken();

                if (At(Tk::String)) {
                    modifiers.push_back(DeclModifier{Tk::Foreign, tok.text});
                    NextToken();
                } else {
                    modifiers.push_back(DeclModifier{Tk::Foreign});
                }
            } else if (At(Tk::Callconv)) {
                NextToken();

                bool had_open = At(Tk::OpenParen);
                if (not Consume(Tk::OpenParen)) {
                    if (allocate) Error("Expected '('");
                }

                auto call_conv = CallConv::C;
                if (At(Tk::Ident)) {
                    if (tok.text == "cdecl") {
                        call_conv = CallConv::C;
                    } else if (tok.text == "laye") {
                        call_conv = CallConv::Laye;
                    } else if (tok.text == "intercept") {
                        call_conv = CallConv::Intercept;
                    } else {
                        if (allocate) Error("Expected calling convention name (one of 'cdecl', 'laye' or 'intercept')");
                    }
                    NextToken();
                }

                if (had_open and not Consume(Tk::CloseParen)) {
                    if (allocate) Error("Expected ')'");
                }

                modifiers.push_back(DeclModifier{Tk::Callconv, "", call_conv});
            }
        }

        return modifiers;
    };

    LCC_ASSERT(not IsInSpeculativeParse());

    { // speculative parse
        auto spec = EnterSpeculativeParse();
        auto discard_modifiers = GetModifiers(false);

        if (not At(Tk::Struct, Tk::Enum)) {
            auto before_type_offset = tok.location.pos;
            if (not SpeculativeParseType()) {
                return Result<Decl*>::Null();
            }

            LCC_ASSERT(tok.location.pos != before_type_offset);
            if (not At(Tk::Ident) or not PeekAt(1, Tk::SemiColon, Tk::OpenParen, Tk::Equal)) {
                return Result<Decl*>::Null();
            }
        }
    } // end speculative parse

    LCC_ASSERT(not IsInSpeculativeParse());
    auto modifiers = GetModifiers(true);

    if (Consume(Tk::Struct)) {
        LCC_ASSERT(false, "TODO");
    } else if (Consume(Tk::Enum)) {
        LCC_ASSERT(false, "TODO");
    }

    auto type = ParseType();

    LCC_ASSERT(At(Tk::Ident));
    auto location = tok.location;
    auto name = tok.text;
    NextToken();

    auto template_params = MaybeParseTemplateParams();

    if (Consume(Tk::OpenParen)) {
        std::vector<FunctionParam> params{};
        while (not At(Tk::Eof, Tk::CloseParen)) {
            auto type = ParseType();

            auto name = tok.text;
            if (not Consume(Tk::Ident)) {
                name.clear();
                Error("Expected identifier");
            }

            auto init = Result<Expr*>::Null();
            if (Consume(Tk::Equal)) {
                init = ParseExpr();
            }

            params.push_back(FunctionParam{*type, name, *init});

            if (not Consume(Tk::Comma)) break;

            if (At(Tk::CloseParen)) {
                Error("Expected type");
                break;
            }
        }

        if (not Consume(Tk::CloseParen)) {
            Error("Expected ')'");
        }

        auto body = Result<Statement*>::Null();
        if (At(Tk::OpenBrace)) {
            body = ParseBlockStatement();
        } else {
            if (Consume(Tk::EqualGreater)) {
                auto expr = ParseExpr();
                if (expr) body = new (*this) ExprStatement{*expr};
            }

            if (not Consume(Tk::SemiColon)) {
                Error("Expected ';'");
            }
        }

        return new (*this) FunctionDecl{location, modifiers, *type, name, *template_params, params, *body};
    }

    if (template_params.is_value() and not(*template_params).empty()) {
        Error("Binding declarations cannot have template parameters");
    }

    auto init = Result<Expr*>::Null();
    if (Consume(Tk::Equal)) {
        init = ParseExpr();
    }

    if (not Consume(Tk::SemiColon)) {
        Error("Expected ';'");
    }

    return new (*this) BindingDecl{location, modifiers, *type, name, *init};
}

auto Parser::ParseDecl() -> Result<Decl*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto decl_result = TryParseDecl();
    if (not decl_result) return decl_result.diag();

    auto diag = *decl_result;
    if (not diag) return Error("Expected 'struct', 'enum' or identifier");

    return diag;
}

auto Parser::ParseDeclOrStatement() -> Result<Statement*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto decl_result = TryParseDecl();
    if (not decl_result) return decl_result.diag();

    auto decl = *decl_result;
    if (decl) return decl;

    return ParseStatement();
}

auto Parser::ParseStatement(bool consumeSemi) -> Result<Statement*> {
    auto expr = ParseExpr();

    if (consumeSemi and not Consume(Tk::SemiColon)) {
        Error("Expected ';'");
    }

    if (not expr) return expr.diag();

    auto expr_statement = new (*this) ExprStatement{*expr};
    return expr_statement;
}

auto Parser::ParseBlockStatement() -> Result<BlockStatement*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto start_location = CurrLocation();
    LCC_ASSERT(Consume(Tk::OpenBrace));

    std::vector<Statement*> children{};

    while (not At(Tk::Eof, Tk::CloseBrace)) {
        auto child = ParseDeclOrStatement();
        if (child) children.push_back(*child);
    }

    if (not Consume(Tk::CloseBrace)) {
        Error("Expected '}}'");
    }

    auto end_location = CurrLocation();
    return new (*this) BlockStatement{Location{start_location, end_location}, children};
}

auto Parser::TryParseTemplateParams(bool allocate) -> Result<std::vector<TemplateParam>> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse(), "TryParseTemplateParams requires that the allocate parameter be the opposite of the result of IsInSpeculativeParse(). If allocations are enabled, then no speculative parse stack should exist. If allocations are disabled, then it is required that a specilative parse stack exists.");

    std::vector<TemplateParam> template_params{};
    return template_params;
}

// import "file";
// import std;
// import * from "file";
// import foo, bar from "file";
// import "file" as file;
auto Parser::ParseImportDecl(bool is_export) -> Result<ImportHeader*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto start_location = CurrLocation();

    if (not Consume(Tk::Import)) {
        return Error("Expected 'import'");
    }

    std::string import_name{};
    std::string alias{};

    auto HandleImportAlias = [&]() {
        if (not Consume(Tk::As)) {
            return;
        }

        auto alias_token = tok;
        if (not Consume(Tk::Ident, Tk::String)) {
            Error(alias_token.location, "Expected string literal or identifier as import alias name");
        } else {
            alias = alias_token.text;
        }
    };

    if (At(Tk::Star)) {
        NextToken();

        // TODO(local): special case parse additional import names and error?

        if (not Consume(Tk::From)) {
            Error("Expected 'from'");
        }

        auto import_name_token = tok;
        if (not Consume(Tk::Ident, Tk::String)) {
            Error(import_name_token.location, "Expected string literal or identifier as import file/package name");
            Synchronise(); // we give up parsing this, sync
            return new (*this) ImportHeader(
                start_location,
                is_export,
                "",
                true
            );
        }

        import_name = tok.text;
        HandleImportAlias();
        ExpectSemiColon();

        return new (*this) ImportHeader(
            start_location,
            is_export,
            import_name,
            true,
            alias
        );
    }

    std::vector<std::string> import_names{};

    if (At(Tk::Ident) and PeekAt(1, Tk::Comma, Tk::From)) {
        // TODO(local): special case parse handle wildcard and error?
        while (At(Tk::Ident)) {
            import_names.push_back(tok.text);
            NextToken();

            if (not Consume(Tk::Comma)) break;

            if (not At(Tk::Ident)) {
                Error("Expected identifier to continue list of names to import");
                break;
            }
        }
    }

    if (not import_names.empty()) {
        if (not Consume(Tk::From)) {
            Error("Expected 'from' after list of names to import");
        }
    }

    auto import_name_token = tok;
    if (not Consume(Tk::Ident, Tk::String)) {
        Error(import_name_token.location, "Expected string literal or identifier as import file/package name");
        Synchronise(); // we give up parsing this, sync
        return new (*this) ImportHeader(
            start_location,
            is_export,
            "",
            true
        );
    }

    import_name = tok.text;
    HandleImportAlias();
    ExpectSemiColon();

    auto import_locaiton = start_location;
    return new (*this) ImportHeader(
        import_locaiton,
        is_export,
        import_name,
        std::move(import_names),
        alias
    );
}

auto Parser::TryParseTypeContinue(Type* type, bool allocate, bool allowFunctions) -> Result<Type*> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse(), "TryParseTypeContinue requires that the allocate parameter be the opposite of the result of IsInSpeculativeParse(). If allocations are enabled, then no speculative parse stack should exist. If allocations are disabled, then it is required that a specilative parse stack exists.");
    return type;
}

auto Parser::TryParseTemplateArguments(bool allocate) -> Result<std::vector<Expr*>> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse());
    
    std::vector<Expr*> args{};
    if (not At(Tk::Less)) {
        // regardless of `allocate`, we already have the args. return them instead of nullptr
        return args;
    }

    LCC_ASSERT(false, "TODO");
    return args;
}

auto Parser::TryParseNameOrPath(
    bool allocate,
    std::function<Expr*(Location location, std::string name, std::vector<Expr*> template_args)> name_ctor,
    std::function<Expr*(PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args)> path_ctor
) -> Result<Expr*> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse(), "TryParseNameOrPath requires that the allocate parameter be the opposite of the result of IsInSpeculativeParse(). If allocations are enabled, then no speculative parse stack should exist. If allocations are disabled, then it is required that a specilative parse stack exists.");
    LCC_ASSERT(At(Tk::Ident, Tk::ColonColon, Tk::Global), "TryParseNameOrPath requires that the current parser state be at 'global', '::' or an identifier");

    auto path_kind = PathKind::Default;

    std::vector<std::string> path_names{};
    std::vector<Location> path_locations{};

    if (Consume(Tk::Global)) {
        path_kind = PathKind::Global;
        if (not Consume(Tk::ColonColon)) {
            if (allocate) return Error("Expected '::");
            else goto return_null_type;
        }

        goto start_path_resolution_parse;
    } else if (Consume(Tk::ColonColon)) {
        path_kind = PathKind::Headless;
        goto start_path_resolution_parse;
    } else if (Consume(Tk::Ident)) {
        { // so we can goto correctly without moving these two declarations
            auto name_text = tok.text;
            auto name_location = tok.location;

            if (not Consume(Tk::ColonColon)) {
                auto template_args_result = TryParseTemplateArguments(allocate);
                if (not template_args_result) {
                    LCC_ASSERT(not allocate);
                    return template_args_result.diag();
                }

                if (allocate) {
                    return name_ctor(name_location, name_text, *template_args_result);
                } else {
                    goto return_null_type;
                }
            }

            path_names.push_back(name_text);
            path_locations.push_back(name_location);
        }

    start_path_resolution_parse:;
        do {
            auto name_text = tok.text;
            auto name_location = tok.location;

            if (not Consume(Tk::Ident)) {
                if (allocate) return Error("Expected identifier");
                else goto return_null_type;
            }

            path_names.push_back(name_text);
            path_locations.push_back(name_location);
        } while (Consume(Tk::ColonColon));

        auto template_args_result = TryParseTemplateArguments(allocate);
        if (not template_args_result) {
            LCC_ASSERT(not allocate);
            return template_args_result.diag();
        }

        // TODO(local): (try) parse template arguments
        if (allocate) {
            return path_ctor(path_kind, path_names, path_locations, *template_args_result);
        } else {
            goto return_null_type;
        }
    }

return_null_type:;
    LCC_ASSERT(not allocate, "Can only return a nullptr value for the result type if we are not allowed to allocate data (read: we are in a speculative parse mode)");
    return Result<Expr*>::Null();
}

auto Parser::TryParseType(bool allocate, bool allowFunctions) -> Result<Type*> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse(), "TryParseType requires that the allocate parameter be the opposite of the result of IsInSpeculativeParse(). If allocations are enabled, then no speculative parse stack should exist. If allocations are disabled, then it is required that a specilative parse stack exists.");

    auto start = CurrLocation();

    auto type_access = TypeAccess::Default;
    bool has_errored_for_access = false;

    while (At(Tk::Readonly, Tk::Writeonly)) {
        if (type_access != TypeAccess::Default && not has_errored_for_access) {
            if (allocate) Error("Only one of 'readonly' or 'writeonly' may be specified for type access modifiers");
            has_errored_for_access = true;
        }

        if (Consume(Tk::Readonly)) {
            type_access = TypeAccess::ReadOnly;
        } else if (Consume(Tk::Writeonly)) {
            type_access = TypeAccess::WriteOnly;
        } else {
            LCC_ASSERT(false, "Somehow unhandled case of type access modifiers");
        }
    }

    if (Consume(Tk::Bang)) {
        if (type_access != TypeAccess::Default) {
            if (allocate) Error("Error-union types cannot have access modifiers");
        }

        auto value_type = TryParseType(allocate, false);
        Type* error_union_type = nullptr;
        if (allocate) {
            error_union_type = new (*this) ErrUnionType{GetLocation(start), "", *value_type};
        }

        return TryParseTypeContinue(error_union_type, allocate, false);
    }

    if (At(Tk::Ident, Tk::ColonColon, Tk::Global)) {
        // these constructors are already wrapped in `if (allocate)` in the TryparseNameOrPath function,
        //  so we don't have to do that explicitly.
        auto NameCtor = [&](Location location, std::string name, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) NameType{location, type_access, name, template_args};
        };

        auto PathCtor = [&](PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) PathType{path_kind, type_access, names, locations, template_args};
        };

        auto id_type = TryParseNameOrPath(allocate, NameCtor, PathCtor);
        // since the TryParseNameOrPath function works for the expression case, too, we have to
        //  explicitly cast back to a Type* to continue type parsing.
        return TryParseTypeContinue(static_cast<Type*>(*id_type), allocate);
    }

    if (At(Tk::Bool)) {
        if (type_access != TypeAccess::Default) {
            if (allocate) Error("Access modifiers do not apply to bool types");
        }

        auto location = tok.location;
        int bit_width = (int) tok.integer_value;

        NextToken();

        auto float_type = Result<Type*>::Null();
        if (allocate) {
            float_type = new (*this) BoolType{location, bit_width};
        }

        return TryParseTypeContinue(*float_type, allocate);
    }

    if (At(Tk::Int, Tk::UInt)) {
        auto kw_kind = tok.kind;

        if (type_access != TypeAccess::Default) {
            if (allocate) Error("Access modifiers do not apply to integer types");
        }

        auto location = tok.location;
        int bit_width = (int) tok.integer_value;

        NextToken();

        auto int_type = Result<Type*>::Null();
        if (allocate) {
            int_type = new (*this) IntType{location, kw_kind == Tk::Int, bit_width};
        }

        return TryParseTypeContinue(*int_type, allocate);
    }

    if (At(Tk::Float)) {
        if (type_access != TypeAccess::Default) {
            if (allocate) Error("Access modifiers do not apply to float types");
        }

        auto location = tok.location;
        int bit_width = (int) tok.integer_value;

        NextToken();

        auto float_type = Result<Type*>::Null();
        if (allocate) {
            float_type = new (*this) FloatType{location, bit_width};
        }

        return TryParseTypeContinue(*float_type, allocate);
    }

    LCC_ASSERT(false, "TODO(local): finish type parsing (including nilable!!!)");

return_null_type:;
    LCC_ASSERT(not allocate, "Can only return a nullptr value for the result type if we are not allowed to allocate data (read: we are in a speculative parse mode)");
    return Result<Type*>::Null();
}

auto Parser::ParseConstructorBody() -> Result<std::vector<CtorFieldInit>> {
    LCC_ASSERT(not IsInSpeculativeParse());

    std::vector<CtorFieldInit> inits{};
    if (Consume(Tk::OpenBrace)) {
        while (At(Tk::Ident)) {
            auto ident_name = tok.text;
            auto ident_location = tok.location;
            NextToken();

            Expr* init = nullptr;
            if (Consume(Tk::Equal) and not At(Tk::Comma, Tk::CloseBrace)) {
                if (not At(Tk::Comma, Tk::CloseBrace)) {
                    init = *ParseExpr();
                } else {
                    Error("Expected expression");
                }
            } else {
                Error("Expected '='");
                if (not At(Tk::Comma, Tk::CloseBrace)) {
                    init = *ParseExpr();
                }
            }

            inits.push_back(CtorFieldInit{ident_name, ident_location, init});
            if ((not Consume(Tk::Comma) and not At(Tk::Ident)) or At(Tk::CloseBrace)) break;
        }

        if (not Consume(Tk::CloseBrace)) {
            Error("Expected '}}'");
        }
    }

    return inits;
}

auto Parser::ParsePrimaryExprContinue(Expr* expr) -> Result<Expr*> {
    LCC_ASSERT(not IsInSpeculativeParse());
    LCC_ASSERT(expr);

    if (Consume(Tk::OpenParen)) {
        std::vector<Expr*> args{};

        while (not At(Tk::Eof)) {
            auto arg = ParseExpr();
            if (arg) args.push_back(*arg);

            if (not Consume(Tk::Comma) or At(Tk::CloseParen)) break;
        }

        if (not Consume(Tk::CloseParen)) {
            Error("Expected ')'");
        }

        return ParsePrimaryExprContinue(new (*this) CallExpr{GetLocation(expr->location()), expr, args});
    } else if (Consume(Tk::Dot)) {
        std::string field_name = "";
        if (At(Tk::Ident)) {
            field_name = tok.text;
            NextToken();
        } else Error("Expected identifier");
        return ParsePrimaryExprContinue(new (*this) FieldIndexExpr{GetLocation(expr->location()), expr, std::move(field_name)});
    } else if (Consume(Tk::OpenBracket)) {
        if (Consume(Tk::Colon)) {
            Expr* slice_length = nullptr;
            if (not At(Tk::CloseBracket)) {
                slice_length = *ParseExpr();
            }

            if (not Consume(Tk::CloseBracket)) {
                Error("Expected ']'");
            }

            return ParsePrimaryExprContinue(new (*this) SliceExpr{GetLocation(expr->location()), expr, nullptr, slice_length});
        }

        auto first_expr = ParseExpr();

        if (Consume(Tk::Colon)) {
            Expr* slice_length = nullptr;
            if (not At(Tk::CloseBracket)) {
                slice_length = *ParseExpr();
            }

            if (not Consume(Tk::CloseBracket)) {
                Error("Expected ']'");
            }

            return ParsePrimaryExprContinue(new (*this) SliceExpr{GetLocation(expr->location()), expr, *first_expr, slice_length});
        }

        std::vector<Expr*> index_args{};
        index_args.push_back(*first_expr);

        if (Consume(Tk::Comma)) {
            while (not At(Tk::Eof)) {
                auto arg = ParseExpr();
                if (arg) index_args.push_back(*arg);

                if (not Consume(Tk::Comma) or At(Tk::CloseParen)) break;
            }
        }

        if (not Consume(Tk::CloseBracket)) {
            Error("Expected ']'");
        }

        return ParsePrimaryExprContinue(new (*this) ValueIndexExpr{GetLocation(expr->location()), expr, index_args});
    } else if (Consume(Tk::Catch)) {
        std::string capture_name = "";
        if (Consume(Tk::OpenParen)) {
            if (At(Tk::Ident)) {
                capture_name = tok.text;
                NextToken();
            } else Error("Expected identifier");

            if (not Consume(Tk::CloseParen)) {
                Error("Expected ')'");
            }
        }

        auto body = ParseStatement(false);
        return new (*this) CatchExpr{GetLocation(expr->location()), expr, capture_name, *body};
    }

    return expr;
}

auto Parser::ParsePrimaryIdentExprContinue(Expr* expr) -> Result<Expr*> {
    LCC_ASSERT(not IsInSpeculativeParse());
    LCC_ASSERT(expr);

    if (At(Tk::OpenBrace)) {
        auto body = ParseConstructorBody();
        auto location = GetLocation(expr->location());

        Type* type = nullptr;
        if (auto name_expr = cast<NameExpr>(expr)) {
            type = new (*this) NameType{name_expr->location(), TypeAccess::Default, name_expr->name(), name_expr->template_args()};
        } else if (auto path_expr = cast<PathExpr>(expr)) {
            type = new (*this) PathType{path_expr->path_kind(), TypeAccess::Default, path_expr->names(), path_expr->locations(), path_expr->template_args()};
        } else {
            LCC_ASSERT(false, "How did we get here?");
        }

        return new (*this) CtorExpr{location, type, *body};
    }

    return expr;
}

auto Parser::ParsePrimaryExpr() -> Result<Expr*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto location = CurrLocation();

    if (At(Tk::Ident, Tk::ColonColon, Tk::Global)) {
        auto NameCtor = [&](Location location, std::string name, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) NameExpr{location, name, template_args};
        };

        auto PathCtor = [&](PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) PathExpr{path_kind, names, locations, template_args};
        };

        auto id_expr = TryParseNameOrPath(true, NameCtor, PathCtor);
        if (not id_expr) return id_expr.diag();

        return ParsePrimaryIdentExprContinue(*id_expr);
    } else if (Consume(Tk::Try)) {
        auto try_expr = ParsePrimaryExpr();
        if (not try_expr) return try_expr.diag();
        return new (*this) TryExpr{try_expr->location(), *try_expr};
    } else if (Consume(Tk::New)) {
        Expr* allocator = nullptr;
        if (Consume(Tk::OpenParen)) {
            allocator = *ParseExpr();
            if (not Consume(Tk::CloseParen)) {
                if (allocator) Error("Expected ')'");
                else {
                    Synchronise();
                    return Error("Expected ')'");
                }
            }
        }

        auto type = ParseType();
        auto body = ParseConstructorBody();

        return new (*this) NewExpr{GetLocation(location), allocator, *type, *body};
    } else if (At(Tk::True, Tk::False)) {
        auto literal_value = tok.kind == Tk::True;
        NextToken();
        return ParsePrimaryExprContinue(new (*this) LitBoolExpr{location, literal_value});
    } else if (At(Tk::LitString)) {
        auto literal_value = tok.text;
        NextToken();
        return ParsePrimaryExprContinue(new (*this) LitStringExpr{location, literal_value});
    } else if (At(Tk::LitInt)) {
        auto literal_value = tok.integer_value;
        NextToken();
        return ParsePrimaryExprContinue(new (*this) LitIntExpr{location, literal_value});
    } else if (At(Tk::LitFloat)) {
        auto literal_value = tok.float_value;
        NextToken();
        return ParsePrimaryExprContinue(new (*this) LitFloatExpr{location, literal_value});
    }

    NextToken();
    return Error(location, "Unexpected token when parsing expression");
}

bool Parser::IsBinaryOperatorWithPrecedence(int precedence, int& next_precedence) {
    if (auto binop = operator_precedence.find(tok.kind); binop != operator_precedence.end()) {
        auto p = binop->second;
        if (p >= precedence) {
            next_precedence = p;
            return true;
        }
    }

    return false;
}

auto Parser::ParseBinaryExpr(Expr* lhs, int precedence) -> Result<Expr*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    int next_precedence = 0;
    while (IsBinaryOperatorWithPrecedence(precedence, next_precedence)) {
        auto op_token_kind = tok.kind;
        NextToken();

        auto rhs = ParsePrimaryExpr();
        if (not rhs) return rhs.diag();

        int rhs_precedence = next_precedence;
        while (IsBinaryOperatorWithPrecedence(rhs_precedence, next_precedence)) {
            rhs = ParseBinaryExpr(*rhs, rhs_precedence);
            if (not rhs) return rhs.diag();
        }

        if (op_token_kind == Tk::And) {
            lhs = new (*this) AndExpr{Location{lhs->location(), rhs->location()}, lhs, *rhs};
        } else if (op_token_kind == Tk::Or) {
            lhs = new (*this) OrExpr{Location{lhs->location(), rhs->location()}, lhs, *rhs};
        } else if (op_token_kind == Tk::Xor) {
            lhs = new (*this) XorExpr{Location{lhs->location(), rhs->location()}, lhs, *rhs};
        } else lhs = new (*this) BinaryExpr{Location{lhs->location(), rhs->location()}, BinaryOperatorKind(op_token_kind), lhs, *rhs};
    }

    return lhs;
}

auto Parser::ParseExpr() -> Result<Expr*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto primary = ParsePrimaryExpr();
    if (not primary) return primary.diag();

    return ParseBinaryExpr(*primary);
}
} // namespace lcc::laye
