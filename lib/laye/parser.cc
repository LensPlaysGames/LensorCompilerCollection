#include <laye/parser.hh>

namespace lcc::laye {
using Tk = TokenKind;

std::unique_ptr<Module> Parser::Parse(Context* context, File* file) {
    auto result = new Module;

    Parser parser{context, file, result};

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
            if (not SpeculativeParseType()) {
                return Result<Decl*>::Null();
            }

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
    auto decl_result = TryParseDecl();
    if (not decl_result) return decl_result.diag();

    auto diag = *decl_result;
    if (not diag) return Error("Expected 'struct', 'enum' or identifier");

    return diag;
}

auto Parser::ParseDeclOrStatement() -> Result<std::variant<Statement*, Decl*>> {
    auto decl_result = TryParseDecl();
    if (not decl_result) return decl_result.diag();

    auto diag = *decl_result;
    if (diag) return std::variant<Statement*, Decl*>(diag);

    LCC_ASSERT(false, "TODO");
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
    LCC_ASSERT(false, "TODO: implement TryParseTypeContinue in Laye parser");
}

auto Parser::TryParseNameOrPath(
    bool allocate,
    std::function<Expr*(Location location, std::string name)> name_ctor,
    std::function<Expr*(PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations)> path_ctor
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

            NextToken();
            if (not Consume(Tk::ColonColon)) {
                if (allocate) {
                    return name_ctor(name_location, name_text);
                } else goto return_null_type;
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

        if (allocate) {
            return path_ctor(path_kind, path_names, path_locations);
        } else goto return_null_type;
    }

return_null_type:;
    LCC_ASSERT(not allocate, "Can only return a nullptr value for the result type if we are not allowed to allocate data (read: we are in a speculative parse mode)");
    return Result<Expr*>::Null();
}

auto Parser::TryParseType(bool allocate, bool allowFunctions) -> Result<Type*> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse(), "TryParseType requires that the allocate parameter be the opposite of the result of IsInSpeculativeParse(). If allocations are enabled, then no speculative parse stack should exist. If allocations are disabled, then it is required that a specilative parse stack exists.");

    u32 start = tok.location.pos;

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
        auto NameCtor = [&](Location location, std::string name) -> Expr* {
            return new (*this) NameType{location, type_access, name};
        };

        auto PathCtor = [&](PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations) -> Expr* {
            return new (*this) PathType{path_kind, type_access, names, locations};
        };

        auto id_type = TryParseNameOrPath(allocate, NameCtor, PathCtor);
        // since the TryParseNameOrPath function works for the expression case, too, we have to
        //  explicitly cast back to a Type* to continue type parsing.
        return TryParseTypeContinue(static_cast<Type*>(*id_type), allocate);
    }

return_null_type:;
    LCC_ASSERT(not allocate, "Can only return a nullptr value for the result type if we are not allowed to allocate data (read: we are in a speculative parse mode)");
    return Result<Type*>::Null();
}
} // namespace lcc::laye
