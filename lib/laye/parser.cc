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
    {Tk::Equal, OperatorKind::Assign},

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

OperatorKind Parser::AssignOperatorKind(TokenKind tokenKind) {
    if (auto result = assign_operator_kinds.find(tokenKind); result != assign_operator_kinds.end()) {
        return result->second;
    }

    return OperatorKind::Invalid;
}

OperatorKind Parser::BinaryOperatorKind(TokenKind tokenKind) {
    if (auto result = binary_operator_kinds.find(tokenKind); result != binary_operator_kinds.end()) {
        return result->second;
    }

    return OperatorKind::Invalid;
}

auto Parser::Parse(LayeContext* laye_context, File& file) -> Module* {
    auto result = new Module{laye_context, &file};

    Parser parser{laye_context, &file, result};
    parser.NextToken();

    parser.scope_stack.push_back(new (parser) Scope{nullptr});

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
            if (import_header) {
                auto import = *import_header;
                result->add_header(import);

                std::string import_namespace = import->query();

                auto file_dir = file.path().parent_path();
                auto import_file_path = file_dir / import_namespace;
                if (not fs::exists(import_file_path)) {
                    for (auto& dir : laye_context->context()->include_directories()) {
                        import_file_path = fs::path{dir} / import_namespace;
                        if (fs::exists(import_file_path))
                            break;
                    }
                }

                if (not fs::exists(import_file_path)) {
                    parser.Error(import->location(), "Cannot import file because it does not exist");
                    continue;
                }

                if (import->has_alias()) {
                    import_namespace = import->alias();
                } else {
                    // TODO(local): this should turn `import_namespace` into a valid Laye identifier if it isn't already
                    fs::path name_as_path = import_namespace;
                    import_namespace = name_as_path.filename().replace_extension("").string();
                }

                import->import_namespace(import_namespace);

                auto imported_module = laye_context->get_or_load_module(import_file_path);
                import->target_module(imported_module);
            }
        } else {
            break;
        }
    }

    while (not parser.At(Tk::Eof)) {
        auto top_level = parser.ParseTopLevel();
        if (top_level) result->add_top_level_decl(*top_level);
        else {
            parser.NextToken();
            parser.Synchronise();
        }
    }

    return result;
}

void Parser::Synchronise() {
    LCC_ASSERT(not IsInSpeculativeParse(), "The Synchronise function is not intended to be called from within a speculative parse state since it is for recovering from nasty errors");
    while (not At(Tk::Eof, Tk::SemiColon, Tk::CloseBrace, Tk::CloseParen))
        NextToken();
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
        while (At(Tk::Inline, Tk::Export, Tk::Const, Tk::Foreign, Tk::Callconv, Tk::Nodiscard, Tk::Impure)) {
            if (std::find_if(modifiers.begin(), modifiers.end(), [&](const auto& m) { return m.decl_kind == tok.kind; }) != modifiers.end()) {
                if (allocate) Error("Duplicate modifier for declaration");
            }

            if (At(Tk::Inline, Tk::Export, Tk::Const, Tk::Nodiscard, Tk::Impure)) {
                modifiers.push_back(DeclModifier{tok.location, tok.kind});
                NextToken();
            } else if (At(Tk::Foreign)) {
                NextToken();

                if (At(Tk::LitString)) {
                    modifiers.push_back(DeclModifier{tok.location, Tk::Foreign, tok.text});
                    NextToken();
                } else {
                    modifiers.push_back(DeclModifier{tok.location, Tk::Foreign});
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
                    if (allocate) Error("Expected ')' to close calling convention specification");
                }

                modifiers.push_back(DeclModifier{tok.location, Tk::Callconv, "", call_conv});
            }
        }

        return modifiers;
    };

    LCC_ASSERT(not IsInSpeculativeParse());

    { // speculative parse
        auto spec = EnterSpeculativeParse();
        auto discard_modifiers = GetModifiers(false);

        if (not At(Tk::Struct, Tk::Enum, Tk::Alias)) {
            auto before_type_offset = tok.location.pos;
            if (not SpeculativeParseType()) {
                return Result<Decl*>::Null();
            }

            LCC_ASSERT(tok.location.pos != before_type_offset);
            if (not will_attempt_to_parse_template_args and (not At(Tk::Ident) or not PeekAt(1, Tk::SemiColon, Tk::OpenParen, Tk::Equal))) {
                return Result<Decl*>::Null();
            }
        }
    } // end speculative parse

    LCC_ASSERT(not IsInSpeculativeParse());
    auto modifiers = GetModifiers(true);

    if (At(Tk::Struct)) {
        auto struct_result = ParseStruct(std::move(modifiers));
        if (not struct_result) return struct_result.diag();
        return CurrScope()->declare(module, struct_result->name(), *struct_result);
    } else if (At(Tk::Enum)) {
        LCC_ASSERT(false, "TODO enum");
    } else if (At(Tk::Alias)) {
        auto alias_result = ParseAlias(std::move(modifiers), false);
        if (not alias_result) return alias_result.diag();
        return CurrScope()->declare(module, alias_result->name(), *alias_result);
    }

    auto type = ParseType();

    LCC_ASSERT(At(Tk::Ident));
    auto location = tok.location;
    auto name = tok.text;
    NextToken();

    auto template_params = MaybeParseTemplateParams();

    if (Consume(Tk::OpenParen)) {
        // TODO(local): parse varargs in function decls
        std::vector<BindingDecl*> params{};
        while (not At(Tk::Eof, Tk::CloseParen)) {
            auto type = ParseType();

            auto param_name = tok.text;
            auto param_location = tok.location;
            if (not Consume(Tk::Ident)) {
                param_name.clear();
                Error("Expected identifier");
            }

            auto init = Result<Expr*>::Null();
            if (Consume(Tk::Equal)) {
                init = ParseExpr();
            }

            params.push_back(new (*module) BindingDecl{module, param_location, {}, *type, param_name, *init});

            if (not Consume(Tk::Comma)) break;

            if (At(Tk::CloseParen)) {
                Error("Expected type");
                break;
            }
        }

        if (not Consume(Tk::CloseParen)) {
            Error("Expected ')' to close function parameter list");
        }

        FunctionDecl* func_decl{nullptr};
        {
            auto func_scope = EnterScope();
            func_scope.scope->set_function_scope();

            for (auto& param : params) {
                (void) CurrScope()->declare(module, param->name(), param);
            }

            auto body = Result<Statement*>::Null();
            if (At(Tk::OpenBrace)) {
                body = ParseBlockStatement(std::move(func_scope));
            } else {
                if (Consume(Tk::EqualGreater)) {
                    auto expr = ParseExpr();
                    if (expr) body = new (*this) ExprStatement{*expr};
                }

                if (not Consume(Tk::SemiColon)) {
                    Error("Expected ';'");
                }
            }

            func_decl = new (*this) FunctionDecl{module, location, modifiers, *type, name, *template_params, std::move(params), *body};
        }

        LCC_ASSERT(func_decl);
        if (not CurrScope()->parent() and func_decl->is_export()) module->add_export(func_decl);
        return CurrScope()->declare(module, func_decl->name(), func_decl);
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

    auto binding_decl = new (*this) BindingDecl{module, location, modifiers, *type, name, *init};
    if (not CurrScope()->parent() and binding_decl->is_export()) module->add_export(binding_decl);
    return CurrScope()->declare(module, binding_decl->name(), binding_decl);
}

auto Parser::ParseStruct(std::vector<DeclModifier> mods) -> Result<StructDecl*> {
    LCC_ASSERT(not IsInSpeculativeParse());
    LCC_ASSERT(Consume(Tk::Struct, Tk::Variant));

    auto location = tok.location;
    std::string struct_name{};
    if (At(Tk::Ident)) {
        struct_name = tok.text;
        NextToken();
    } else Error("Expected identifier");

    auto template_params = MaybeParseTemplateParams();
    if (not template_params) return template_params.diag();

    if (not Consume(Tk::OpenBrace)) {
        return Error("Expected '{{'");
    }

    std::vector<BindingDecl*> fields{};
    std::vector<StructDecl*> variants{};

    while (not At(Tk::CloseBrace)) {
        if (At(Tk::Variant)) {
            auto variant = ParseStruct();
            if (not variant) return variant.diag();
            variants.push_back(*variant);
        } else {
            auto field_start = CurrLocation();

            // TODO(local): struct field modifiers
            std::vector<DeclModifier> mods{};

            Type* field_type = nullptr;
            {
                auto field_type_result = ParseType();
                if (field_type_result) field_type = *field_type_result;
                else {
                    Synchronise();
                    continue;
                }
            }

            std::string field_name{};
            if (At(Tk::Ident)) {
                field_name = tok.text;
                NextToken();
            } else Error("Expected identifier");

            Expr* init = nullptr;
            if (Consume(Tk::Equal)) {
                auto init_result = ParseExpr();
                if (init_result) init = *init_result;
            }

            if (not Consume(Tk::SemiColon)) {
                Error("Expected ';'");
            }

            fields.push_back(new (*this) BindingDecl{module, GetLocation(field_start), mods, field_type, field_name, init});
        }
    }

    if (not Consume(Tk::CloseBrace)) {
        Error("Expected '}}'");
    }

    return new (*this) StructDecl{module, location, mods, struct_name, *template_params, fields, variants};
}

auto Parser::ParseAlias(std::vector<DeclModifier> mods, bool is_strict) -> Result<AliasDecl*> {
    LCC_ASSERT(not IsInSpeculativeParse());
    LCC_ASSERT(Consume(Tk::Alias));

    auto location = tok.location;
    std::string alias_name{};
    if (At(Tk::Ident)) {
        alias_name = tok.text;
        NextToken();
    } else Error("Expected identifier");

    if (not Consume(Tk::Equal)) {
        return Error("Expected '='");
    }

    Type* alias_type = nullptr;
    {
        auto alias_type_result = ParseType();
        if (alias_type_result) alias_type = *alias_type_result;
        else {
            Synchronise();
            return alias_type_result.diag();
        }
    }

    if (not Consume(Tk::SemiColon)) {
        Error("Expected ';'");
    }

    return new (*this) AliasDecl{module, location, mods, is_strict, alias_name, alias_type};
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

    auto start_location = CurrLocation();
    auto start_kind = tok.kind;

    auto decl_result = TryParseDecl();
    if (not decl_result) return decl_result.diag();

    auto decl = *decl_result;
    if (decl) return decl;

    LCC_ASSERT(CurrLocation().pos == start_location.pos);
    LCC_ASSERT(tok.kind == start_kind);
    return ParseStatement();
}

auto Parser::ParseStatement(bool consumeSemi) -> Result<Statement*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto start = CurrLocation();

    if (At(Tk::OpenBrace)) {
        auto block_scope = EnterScope();
        return ParseBlockStatement(std::move(block_scope));
    } else if (Consume(Tk::Return)) {
        auto return_value = Result<Expr*>::Null();
        if (not At(Tk::SemiColon)) {
            return_value = ParseExpr();
            if (not return_value) return return_value.diag();
        }

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) ReturnStatement{start, *return_value};
    } else if (Consume(Tk::Break)) {
        std::string target{};
        if (At(Tk::Ident)) {
            target = tok.text;
            NextToken();
        }

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) BreakStatement{start, target};
    } else if (Consume(Tk::Continue)) {
        std::string target{};
        if (At(Tk::Ident)) {
            target = tok.text;
            NextToken();
        }

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) ContinueStatement{start, target};
    } else if (Consume(Tk::Discard)) {
        Expr* value = nullptr;
        if (not At(Tk::SemiColon)) {
            if (auto expr = ParseExpr()) value = *expr;
        } else Error(CurrLocation(), "Expression expected");

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) DiscardStatement{start, value};
    } else if (Consume(Tk::Delete)) {
        Expr* value = nullptr;
        if (not At(Tk::SemiColon)) {
            if (auto expr = ParseExpr()) value = *expr;
        } else Error(CurrLocation(), "Expression expected");

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) DeleteStatement{start, value};
    } else if (Consume(Tk::Defer)) {
        auto statement_result = ParseStatement(false);
        if (not statement_result) {
            Synchronise();
            return statement_result.diag();
        }

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) DeferStatement{start, *statement_result};
    } else if (Consume(Tk::Goto)) {
        std::string target{};
        if (At(Tk::Ident)) {
            target = tok.text;
            NextToken();
        } else Error("Expected identifier");

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) GotoStatement{start, target};
    } else if (Consume(Tk::Xyzzy)) {
        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) XyzzyStatement{start};
    } else if (Consume(Tk::If)) {
        if (not Consume(Tk::OpenParen)) {
            Error("Expected '('");
        }

        auto condition_result = ParseExpr();
        if (not condition_result) {
            Synchronise();
            return condition_result.diag();
        }

        if (not Consume(Tk::CloseParen)) {
            Error("Expected ')' to close if condition");
        }

        auto pass_body = ParseStatement();
        if (not pass_body) {
            Synchronise();
            return pass_body.diag();
        }

        auto fail_body = Result<Statement*>::Null();
        if (Consume(Tk::Else)) {
            fail_body = ParseStatement();
            if (not fail_body) {
                Synchronise();
                return fail_body.diag();
            }
        }

        return new (*this) IfStatement{start, *condition_result, *pass_body, *fail_body};
    } else if (Consume(Tk::For)) {
        if (not Consume(Tk::OpenParen)) {
            auto pass_body = ParseStatement();
            if (not pass_body) {
                Synchronise();
                return pass_body.diag();
            }

            return new (*this) ForStatement{start, nullptr, *pass_body, nullptr};
        }

        auto first_start = CurrLocation();
        bool starts_with_decl = false;
        if (not At(Tk::SemiColon)) {
            auto spec = EnterSpeculativeParse();
            if (SpeculativeParseType() and At(Tk::Ident) and PeekAt(1, Tk::Equal, Tk::SemiColon, Tk::Colon)) {
                starts_with_decl = true;
            }
        }

        Statement* init = nullptr;
        if (starts_with_decl or At(Tk::SemiColon)) {
            if (not At(Tk::SemiColon)) {
                auto init_type = *ParseType();
                auto init_name = tok;
                if (not Consume(Tk::Ident)) LCC_ASSERT(false, "should've been at an ident, what happened?");

                Expr* init_value = nullptr;
                if (Consume(Tk::Equal)) {
                    auto init_value_result = ParseExpr();
                    if (init_value_result) init_value = *init_value_result;
                }

                init = new (*this) BindingDecl{module, GetLocation(first_start), {}, init_type, init_name.text, init_value};
            }

            if (starts_with_decl and Consume(Tk::Colon)) {
                LCC_ASSERT(init != nullptr);
                auto init_binding = cast<BindingDecl>(init);

                auto type = init_binding->type();
                auto name = init_binding->name();
                auto value = init_binding->init();

                if (value) {
                    Error("Cannot initialize a 'for' iterator value");
                }

                auto sequence = ParseExpr();
                if (not sequence) {
                    Synchronise();
                    return sequence.diag();
                }

                if (not Consume(Tk::CloseParen)) {
                    Error("Expected ')' to close 'for' statement specifier");
                }

                auto pass_body = ParseStatement();
                if (not pass_body) {
                    Synchronise();
                    return pass_body.diag();
                }

                auto fail_body = Result<Statement*>::Null();
                if (Consume(Tk::Else)) {
                    fail_body = ParseStatement();
                    if (not fail_body) {
                        Synchronise();
                        return fail_body.diag();
                    }
                }

                return new (*this) ForEachStatement{start, type, name, *sequence, *pass_body, *fail_body};
            }

            if (not Consume(Tk::SemiColon)) {
                Error("Expected ';' in 'for' statement specifier");
            }

        continue_c_style_for:;
            auto condition = Result<Expr*>::Null();
            if (not Consume(Tk::SemiColon)) {
                condition = ParseExpr();
                if (not condition) {
                    Synchronise();
                    return condition.diag();
                }

                if (not Consume(Tk::SemiColon)) {
                    Error("Expected ';' in 'for' statement specifier");
                }
            }

            auto increment = Result<Expr*>::Null();
            if (not Consume(Tk::SemiColon)) {
                increment = ParseExpr();
                if (not increment) {
                    Synchronise();
                    return increment.diag();
                }
            }

            if (not Consume(Tk::CloseParen)) {
                Error("Expected ')' to close 'for' statement specifier");
            }

            auto pass_body = ParseStatement();
            if (not pass_body) {
                Synchronise();
                return pass_body.diag();
            }

            auto fail_body = Result<Statement*>::Null();
            if (Consume(Tk::Else)) {
                fail_body = ParseStatement();
                if (not fail_body) {
                    Synchronise();
                    return fail_body.diag();
                }
            }

            return new (*this) ForStatement{start, init, *condition, *increment, *pass_body, *fail_body};
        } else {
            Expr* first_expr = nullptr;
            {
                if (auto first_expr_result = ParseExpr())
                    first_expr = *first_expr_result;
            }

            if (Consume(Tk::SemiColon)) {
                init = new (*this) ExprStatement{first_expr};
                goto continue_c_style_for;
            }

            if (not Consume(Tk::CloseParen)) {
                Error("Expected ')' to close 'for' statement specifier");
            }

            auto pass_body = ParseStatement();
            if (not pass_body) {
                Synchronise();
                return pass_body.diag();
            }

            auto fail_body = Result<Statement*>::Null();
            if (Consume(Tk::Else)) {
                fail_body = ParseStatement();
                if (not fail_body) {
                    Synchronise();
                    return fail_body.diag();
                }
            }

            return new (*this) ForStatement{start, first_expr, *pass_body, *fail_body};
        }
    } else if (At(Tk::Do) and PeekAt(1, Tk::OpenBrace)) {
        NextToken();

        auto block_scope = EnterScope();
        auto body = ParseBlockStatement(std::move(block_scope));
        if (not body) return body.diag();

        if (not Consume(Tk::For)) {
            return Error("Expected 'for'");
        }

        if (not Consume(Tk::OpenParen)) {
            Error("Expected '('");
        }

        auto condition_result = ParseExpr();
        if (not condition_result) return condition_result.diag();

        if (not Consume(Tk::CloseParen)) {
            Error("Expected ')' to close do-for condition");
        }

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        return new (*this) DoForStatement{start, *condition_result, *body};
    }

    auto expr = ParseExpr();
    if (not expr) {
        Synchronise();
        return expr.diag();
    }

    auto maybe_eq_tok = tok;
    if (Consume(
            Tk::Equal,
            Tk::PlusEqual,
            Tk::MinusEqual,
            Tk::SlashEqual,
            Tk::StarEqual,
            Tk::PercentEqual,
            Tk::LessEqual,
            Tk::GreaterEqual,
            Tk::AmpersandEqual,
            Tk::PipeEqual,
            Tk::TildeEqual,
            Tk::LessLessEqual,
            Tk::GreaterGreaterEqual
        )) {
        auto rhs = ParseExpr();
        if (not rhs) {
            Synchronise();
            return rhs.diag();
        }

        if (consumeSemi and not Consume(Tk::SemiColon)) {
            Error("Expected ';'");
        }

        auto assign_op = AssignOperatorKind(maybe_eq_tok.kind);
        return new (*this) AssignStatement{maybe_eq_tok.location, assign_op, *expr, *rhs};
    }

    if (consumeSemi and not Consume(Tk::SemiColon)) {
        Error("Expected ';'");
    }

    return new (*this) ExprStatement{*expr};
}

auto Parser::ParseBlockStatement([[maybe_unused]] ScopeRAII sc) -> Result<BlockStatement*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto start_location = CurrLocation();
    if (not Consume(Tk::OpenBrace)) LCC_ASSERT(false);

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
    if (not allocate) {
        return template_params;
    }

    if (Consume(Tk::Less)) {
        auto t = EnterTemplateParse();

        if (not At(Tk::Greater)) {
            while (not At(Tk::Eof)) {
                if (At(Tk::Ident) and PeekAt(1, Tk::Comma, Tk::Greater, Tk::GreaterGreater)) {
                    std::string name = tok.text;
                    auto location = tok.location;
                    NextToken();

                    template_params.push_back(TemplateParam{std::move(name), location, nullptr});
                } else {
                    auto type = ParseType();
                    if (not type) goto continue_template_list;

                    std::string name = tok.text;
                    auto location = tok.location;

                    if (not Consume(Tk::Ident)) {
                        name.clear();
                        Error("Expected identifier to name template parameter");
                    }

                    template_params.push_back(TemplateParam{std::move(name), location, *type});
                }

            continue_template_list:;
                if (not Consume(Tk::Comma) or At(Tk::CloseParen)) break;
            }
        } else {
            Error("Empty template parameter list");
        }

        if (not ConsumeTemplateClose()) {
            Error("Expected '>' to close template parameter list");
        }
    }

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
        if (not Consume(Tk::Ident, Tk::LitString)) {
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
        if (not Consume(Tk::Ident, Tk::LitString)) {
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
    if (not Consume(Tk::Ident, Tk::LitString)) {
        Error(import_name_token.location, "Expected string literal or identifier as import file/package name");
        Synchronise(); // we give up parsing this, sync
        return new (*this) ImportHeader(
            start_location,
            is_export,
            "",
            true
        );
    }

    import_name = import_name_token.text;
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

auto Parser::TryParseTypeContinue(Type* type, bool allocate, bool allow_functions) -> Result<Type*> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse(), "TryParseTypeContinue requires that the allocate parameter be the opposite of the result of IsInSpeculativeParse(). If allocations are enabled, then no speculative parse stack should exist. If allocations are disabled, then it is required that a specilative parse stack exists.");
    if (not allocate) LCC_ASSERT(type == nullptr);

    if (At(Tk::Eof)) return type;

    auto start = CurrLocation();
    if (type) start = type->location();

    auto type_access = TypeAccess::ReadOnly;
    bool has_errored_for_access = false;

    while (At(Tk::Mut)) {
        if (type_access != TypeAccess::ReadOnly && not has_errored_for_access) {
            if (allocate) Error("Only one of 'mut' may be specified for type access modifiers");
            has_errored_for_access = true;
        }

        type_access = TypeAccess::Mutable;
    }

    if (Consume(Tk::Star)) {
        Type* pointer_type = nullptr;
        if (allocate) {
            pointer_type = new (*this) PointerType{GetLocation(start), type_access, type};
        }
        return TryParseTypeContinue(pointer_type, allocate, allow_functions);
    } else if (Consume(Tk::Ampersand)) {
        Type* reference_type = nullptr;
        if (allocate) {
            reference_type = new (*this) ReferenceType{GetLocation(start), type_access, type};
        }
        return TryParseTypeContinue(reference_type, allocate, allow_functions);
    } else if (Consume(Tk::OpenBracket)) {
        if (Consume(Tk::CloseBracket)) {
            Type* slice_type = nullptr;
            if (allocate) {
                slice_type = new (*this) SliceType{GetLocation(start), type_access, type};
            }
            return TryParseTypeContinue(slice_type, allocate, allow_functions);
        } else if (At(Tk::Star) and PeekAt(1, Tk::CloseBracket)) {
            NextToken();
            NextToken();

            Type* buffer_type = nullptr;
            if (allocate) {
                buffer_type = new (*this) BufferType{GetLocation(start), type_access, type};
            }
            return TryParseTypeContinue(buffer_type, allocate, allow_functions);
        }

        std::vector<Expr*> rank_lengths{};
        while (not At(Tk::Eof)) {
            auto len = ParseExpr();
            if (not len) return len.diag();
            if (allocate) rank_lengths.push_back(*len);
            if (not Consume(Tk::Comma) or At(Tk::CloseBracket)) break;
        }

        if (not Consume(Tk::CloseBracket)) {
            Error("Expected ']'");
        }

        Type* array_type = nullptr;
        if (allocate) {
            array_type = new (*this) ArrayType{GetLocation(start), type_access, type, rank_lengths};
        }
        return TryParseTypeContinue(array_type, allocate, allow_functions);
    } else if (Consume(Tk::Question)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Nilable types cannot have access modifiers");
        }

        Type* nilable_type = nullptr;
        if (allocate) {
            nilable_type = new (*this) NilableType{type};
        }
        return TryParseTypeContinue(nilable_type, allocate, allow_functions);
    } else if (allow_functions and Consume(Tk::OpenParen)) {
        // TODO(local): get a calling convention in here somewhere
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Function types cannot have access modifiers");
        }

        // TODO(local): parse varargs in function types
        std::vector<Type*> param_types{};
        while (not At(Tk::Eof)) {
            auto param_type = TryParseType(allocate);
            if (not param_type) return param_type.diag();
            if (allocate) param_types.push_back(*param_type);
            if (not Consume(Tk::Comma) or At(Tk::CloseParen)) break;
        }

        if (not Consume(Tk::CloseParen)) {
            if (allocate) Error("Expected ')' to close function type");
        }

        Type* function_type = nullptr;
        if (allocate) {
            function_type = new (*this) FuncType{GetLocation(start), type, param_types};
        }
        return TryParseTypeContinue(function_type, allocate, allow_functions);
    } else {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Expected '*' or '[' to construct a container type");
        }
        return type;
    }

    LCC_ASSERT(false, "unreachable");
}

auto Parser::TryParseTemplateArguments(bool allocate) -> Result<std::vector<Expr*>> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse());

    std::vector<Expr*> template_args{};
    if (not allocate or not At(Tk::Less) or not IsLocationImmediatelyFollowing()) {
        // regardless of `allocate`, we already have the args. return them instead of nullptr
        return template_args;
    }

    LCC_ASSERT(allocate and At(Tk::Less) and IsLocationImmediatelyFollowing());

    NextToken();
    auto t = EnterTemplateParse();

    if (not At(Tk::Greater)) {
        while (not At(Tk::Eof)) {
            {
                auto spec = EnterSpeculativeParse();
                if (SpeculativeParseType()) goto parse_type_arg;
            }

            {
                auto arg = ParseExpr();
                if (arg) template_args.push_back(*arg);
            }

        continue_parse_args:;
            if (not Consume(Tk::Comma) or At(Tk::CloseParen)) break;
            else continue;

        parse_type_arg:
            auto arg = ParseType();
            LCC_ASSERT(arg);
            template_args.push_back(*arg);
            goto continue_parse_args;
        }
    } else {
        Error("Empty template argument list");
    }

    if (not ConsumeTemplateClose()) {
        Error("Expected '>' to close template parameter list");
    }

    return template_args;
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
            else goto return_null_expr;
        }

        goto start_path_resolution_parse;
    } else if (Consume(Tk::ColonColon)) {
        path_kind = PathKind::Headless;
        goto start_path_resolution_parse;
    } else if (At(Tk::Ident)) {
        { // so we can goto correctly without moving these two declarations
            auto name_text = tok.text;
            auto name_location = tok.location;
            NextToken();

            if (not Consume(Tk::ColonColon)) {
                auto template_args_result = TryParseTemplateArguments(allocate);
                if (not template_args_result) {
                    LCC_ASSERT(not allocate);
                    return template_args_result.diag();
                }

                if (allocate) {
                    return name_ctor(name_location, name_text, *template_args_result);
                } else {
                    if (At(Tk::Less) and IsLocationImmediatelyFollowing())
                        will_attempt_to_parse_template_args = true;
                    goto return_null_expr;
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
                else goto return_null_expr;
            }

            path_names.push_back(name_text);
            path_locations.push_back(name_location);
        } while (Consume(Tk::ColonColon));

        auto template_args_result = TryParseTemplateArguments(allocate);
        if (not template_args_result) {
            LCC_ASSERT(not allocate);
            return template_args_result.diag();
        }

        if (allocate) {
            return path_ctor(path_kind, path_names, path_locations, *template_args_result);
        } else {
            if (At(Tk::Less) and IsLocationImmediatelyFollowing())
                will_attempt_to_parse_template_args = true;
            goto return_null_expr;
        }
    }

return_null_expr:;
    LCC_ASSERT(not allocate, "Can only return a nullptr value for the result type if we are not allowed to allocate data (read: we are in a speculative parse mode)");
    return Result<Expr*>::Null();
}

auto Parser::TryParseType(bool allocate, bool allowFunctions) -> Result<Type*> {
    LCC_ASSERT((not allocate) == IsInSpeculativeParse(), "TryParseType requires that the allocate parameter be the opposite of the result of IsInSpeculativeParse(). If allocations are enabled, then no speculative parse stack should exist. If allocations are disabled, then it is required that a specilative parse stack exists.");

    will_attempt_to_parse_template_args = false;
    auto start = CurrLocation();

    auto type_access = TypeAccess::ReadOnly;
    bool has_errored_for_access = false;

    while (At(Tk::Mut)) {
        if (type_access != TypeAccess::ReadOnly && not has_errored_for_access) {
            if (allocate) Error("Only one of 'mut' may be specified for type access modifiers");
            has_errored_for_access = true;
        }

        type_access = TypeAccess::Mutable;
    }

    if (Consume(Tk::Bang)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Error-union types cannot have access modifiers");
        }

        auto value_type = TryParseType(allocate, false);
        Type* error_union_type = nullptr;
        if (allocate) {
            error_union_type = new (*this) ErrUnionType{GetLocation(start), "", *value_type};
        }

        return TryParseTypeContinue(error_union_type, allocate, false);
    }

    if (At(Tk::Var)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to the var type (for now)");
        }

        auto location = tok.location;
        NextToken();

        auto var_type = Result<Type*>::Null();
        if (allocate) {
            var_type = new (*this) InferType{location};
        }

        return *var_type;
        // TODO(local): allow parsing continues for 'var', and disallow them at sema for better errors.
        // return TryParseTypeContinue(*var_type, allocate);
    }

    if (At(Tk::Ident, Tk::ColonColon, Tk::Global)) {
        // these constructors are already wrapped in `if (allocate)` in the TryparseNameOrPath function,
        //  so we don't have to do that explicitly.
        auto NameCtor = [&](Location location, std::string name, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) NameType{location, type_access, CurrScope(), name, template_args};
        };

        auto PathCtor = [&](PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) PathType{path_kind, type_access, CurrScope(), names, locations, template_args};
        };

        auto id_type = TryParseNameOrPath(allocate, NameCtor, PathCtor);
        if (not id_type) return id_type.diag();
        // since the TryParseNameOrPath function works for the expression case, too, we have to
        //  explicitly cast back to a Type* to continue type parsing.
        return TryParseTypeContinue(static_cast<Type*>(*id_type), allocate);
    }

    if (At(Tk::Noreturn)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to the noreturn type");
        }

        auto location = tok.location;
        NextToken();

        auto noreturn_type = Result<Type*>::Null();
        if (allocate) {
            noreturn_type = new (*this) NoreturnType{location};
        }

        return TryParseTypeContinue(*noreturn_type, allocate);
    }

    if (At(Tk::Void)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to the void type");
        }

        auto location = tok.location;
        NextToken();

        auto void_type = Result<Type*>::Null();
        if (allocate) {
            void_type = new (*this) VoidType{location};
        }

        return TryParseTypeContinue(*void_type, allocate);
    }

    if (At(Tk::Bool)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to bool types");
        }

        auto location = tok.location;

        NextToken();

        auto bool_type = Result<Type*>::Null();
        if (allocate) {
            bool_type = new (*this) BoolType{location, 0, true};
        }

        return TryParseTypeContinue(*bool_type, allocate);
    }

    if (At(Tk::BoolSized)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to bool types");
        }

        auto location = tok.location;
        int bit_width = (int) tok.integer_value;
        if (bit_width <= 0 or bit_width > 65535) {
            Error("Primitive type bit width must be in the range (0, 65535]");
        }

        NextToken();

        auto bool_type = Result<Type*>::Null();
        if (allocate) {
            bool_type = new (*this) BoolType{location, bit_width};
        }

        return TryParseTypeContinue(*bool_type, allocate);
    }

    if (At(Tk::Int, Tk::UInt)) {
        auto kw_kind = tok.kind;

        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to integer types");
        }

        auto location = tok.location;

        NextToken();

        auto int_type = Result<Type*>::Null();
        if (allocate) {
            int_type = new (*this) IntType{location, kw_kind == Tk::Int, 0, true};
        }

        return TryParseTypeContinue(*int_type, allocate);
    }

    if (At(Tk::IntSized, Tk::UIntSized)) {
        auto kw_kind = tok.kind;

        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to integer types");
        }

        auto location = tok.location;
        int bit_width = (int) tok.integer_value;
        if (bit_width <= 0 or bit_width > 65535) {
            Error("Primitive type bit width must be in the range (0, 65535]");
        }

        NextToken();

        auto int_type = Result<Type*>::Null();
        if (allocate) {
            int_type = new (*this) IntType{location, kw_kind == Tk::IntSized, bit_width};
        }

        return TryParseTypeContinue(*int_type, allocate);
    }

    if (At(Tk::Float)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to float types");
        }

        auto location = tok.location;

        NextToken();

        auto float_type = Result<Type*>::Null();
        if (allocate) {
            float_type = new (*this) FloatType{location, 0, true};
        }

        return TryParseTypeContinue(*float_type, allocate);
    }

    if (At(Tk::FloatSized)) {
        if (type_access != TypeAccess::ReadOnly) {
            if (allocate) Error("Access modifiers do not apply to float types");
        }

        auto location = tok.location;
        int bit_width = (int) tok.integer_value;
        if (bit_width != 16 and bit_width != 32 and bit_width != 64 and bit_width != 80 and bit_width != 128) {
            Error("Float type bit width must be 16, 32, 64, 80 or 128");
        }

        NextToken();

        auto float_type = Result<Type*>::Null();
        if (allocate) {
            float_type = new (*this) FloatType{location, bit_width};
        }

        return TryParseTypeContinue(*float_type, allocate);
    }

    return Error("Unexpected token when parsing type");
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

        if (not At(Tk::CloseParen)) {
            while (not At(Tk::Eof)) {
                auto arg = ParseExpr();
                if (arg) args.push_back(*arg);

                if (not Consume(Tk::Comma) or At(Tk::CloseParen)) break;
            }
        }

        if (not Consume(Tk::CloseParen)) {
            Error("Expected ')' to close call argument list");
        }

        return ParsePrimaryExprContinue(new (*this) CallExpr{GetLocation(expr->location()), expr, std::move(args)});
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
                Error("Expected ')' to close catch capture");
            }
        }

        auto body = ParseStatement(false);
        if (not body) return body.diag();

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
            type = new (*this) NameType{name_expr->location(), TypeAccess::ReadOnly, name_expr->scope(), name_expr->name(), name_expr->template_args()};
        } else if (auto path_expr = cast<PathExpr>(expr)) {
            type = new (*this) PathType{path_expr->path_kind(), TypeAccess::ReadOnly, path_expr->scope(), path_expr->names(), path_expr->locations(), path_expr->template_args()};
        } else {
            LCC_ASSERT(false, "How did we get here?");
        }

        return new (*this) CtorExpr{location, type, *body};
    }

    return ParsePrimaryExprContinue(expr);
}

auto Parser::ParsePrimaryExpr() -> Result<Expr*> {
    LCC_ASSERT(not IsInSpeculativeParse());

    auto location = CurrLocation();

    if (At(Tk::Ident, Tk::ColonColon, Tk::Global)) {
        auto NameCtor = [&](Location location, std::string name, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) NameExpr{location, CurrScope(), name, template_args};
        };

        auto PathCtor = [&](PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args) -> Expr* {
            return new (*this) PathExpr{path_kind, CurrScope(), names, locations, template_args};
        };

        auto id_expr = TryParseNameOrPath(true, NameCtor, PathCtor);
        if (not id_expr) return id_expr.diag();

        return ParsePrimaryIdentExprContinue(*id_expr);
    } else if (Consume(Tk::Try)) {
        auto try_expr = ParsePrimaryExpr();
        if (not try_expr) return try_expr.diag();
        return new (*this) TryExpr{try_expr->location(), *try_expr};
    } else if (Consume(Tk::Do)) {
        LCC_ASSERT(false, "TODO do (expr)");
    } else if (Consume(Tk::New)) {
        std::vector<Expr*> args{};
        if (Consume(Tk::OpenParen)) {
            if (not At(Tk::CloseParen)) {
                while (not At(Tk::Eof)) {
                    auto arg = ParseExpr();
                    if (arg) args.push_back(*arg);

                    if (not Consume(Tk::Comma) or At(Tk::CloseParen)) break;
                }
            }

            if (not Consume(Tk::CloseParen)) {
                Error("Expected ')' to close call argument list");
            }
        }

        auto type = ParseType();
        auto body = ParseConstructorBody();

        return new (*this) NewExpr{GetLocation(location), args, *type, *body};
    } else if (Consume(Tk::Cast)) {
        Type* cast_type = nullptr;
        if (not Consume(Tk::OpenParen)) {
            Error("Expected '(' to start cast expression");
        } else {
            auto cast_type_result = ParseType();
            if (cast_type_result) cast_type = *cast_type_result;

            if (not Consume(Tk::CloseParen)) {
                Error("Expected ')' to close cast expression type specifier");
            }
        }

        auto cast_value = ParsePrimaryExpr();
        if (not cast_value) return cast_value.diag();

        return new (*this) CastExpr{location, cast_type, *cast_value, CastKind::HardCast};
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
    } else if (Consume(Tk::Ampersand)) {
        auto addr_value = ParsePrimaryExpr();
        if (not addr_value) return addr_value.diag();
        return new (*this) UnaryExpr{location, OperatorKind::Address, *addr_value};
    } else if (Consume(Tk::Star)) {
        auto deref_value = ParsePrimaryExpr();
        if (not deref_value) return deref_value.diag();
        return new (*this) UnaryExpr{location, OperatorKind::Deref, *deref_value};
    } else if (Consume(Tk::OpenParen)) {
        auto expr = ParseExpr();
        if (not expr) return expr.diag();
        if (not Consume(Tk::CloseParen)) {
            Error("Expected ')' to close grouped expression");
        }
        return expr;
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
        if (IsInTemplateParse() and At(Tk::Greater, Tk::GreaterGreater) and IsLocationImmediatelyFollowing())
            break;

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
