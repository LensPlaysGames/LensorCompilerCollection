#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/string_distance.hh>

#include <object/elf.h>
#include <object/elf.hh>

#include <glint/ast.hh>
#include <glint/error_ids.hh>
#include <glint/module_description.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <algorithm>
#include <filesystem>
#include <functional>
#include <iterator>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

auto lcc::glint::Sema::AnalyseAndDiscard(Expr** expr) -> bool {
    LCC_ASSERT(expr);
    if (not Analyse(expr)) return false;
    Discard(expr);
    return true;
}

auto lcc::glint::Sema::DeclTypeDecay(Type* type) -> Type* {
    LCC_ASSERT(type);
    return type->is_function() ? Ptr(type) : type;
}

auto lcc::glint::Sema::Deproceduring(Expr** expr_ptr) -> bool {
    LCC_ASSERT(expr_ptr);

    // fmt::print("Possibly Deproceduring:\n{}", (*expr_ptr)->string(true));

    /// This conversion only applies to functions and function pointers.
    auto* expr = *expr_ptr;
    LCC_ASSERT(expr);

    auto* ty = expr->type();
    LCC_ASSERT(ty);

    if (not ty->is_function()
        and (not ty->is_pointer() or not ty->elem()->is_function()))
        return false;

    /// Declarations are never deprocedured automatically.
    if (is<Decl>(expr)) return false;
    /// Block expressions are never deprocedured automatically.
    if (is<BlockExpr>(expr)) return false;

    /// Functions that take arguments are not affected.
    auto* ftype = cast<FuncType>(ty->is_function() ? ty : ty->elem());
    if (not ftype->params().empty()) return false;

    /// Otherwise, insert a call.
    *expr_ptr = new (mod) CallExpr(expr, {}, expr->location());
    // Whether or not the inserted call is valid, we did insert it, so we
    // return true either way.
    (void) Analyse(expr_ptr);
    return true;
}

void lcc::glint::Sema::Discard(Expr** expr_ptr) {
    LCC_ASSERT(expr_ptr);

    auto* expr = *expr_ptr;
    LCC_ASSERT(expr);

    /// If the expression returns void, or has an error, ignore it.
    if (not expr->ok() or expr->type()->is_void()) return;

    /// If the expression is a call to a function not marked
    /// as discardable, issue an error.
    if (auto* call = cast<CallExpr>(expr)) {
        // TODO: If call is a special form (i.e. a template expansion, or a type
        // instantation), then this does the big bad.
        auto* ftype = call->callee_type();
        if (not ftype->has_attr(FuncAttr::Discardable)) Error(
            call->location(),
            "Discarding return value of function not marked as 'discardable'"
        );
    }

    /// Otherwise, perform deproceduring. For now, we only apply
    /// deproceduring exactly once. If you need more, you can always
    /// use `()` to call the function.
    if (Deproceduring(expr_ptr)) return;

    /// Otherwise, issue a warning if this expression does not have
    /// side effects.
    if (not HasSideEffects(expr)) Warning(
        expr->location(),
        "Expression result unused"
    );
}

auto lcc::glint::Sema::EvaluateAsInt(Expr* expr, Type* int_type, aint& out) -> bool {
    LCC_ASSERT(expr and int_type);

    EvalResult res;
    if (not expr->evaluate(context, res, true)) return false;

    /// Must be an int.
    if (not res.is_int()) {
        Error(expr->location(), "Expression is not an integer constant expression");
        return false;
    }

    /// Print a diagnostic if the thing doesn’t fit.
    bool ok = true;
    auto bits = int_type->size(context);
    aint val = res.as_int();
    auto TooLarge = [&]<typename Int>(auto cb) {
        out = std::invoke(cb, val, bits);
        if (std::invoke(cb, out, 64) != std::invoke(cb, val, 64)) {
            ok = false;
            Error(
                expr->location(),
                "Value {} of integer constant does not fit in an {}",
                Int(val),
                int_type
            );
        }
    };

    /// Check that the value fits in the integer type.
    bool is_signed = int_type->is_signed_int(context);
    LCC_ASSERT(bits <= 64, "Bit width of integer type in constant expression must be 64 or less");
    if (is_signed) utils::invoke_template<i64>(TooLarge, &aint::sext);
    else utils::invoke_template<u64>(TooLarge, &aint::zext);
    return ok;
}

auto lcc::glint::Sema::HasSideEffects(Expr* expr) -> bool {
    LCC_ASSERT(expr);
    switch (expr->kind()) {
        /// These always have side effects.
        case Expr::Kind::Match:
        case Expr::Kind::Switch:
        case Expr::Kind::While:
        case Expr::Kind::For:
        case Expr::Kind::Return:
        case Expr::Kind::Continue:
        case Expr::Kind::Break:
        case Expr::Kind::TypeDecl:
        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::VarDecl:
        case Expr::Kind::FuncDecl:
        case Expr::Kind::TemplatedFuncDecl:
        case Expr::Kind::EnumeratorDecl:
        case Expr::Kind::Apply:
            return true;

        /// These never have side effects.
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::FractionalLiteral:
        case Expr::Kind::StringLiteral:
        case Expr::Kind::OverloadSet:
        case Expr::Kind::NameRef:
        case Expr::Kind::Module:

        case Expr::Kind::Type:
        case Expr::Kind::Sizeof:
        case Expr::Kind::Alignof:
            return false;

        /// For these, it depends.
        case Expr::Kind::Cast:
            return HasSideEffects(as<CastExpr>(expr)->operand());

        case Expr::Kind::Unary:
            return HasSideEffects(as<UnaryExpr>(expr)->operand());

        case Expr::Kind::Template:
            return HasSideEffects(as<TemplateExpr>(expr)->body());

        case Expr::Kind::MemberAccess:
            return HasSideEffects(as<MemberAccessExpr>(expr)->object());

        case Expr::Kind::CompoundLiteral:
            return rgs::any_of(as<CompoundLiteral>(expr)->children(), HasSideEffects);

        case Expr::Kind::Block:
            return rgs::any_of(as<BlockExpr>(expr)->children(), HasSideEffects);

        case Expr::Kind::Group:
            return rgs::any_of(as<GroupExpr>(expr)->expressions(), HasSideEffects);

        case Expr::Kind::EvaluatedConstant: {
            auto* c = as<ConstantExpr>(expr);
            return c->expr() and HasSideEffects(c->expr());
        }

        case Expr::Kind::Binary: {
            auto* b = as<BinaryExpr>(expr);
            if (HasSideEffects(b->lhs()) or HasSideEffects(b->rhs())) return true;
            return b->op() == TokenKind::ColonEq;
        }

        case Expr::Kind::If: {
            auto* i = as<IfExpr>(expr);
            if (HasSideEffects(i->condition()))
                return true;
            if (HasSideEffects(i->then()))
                return true;
            return i->otherwise() and HasSideEffects(i->otherwise());
        }

        case Expr::Kind::Call: {
            auto* c = as<CallExpr>(expr);

            if (HasSideEffects(c->callee())) return true;
            if (rgs::any_of(c->args(), HasSideEffects)) return true;

            // Function calls
            auto* callee_ty = c->callee()->type()->strip_pointers_and_references();
            if (callee_ty->is_function()) {
                auto* f = c->callee_type();
                return not f->has_attr(FuncAttr::Pure) and not f->has_attr(FuncAttr::Const);
            }

            return false;
        }

        case Expr::Kind::IntrinsicCall: {
            auto* c = as<IntrinsicCallExpr>(expr);
            switch (c->intrinsic_kind()) {
                case IntrinsicKind::BuiltinDebugtrap:
                case IntrinsicKind::BuiltinMemCopy:
                case IntrinsicKind::BuiltinMemSet:
                case IntrinsicKind::BuiltinSyscall:
                    return true;

                case IntrinsicKind::BuiltinFilename:
                case IntrinsicKind::BuiltinLine:
                    return false;

                case IntrinsicKind::BuiltinInline:
                    if (c->sema_errored()) return true;
                    return HasSideEffects(c->args()[0]);
            }

            LCC_UNREACHABLE();
        }
    }

    LCC_UNREACHABLE();
}

auto lcc::glint::Sema::ImplicitDereference(Expr** expr) -> bool {
    LCC_ASSERT(expr and *expr);

    (void) Convert__RemoveReferences(expr);

    while (is<PointerType>((*expr)->type())) {
        *expr = new (mod) UnaryExpr(
            TokenKind::Dereference,
            *expr,
            false,
            (*expr)->location()
        );

        LCC_ASSERT(Analyse(expr));
    }

    return (*expr)->is_lvalue();
}

void lcc::glint::Sema::InsertImplicitCast(Expr** expr_ptr, Type* ty) {
    LCC_ASSERT(expr_ptr and *expr_ptr and ty);
    WrapWithCast(expr_ptr, ty, CastKind::ImplicitCast);
}

void lcc::glint::Sema::InsertPointerToIntegerCast(Expr** operand) {
    LCC_ASSERT(operand);
    if ((*operand)->type()->is_pointer())
        InsertImplicitCast(operand, Type::Int);
}

void lcc::glint::Sema::LValueToRValue(Expr** expr, bool strip_ref) {
    LCC_ASSERT(expr and *expr);

    if ((*expr)->sema_errored()) return;

    // This converts the type of a member access of a sum type into the type
    // of the member it is accessing.
    // This matters because when we do something like `bar.x := 69;`, we need
    // to access both the `tag` and `data` of `bar`, so we need the member
    // access of it's member to actually return an lvalue to `bar`, rather
    // than an lvalue to the member itself. But, when we do lvalue to rvalue
    // conversion on this member access, we actually want to access the member
    // itself (and not the value of `bar`), so the type is changed to reflect
    // the fact that we are only accessing the single member (even though we
    // will likely end up accessing the underlying object in order to check
    // that the tag is valid, for example). This is just a reflection of the
    // type of the value this member access expression returns.
    // NOTE: This may not be /exactly/ correct when it comes to the type
    // semantics of the language /iff/ we didn't have ways to know that the
    // underlying object the member access is accessing is of a sum type.
    {
        if (auto* m = cast<MemberAccessExpr>(*expr)) {
            if (auto* s = cast<SumType>(m->type())) {
                auto mindex = m->member();
                // TODO: "1" is actually index of ".data" in underlying struct type.
                m->finalise(s->struct_type(), 1);
                m->type(s->members().at(mindex).type);
            }
        }
    }

    if ((*expr)->is_lvalue())
        WrapWithCast(expr, (*expr)->type(), CastKind::LValueToRValueConv);

    if (strip_ref and is<ReferenceType>((*expr)->type())) {
        WrapWithCast(
            expr,
            as<TypeWithOneElement>((*expr)->type())->element_type(),
            CastKind::ReferenceToLValue
        );

        LValueToRValue(expr);
    }
}

auto lcc::glint::Sema::Ptr(Type* ty) -> PointerType* {
    LCC_ASSERT(ty);

    Type* ptr = new (mod) PointerType(ty, ty->location());
    // If we fail to analyse the type, it is still valid to return as a
    // pointer type (it just will have it's error flag set and won't be able
    // to be used, really).
    (void) Analyse(&ptr);
    return as<PointerType>(ptr);
}

auto lcc::glint::Sema::Ref(Type* ty) -> ReferenceType* {
    LCC_ASSERT(ty);

    Type* ref = new (mod) ReferenceType(ty, ty->location());
    // If we fail to analyse the type, it is still valid to return as a
    // pointer type (it just will have it's error flag set and won't be able
    // to be used, really).
    (void) Analyse(&ref);
    return as<ReferenceType>(ref);
}

void lcc::glint::Sema::WrapWithCast(Expr** expr_ptr, Type* type, CastKind kind) {
    LCC_ASSERT(expr_ptr and *expr_ptr and type);

    Expr* expr = new (mod) CastExpr(
        *expr_ptr,
        type,
        kind,
        (*expr_ptr)->location()
    );

    if (not Analyse(&expr))
        Diag::ICE("Glint Semantic Analysis failed to wrap expression with cast");

    *expr_ptr = expr;
}

/// ===========================================================================
///  Core
/// ===========================================================================
void lcc::glint::Sema::Analyse(Context* ctx, Module& m, bool use_colours) {
    LCC_ASSERT(ctx);
    if (ctx->has_error()) return;
    Sema s{ctx, m, use_colours};
    return s.AnalyseModule();
}

auto lcc::glint::Sema::try_get_metadata_blob_from_object(
    Module::Ref& import_ref,
    const std::string& include_dir,
    std::vector<std::string>& paths_tried
) -> bool {
    auto path_base0
        = include_dir
        + std::filesystem::path::preferred_separator
        + import_ref.name;
    auto path_base1
        = include_dir
        + std::filesystem::path::preferred_separator
        + "lib"
        + import_ref.name;
    auto paths = {
        path_base0 + ".o",
        path_base0 + ".obj",
        path_base0 + ".a",
        path_base1 + ".o",
        path_base1 + ".obj",
        path_base1 + ".a",
    };
    for (auto p : paths) {
        paths_tried.push_back(p);
        if (std::filesystem::exists(p)) {
            if (context->has_option("verbose"))
                fmt::print("Found IMPORT {} at {}\n", import_ref.name, p);
            // Open file, get contents
            auto object_file = File::Read(p);

            LCC_ASSERT(
                not object_file.empty(),
                "Found object file for module {} at {}, but the file is empty",
                import_ref.name,
                p
            );

            // Determine file-type via magic bytes or extension
            std::vector<u8> metadata_blob{};
            if (
                object_file.size() >= sizeof(elf64_header)
                and object_file.at(0) == 0x7f and object_file.at(1) == 'E'
                and object_file.at(2) == 'L' and object_file.at(3) == 'F'
            ) {
                auto section = elf::get_section_from_blob(
                    object_file,
                    metadata_section_name
                );
                metadata_blob = std::move(section.contents());
            } else LCC_ASSERT(
                false,
                "Unrecognized file format of module {} at {}",
                import_ref.name,
                p
            );
            // Very basic validation pass
            LCC_ASSERT(
                not metadata_blob.empty(),
                "Didn't properly get metadata (it's empty) for module {} at {}",
                import_ref.name,
                p
            );
            LCC_ASSERT(
                metadata_blob.at(0) == ModuleDescription::default_version
                    and metadata_blob.at(1) == ModuleDescription::magic_byte0
                    and metadata_blob.at(2) == ModuleDescription::magic_byte1
                    and metadata_blob.at(3) == ModuleDescription::magic_byte2,
                "Metadata for module {} at {} has invalid magic bytes",
                import_ref.name,
                p
            );
            // Deserialise metadata blob into a newly created shell of a module.
            auto imported_mod = new Module(
                nullptr,
                import_ref.name,
                Module::IsAModule
            );
            import_ref.module = imported_mod;
            // "Copy" global scope to imported module, since they will access the same
            // globals in the same program.
            imported_mod->scopes.emplace_back(mod.global_scope());
            return imported_mod->deserialise(context, metadata_blob);
        }
    }
    return false;
}

auto lcc::glint::Sema::try_get_metadata_blob_from_gmeta(
    Module::Ref& import_ref,
    const std::string& include_dir,
    std::vector<std::string>& paths_tried
) -> bool {
    auto path = include_dir
              + std::filesystem::path::preferred_separator
              + import_ref.name + std::string(metadata_file_extension);

    paths_tried.push_back(path);
    if (std::filesystem::exists(path)) {
        if (context->has_option("verbose"))
            fmt::print("Found IMPORT {} at {}\n", import_ref.name, path);

        // Open file, get contents
        auto gmeta_file = File::Read(path);

        std::vector<u8> metadata_blob{};
        metadata_blob.insert(metadata_blob.end(), gmeta_file.begin(), gmeta_file.end());

        LCC_ASSERT(
            not metadata_blob.empty(),
            "Found gmeta file for module {} at {}, but the file is empty",
            import_ref.name,
            path
        );
        LCC_ASSERT(
            metadata_blob.at(0) == ModuleDescription::default_version
                and metadata_blob.at(1) == ModuleDescription::magic_byte0
                and metadata_blob.at(2) == ModuleDescription::magic_byte1
                and metadata_blob.at(3) == ModuleDescription::magic_byte2,
            "Metadata for module {} at {} has invalid magic bytes",
            import_ref.name,
            path
        );
        // Deserialise metadata blob into a newly created shell of a module.
        auto imported_mod = new Module(
            nullptr,
            import_ref.name,
            Module::IsAModule
        );
        // import_ref now "owns" this module...
        import_ref.module = imported_mod;
        // "Copy" global scope to imported module, since they will access the same
        // globals in the same program.
        imported_mod->scopes.emplace_back(mod.global_scope());
        return imported_mod->deserialise(context, metadata_blob);
    }

    return false;
}

auto lcc::glint::Sema::try_get_metadata_blob_from_assembly(
    Module::Ref& import_ref,
    const std::string& include_dir,
    std::vector<std::string>& paths_tried
) -> bool {
    auto path = include_dir
              + std::filesystem::path::preferred_separator
              + import_ref.name + ".s";

    paths_tried.push_back(path);
    if (std::filesystem::exists(path)) {
        // TODO: We can kind of cheat and just direct seek to `.section .glint`,
        // then `.byte`, then parse the whole line as comma-separated integer
        // literals forming a stream of bytes.
        LCC_TODO("Parse Glint module metadata from assembly file (alternatively, provide a gmeta or object file)");
    }
    return false;
}

void lcc::glint::Sema::DeclareImportedGlobalFunction(
    std::string name,
    Type* return_ty,
    std::vector<FuncType::Param> param_ty,
    bool no_return
) {
    LCC_ASSERT(return_ty);
    (void) mod.global_scope()->declare(
        context,
        std::move(name),
        new (mod) FuncDecl(
            name,
            new (mod) FuncType(
                param_ty,
                return_ty,
                {{FuncAttr::NoMangle, true}, {FuncAttr::NoReturn, no_return}},
                {}
            ),
            nullptr,
            mod.global_scope(),
            &mod,
            Linkage::Imported,
            {},
            CallConv::C
        )
    );
}

auto lcc::glint::Sema::apply_template(
    std::string template_source,
    std::vector<Expr*> template_arguments
) -> Expr* {
    auto& f = context->create_file(
        "sema_apply_template.g",
        std::vector<char>{
            template_source.begin(),
            template_source.end()
        }
    );

    auto templates_m = glint::Parser::ParseFreestanding(
        mod,
        context,
        f,
        mod.top_level_scope()
    );
    if (not templates_m) {
        if (templates_m.is_diag())
            templates_m.diag().print();
        Diag::ICE("GlintSema failed to parse semantic template");
    }

    if (templates_m->size() != 1) {
        for (auto e : *templates_m)
            e->print(context->option_use_colour());

        Diag::ICE(
            "Malformed template source passed to sema apply_template: expected ONE unnamed template expression (got {} expressions)\n  | {}",
            templates_m->size(),
            template_source
        );
    }
    auto template_ = templates_m->at(0);
    if (not is<TemplateExpr>(template_)) {
        template_->print(context->option_use_colour());
        Diag::ICE(
            "Malformed template source passed to sema apply_template: expected one unnamed TEMPLATE expression\n  | {}",
            template_source
        );
    }

    return new (mod) CallExpr(template_, template_arguments, {});
};

void lcc::glint::Sema::AnalyseModule() {
    // Load imported modules.
    // Don't load imported modules twice.
    std::unordered_set<Module*> loaded_modules{};
    for (auto& import_ref : mod.imports()) {
        if (loaded_modules.contains(import_ref.module))
            continue;

        bool loaded{false};
        std::vector<std::string> paths_tried{};

        for (const auto& include_dir : context->include_directories()) {
            loaded = try_get_metadata_blob_from_gmeta(import_ref, include_dir, paths_tried)
                  or try_get_metadata_blob_from_object(import_ref, include_dir, paths_tried)
                  or try_get_metadata_blob_from_assembly(import_ref, include_dir, paths_tried);
            if (loaded) break;
        }

        if (not loaded) {
            // TODO: Link/reference help documentation on how to point the compiler to
            // look in the proper place for Glint metadata, and how to produce it.
            Error(
                import_ref.location,
                "Could not find imported module {} in any include directory.\n"
                "Working Directory: {}\n"
                "Paths tried:\n"
                "{}",
                import_ref.name,
                std::filesystem::current_path().lexically_normal().string(),
                fmt::join(paths_tried, "\n")
            );
            std::exit(1);
        }

        LCC_ASSERT(import_ref.module);
        loaded_modules.emplace(import_ref.module);

        // Add imported functions from imported module to our list of functions as
        // imported... I'll say imported one more time just so you don't get confused.
        for (auto f : import_ref.module->functions()) {
            if (IsImportedLinkage(f->linkage()))
                mod.add_function(f);
        }

        // Insert call to init function of imported module.
        auto call_init = new (mod) CallExpr(
            new (mod) NameRefExpr(
                Module::InitFunctionName(import_ref.name),
                mod.global_scope(),
                {}
            ),
            {},
            {}
        );
        as<BlockExpr>(mod.top_level_function()->body())
            ->children()
            .insert(
                as<BlockExpr>(mod.top_level_function()->body())->children().begin(),
                call_init
            );
    }

    // Parse templates that sema will use to expand and/or rewrite things
    // (that way we don't have to create large, branching AST structures in
    // code).
    {
        // TODO: Once we use C++26, just use #embed
        // TODO: It'd be really convenient to have a way to tell the Glint parser
        // to "obfuscate" all identifiers encountered in a source file (such that
        // you can write a standard library without having to riddle everything
        // with double underscores).
        std::string_view templates_source =
            // Initialise a dynamic array expression with the given capacity.
            "__dynarray_init :: template(dynarray : expr, capacity : expr) {\n"
            "  dynarray.capacity := capacity;\n"
            "  dynarray.size := 0;\n"
            "  dynarray.data := malloc (capacity (sizeof @dynarray.data))\n"
            "};\n"

            "__dynarray_grow :: template(dynarray : expr) {\n"
            "  if dynarray.size >= dynarray.capacity - 1, {\n"
            //   Allocate memory, capacity *\ 2
            //   NOTE: Shouldn't have to put parens around the arguments, but there is
            //   currently a bug in the parser where a "single expression" doesn't
            //   include a binary expression or something like that.
            "    newmem :: malloc (2 dynarray.capacity);\n"
            //   Copy <size> elements into newly-allocated memory
            "    memcpy newmem, dynarray.data, dynarray.size;\n"
            //   De-allocate old memory
            "    free dynarray.data;\n"
            //   Assign dynarray.data to newly-allocated memory
            //   dynarray.data <- newmem;
            "    dynarray.data := (typeof dynarray.data) newmem;\n"
            //   Assign dynarray.capacity to dynarray.capacity * 2
            "    dynarray.capacity *= 2;\n"
            "  };\n"
            "};\n"
            "__putchar_each :: template(container : expr, size : uint)\n"
            "  cfor\n"
            "      __i_ii :: 0;\n"
            "      __i_ii < size;\n"
            "      __i_ii += 1;\n"
            "    putchar @container[__i_ii];\n"
            "__print__putchar_each :: template(container : expr)\n"
            "  cfor\n"
            "      __i_ii :: 0;\n"
            "      __i_ii < container.size;\n"
            "      __i_ii += 1;\n"
            "    putchar @container[__i_ii];\n";

        auto& f = context->create_file(
            "sema_templates.g",
            std::vector<char>{templates_source.begin(), templates_source.end()}
        );

        auto templates_m = glint::Parser::ParseFreestanding(
            mod,
            context,
            f,
            new (mod) Scope(mod.top_level_scope())
        );
        if (not templates_m) {
            if (templates_m.is_diag())
                templates_m.diag().print();
            Diag::ICE("GlintSema failed to parse semantic templates");
        }

        for (auto c : *templates_m) {
            LCC_ASSERT(
                is<VarDecl>(c),
                "Malformed sema_templates.g: expected named template as top level expression"
            );
            auto v = as<VarDecl>(c);
            LCC_ASSERT(is<TemplateExpr>(v->init()), "Malformed sema_templates.g: expected named template...");

            sema_templates.emplace_back(v->name(), as<TemplateExpr>(v->init()));
        }
    }
    // Register functions that may be called by expressions inserted by
    // semantic analysis HERE. The reason we have to do this now and not each
    // time it's needed is because it may cause iterator invalidation if
    // analysing the functions causes a function to be added to the list.

    // Glint's `print` requires these...
    DeclareImportedGlobalFunction(
        "puts",
        Type::Void,
        {{"ptr", Type::VoidPtr, {}}}
    );
    DeclareImportedGlobalFunction(
        "putchar",
        Type::Void,
        {{"c", FFIType::CInt(mod), {}}}
    );

    // Dynamic array operations require these...
    DeclareImportedGlobalFunction(
        "malloc",
        Type::VoidPtr,
        {{"size", FFIType::CInt(mod), {}}}
    );
    DeclareImportedGlobalFunction(
        "free",
        Type::Void,
        {{"ptr", Type::VoidPtr, {}}}
    );
    DeclareImportedGlobalFunction(
        "exit",
        Type::Void,
        {{"status", Type::Int, {}}},
        true
    );
    DeclareImportedGlobalFunction(
        "memcpy",
        Type::Void,
        {{"dest", Type::VoidPtr, {}},
         {"src", Type::VoidPtr, {}},
         {"size", FFIType::CInt(mod), {}}}
    );
    DeclareImportedGlobalFunction(
        "memset",
        Type::Void,
        {{"ptr", Type::VoidPtr, {}},
         {"value", FFIType::CInt(mod), {}},
         {"size", FFIType::CULongLong(mod), {}}}
    );
    DeclareImportedGlobalFunction(
        "memmove",
        Type::Void,
        {{"dest", Type::VoidPtr, {}},
         {"src", Type::VoidPtr, {}},
         {"size", FFIType::CInt(mod), {}}}
    );

    {
        std::string_view builtin_formatters =
            "format : [byte](__x : int) {\n"
            "  __out : [byte];\n"
            "  if __x = 0, __out += `0`;\n"
            "  __negative :: __x < 0;\n"
            "  ;; Make x positive\n"
            "  if __negative, __x := -__x;\n"
            "  while __x, {\n"
            "    __out ~= byte __x % 10 + `0`;\n"
            "    __x /= 10;\n"
            "  };\n"
            "  if __negative, __out ~= `-`;\n"
            "  __out;\n"
            "};\n"
            "format : [byte](__x : uint) {\n"
            "  __out : [byte];\n"
            "  if __x = 0, __out += `0`;\n"
            "  while __x, {\n"
            "    __out ~= byte __x % 10 + `0`;\n"
            "    __x /= 10;\n"
            "  };\n"
            "  __out;\n"
            "};\n";

        auto& formatters_source = context->create_file(
            "builtin_formatters.g",
            std::vector<char>{builtin_formatters.begin(), builtin_formatters.end()}
        );

        auto formatters_module = glint::Parser::ParseFreestanding(
            mod,
            context,
            formatters_source,
            mod.top_level_scope()
        );
        if (not formatters_module) {
            if (formatters_module.is_diag())
                formatters_module.diag().print();
            Diag::ICE("GlintSema failed to parse builtin formatters");
        }

        for (auto c : *formatters_module) {
            LCC_ASSERT(
                is<FuncDecl>(c),
                "Malformed builtin_formatters.g: expected function declaration as top level expression"
            );
            auto function = as<FuncDecl>(c);
            LCC_ASSERT(
                function->name() == "format",
                "builtin_formatters.g should be used to define format() overloads"
            );
        }
    }

    // Analyse the signatures of all functions. This must be done
    // before analysing bodies since, in order to perform overload
    // resolution properly, we first need to apply decltype decay
    // to all parameters (e.g. convert parameters of function type
    // to function pointers etc.).
    for (auto& func : mod.functions())
        AnalyseFunctionSignature(func);

    // Analyse function bodies.
    // NOTE: Sema may create new functions, so we have to be very careful of
    // iterator invalidation here.
    for (usz function_index = 0; function_index < mod.functions().size(); ++function_index)
        AnalyseFunctionBody(mod.functions().at(function_index));

    // TODO: Remove unused, _external_ functions.
}

void lcc::glint::Sema::AnalyseFunctionBody(FuncDecl* decl) {
    LCC_ASSERT(decl);

    // fmt::print(
    //     "Analysing body of function {} : {}\n{}\n",
    //     decl->name(),
    //     *decl->type(),
    //     decl->string(false)
    // );

    tempset curr_func = decl;
    auto* ty = decl->function_type();

    // If the function has no body, then we’re done.
    if (not decl->body()) return;

    // If the function is templated, then we’re done (unexpanded body is not
    // checked).
    if (is<TemplatedFuncDecl>(decl)) return;

    // Create variable declarations for the parameters.
    bool params_failed{false};
    for (auto& param : ty->params()) {
        if (param.name.empty()) continue;

        // Check that we don’t already have a declaration with that
        // name in the function scope.
        auto decls = decl->scope()->find(param.name);
        if (not decls.empty()) {
            Error(
                decls.at(0)->location(),
                "Declaration conflicts with parameter name"
            );
            Diag::Note(
                context,
                param.location,
                "Parameter declared here"
            );
            continue;
        }

        /// Declare the parameter.
        Expr* d = new (mod) VarDecl(
            param.name,
            param.type,
            {},
            &mod,
            Linkage::LocalVar,
            param.location
        );
        Expr* dd{d};

        auto declared = decl->scope()->declare(
            context,
            auto(param.name),
            as<VarDecl>(d)
        );
        if (not declared)
            declared.diag().print();
        LCC_ASSERT(declared);
        if (not Analyse(&d)) {
            // Continue to analyse the rest of the parameters
            params_failed = true;
        }
        // NOTE: `dd` copy important, since Analyse() clobbers parameter.
        decl->param_decls().push_back(as<VarDecl>(dd));
    }

    // Gets rid of parameter dynamic array declarations that were falsely
    // recorded as dangling (parameters owned by caller).
    // TODO: Probably should disallow dynamic array parameters entirely? I
    // don't really see when it would be needed. I can see a reference to one
    // making sense, but receiving a copy of a dynamic array for you to mess
    // about with just seems weird to me.
    decl->dangling_dynarrays().clear();

    // If any of the parameters failed to type-check, the body will likely be
    // full of uses of the parameters, and those will all cause a whole bunch
    // of errors that won't be relevant once the programmer fixes the error in
    // the parameter declaration(s).
    if (params_failed) return;

    /// Analyse the body.
    // If we fail to analyse the function body, then the function is ill-
    // formed, and there's no need to continue checking the declaration.
    if (not Analyse(&decl->body(), ty->return_type()))
        return;

    /// The last expression in a function must be a return expression or convertible
    /// to the return type of the function. If it is a return expression, then it has
    /// already been checked for that, so ignore that case.
    ///
    /// Note that the body may be a block, in which case we should check the last
    /// expression of the block rather than just the block itself.
    if (not ty->return_type()->is_void()) {
        Expr** last{};

        // Get last expression in function.
        if (auto* block = cast<BlockExpr>(decl->body())) {
            if (not block->children().empty())
                last = block->last_expr();
        } else last = &decl->body();

        // If there is no "last" expression, then there is no expression;
        // This is most often the case for programs in-development/scaffolding for
        // future implementations. We choose to emit a warning in this case.
        // If there was no last expression, then it is an ill-formed function---
        // unless this is the top-level function, in which case we will insert a
        // valid return value.
        if (not last) {
            if (decl != mod.top_level_function() and decl->name() != "main") {
                Warning(
                    decl->location(),
                    "No expression in body of function {}",
                    decl->name()
                );
                return;
            }
            auto* inserted_return_value = new (mod) IntegerLiteral(0, {});
            decl->body() = new (mod) ReturnExpr(inserted_return_value, {});
            last = &decl->body();
            (void) Analyse(last);
        }

        // We have a return expression, hurray!
        if (auto* ret = cast<ReturnExpr>(*last)) {
            LCC_ASSERT(
                TryConvert(&ret->value(), ty->return_type()).noop(),
                "Last expression may be a return expression, sure, but the expression it's returning is not convertible to the return type!"
            );
            return;
        }

        // If the last expression is not a return expression and the type of the
        // last expression is convertible to the return type, insert a return
        // expression that returns the converted last expression.
        // FIXME: Probably a bug elsewhere, but last type can be void after
        // conversion if deproceduring happens to a function that returns void.
        if (
            TryConvert(last, ty->return_type()).possible()
            and not (*last)->type()->is_void()
        ) {
            // If it works, apply it.
            LCC_ASSERT(Convert(last, ty->return_type()));

            if (is<BlockExpr>(decl->body()))
                *last = new (mod) ReturnExpr(*last, {});
            else decl->body() = new (mod) ReturnExpr(*last, {});

            // Analyse inserted return
            if (not Analyse(last)) {
                {
                    Error(
                        (*last)->location(),
                        "Inserted return failed semantic analysis (probably a compiler error)"
                    );
                }
                Diag::ICE("Inserted return failed semantic analysis (probably a compiler error)");
            }

            return;
        }
        // Otherwise, if the last expression is not a return expression and the
        // type of that last expression is not convertible to the return type of
        // the function, the program is ill-formed (type of return expression does
        // not match return type of enclosing function)---unless this is the top-
        // level function, in which case we will insert a valid return value.
        if (decl != mod.top_level_function() and decl->name() != "main") {
            Error(
                (*last)->location(),
                "Type of last expression {}, which is being implicitly returned, is not convertible to return type {}",
                (*last)->type(),
                ty->return_type()
            );
            // FIXME: For some reason, declarations are set as State::Done already.
            // decl->set_sema_errored();
            return;
        }

        // TODO: If the last expression is a pointer to a convertible type, emit
        // error regarding missing dereference...?
        // Would catch situations where someone is trying to return bar[0], and
        // means to return @bar[0]...

        // insert "return 0;"
        auto* inserted_return_value = new (mod) IntegerLiteral(0, {});
        auto* inserted_return = new (mod) ReturnExpr(inserted_return_value, {});
        if (auto* b = cast<BlockExpr>(decl->body())) {
            b->add(inserted_return);
            last = b->last_expr();
        } else {
            // FIXME: Should this be an error instead of replacing the body?
            decl->body() = inserted_return;
            last = &decl->body();
        }
    } else {
        // else: return type is void

        if (auto* block = cast<BlockExpr>(decl->body())) {
            if (block->children().empty() or not is<ReturnExpr>(*block->last_expr()))
                block->add(new (mod) ReturnExpr(nullptr, {}));
        } else {
            // If a function with void return type and a non-block body
            // (i.e. `foo : void() = bar 42;`) does not have a return expression, we
            // must replace the body with a block containing the non-block body
            // followed by an empty return expression.
            if (not is<ReturnExpr>(decl->body())) {
                decl->body() = new (mod) GroupExpr(
                    {decl->body(),
                     new (mod) ReturnExpr(nullptr, {})},
                    {}
                );
            }
        }

        Discard(&decl->body());
    }

    // Report every dynamic array declared in this function (and that is not
    // returned) which doesn't have NoLongerViable status (aka freed).
    // Parameters are owned by caller, don't count those.
    // NOTE: We must do this /after/ the last expression has been implicitly
    // returned, if that is going to happen. Otherwise, if we implicitly
    // return a dynamic array, and we do the check before we insert the
    // implicit return, we will falsely get an error that it is never
    // freed (when in reality it's ownership is passed to the call-site).
    for (auto* dynarray : decl->dangling_dynarrays()) {
        // Modules that export dynamic arrays should not get this error.
        if (IsExportedLinkage(decl->linkage()))
            continue;

        Error(
            dynarray->location(),
            "You forgot to free this dynamic array"
        );
    }
}

void lcc::glint::Sema::AnalyseFunctionSignature(FuncDecl* decl) {
    LCC_ASSERT(decl);

    /// Set a name for the decl if it’s empty.
    if (decl->name().empty())
        decl->name(mod.unique_name("emptydecl_"));

    /// Typecheck the function type.
    if (not Analyse(decl->type_ref())) {
        decl->set_sema_errored();
        return;
    }

    /// Used attribute is ignored on functions that aren’t internal. If
    /// the function is internal, then set the linkage to used so it isn’t
    /// deleted by the optimiser.
    auto* ty = as<FuncType>(decl->type());
    if (ty->has_attr(FuncAttr::Used)) {
        if (decl->linkage() != Linkage::Internal)
            Warning(decl->location(), "'used' has no effect on this function");
        else decl->linkage(Linkage::Used);
    }
}

auto lcc::glint::Sema::DefaultInitializeImpl(Expr* accessor, ZeroInitializeOption do_zero) -> Expr* {
    LCC_ASSERT(accessor);
    LCC_ASSERT(Analyse(&accessor));
    LCC_ASSERT(accessor->ok());

    switch (accessor->type()->kind()) {
        case Type::Kind::Array:
        case Type::Kind::ArrayView:
        case Type::Kind::Function:
        case Type::Kind::Sum:
        case Type::Kind::Union:
        case Type::Kind::Enum:
        case Type::Kind::Integer:
        case Type::Kind::Builtin:
        case Type::Kind::FFIType:
        case Type::Kind::Pointer: {
            if (do_zero) {
                return apply_template(
                    "template(x : expr) { memset (&x), 0, (sizeof x) / 8; };",
                    {accessor}
                );
            }
            return nullptr;
        }

        case Type::Kind::DynamicArray: {
            auto dynarray_init =
                "template(dynarray : expr, capacity : expr) {"
                "  dynarray.capacity := capacity;"
                "  dynarray.size := 0;"
                "  dynarray.data := (typeof dynarray.data) (malloc (capacity ((sizeof @dynarray.data) / 8)));"
                "};";

            constexpr usz default_dynamic_array_capacity = 8;
            return apply_template(
                dynarray_init,
                {accessor,
                 new (mod) IntegerLiteral(default_dynamic_array_capacity, {})}
            );
        };

        case Type::Kind::Struct: {
            auto s = as<StructType>(accessor->type());

            std::vector<Expr*> exprs{};

            if (do_zero) {
                auto zero_init = apply_template(
                    "template(x : expr) { memset (&x), 0, (sizeof x) / 8; };",
                    {accessor}
                );
                exprs.emplace_back(zero_init);
            }

            for (auto m : s->members()) {
                // TODO: If member needs default-initialized that is NOT zero
                // initialization, add that expression to exprs...
                auto member_accessor = new (mod) MemberAccessExpr(accessor, m.name, {});
                LCC_ASSERT(
                    Analyse((Expr**) &member_accessor)
                );
                auto member_init = DefaultInitializeImpl(
                    member_accessor,
                    DontPerformZeroInitialization
                );
                if (member_init) exprs.emplace_back(member_init);
                // FIXME: If memory usage becomes a large problem (I suspect it won't),
                // then we could free the accessor, if it isn't used. Right now, it is
                // stored within the module's nodes vector until the module is deleted.
            }

            // Don't create an empty block expression in the event that we are not
            // doing zero initialization and no members required initializing.
            if (exprs.empty()) return nullptr;

            // Don't create a needless block expression.
            if (exprs.size() == 1) return exprs.at(0);

            return new (mod) BlockExpr(exprs, {});
        };

        case Type::Kind::Reference:
            Diag::ICE("Cannot default-initialize a reference type");

        case Type::Kind::TemplatedStruct:
        case Type::Kind::Named:
        case Type::Kind::Typeof:
        case Type::Kind::Type:
            LCC_ASSERT(false, "Type {} should have been replaced...", *accessor->type());
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::Sema::DefaultInitialize(VarDecl* decl) -> Expr* {
    LCC_ASSERT(decl);
    LCC_ASSERT(
        not decl->init(),
        "Cannot default initialize a variable that already has an initialiser"
    );
    LCC_ASSERT(
        not IsImportedLinkage(decl->linkage()),
        "Cannot default initialize an imported variable"
    );

    return DefaultInitializeImpl(
        DeclReference(decl),
        PerformZeroInitialization
    );
}

auto lcc::glint::Sema::DeclReference(Decl* decl) -> Expr* {
    LCC_ASSERT(decl);
    return new (mod) NameRefExpr(
        decl->name(),
        decl->scope(),
        decl->location()
    );
}

auto lcc::glint::Sema::AnalyseLoop(Loop* l) -> bool {
    LCC_ASSERT(l);

    auto cond_result = Analyse(&l->condition());
    if (not cond_result) {
        l->set_sema_errored();
        return false;
    }

    if (not Convert(&l->condition(), Type::Bool)) {
        Error(
            l->location(),
            "Invalid type for loop condition: {}",
            l->condition()->type()
        );
        l->set_sema_errored();
        return false;
    }
    LValueToRValue(&l->condition());
    if (not AnalyseAndDiscard(&l->body())) {
        l->set_sema_errored();
        return false;
    }

    return true;
}

/// ===========================================================================
///  Analysing Expressions
/// ===========================================================================
/// Invariants:
///
///   - If an expression is marked as `Done` or `Errored`, it will
///     not be analysed again.
///
///   - If an expression is a `TypedExpr`, its type is analysed first.
///
///   - When this function returns, the expression pointed to by
///     `expr_pointer` will be marked as `Done`, unless it is already
///     marked as `Errored`. This may not end up being the same
///     expression as `expr` in the body of this function.
///
/// \param expr_ptr A pointer to the expression to analyse.
/// \param expected_type The type used for top-down inference. May be null.
/// \return (*expr_ptr)->ok().
auto lcc::glint::Sema::Analyse(Expr** expr_ptr, Type* expected_type) -> bool {
    LCC_ASSERT(expr_ptr);

    auto* expr = *expr_ptr;
    LCC_ASSERT(expr);

    /// Don’t analyse the same expression twice.
    if (expr->sema() != SemaNode::State::NotAnalysed)
        return expr->ok();

    expr->set_sema_in_progress();

    /// Analyse the type if there is one.
    if (auto* tc = cast<TypedExpr>(expr)) {
        if (not Analyse(tc->type_ref()))
            return false;
    }

    /// Analyse the expression itself.
    switch (expr->kind()) {
        /// The condition of a loop must be convertible to bool.
        case Expr::Kind::For: {
            auto* f = as<ForExpr>(expr);

            within_loops.emplace_back(f);

            if (not AnalyseAndDiscard(&f->init())) {
                expr->set_sema_errored();
                return false;
            }
            if (not AnalyseAndDiscard(&f->increment())) {
                expr->set_sema_errored();
                return false;
            }
            if (not AnalyseLoop(as<Loop>(expr)))
                return false;

            within_loops.pop_back();
        } break;

        case Expr::Kind::While: {
            within_loops.emplace_back(expr);

            if (not AnalyseLoop(as<Loop>(expr)))
                return false;

            within_loops.pop_back();
        } break;

        case Expr::Kind::Break: {
            auto* br = as<BreakExpr>(expr);

            if (within_loops.empty()) {
                Error(br->location(), "`break` not within any loop");
                br->set_sema_errored();
                return false;
            }

            // Resolve break target

            // If no target specified, get containing loops. If there is more than one
            // containing loop, it is a program error.
            if (not br->target()) {
                if (within_loops.size() != 1) {
                    Error(
                        br->location(),
                        "`break` without a specified target within multiple loops;"
                        " specify a target by naming a loop or using an integer denoting how many loops to break out of"
                    );
                    br->set_sema_errored();
                    return false;
                }
                br->target() = within_loops.back();
                break;
            }

            // Otherwise, if a target is specified, ensure it is a valid target.
            if (is<NameRefExpr>(br->target())) {
                auto n = as<NameRefExpr>(br->target());
                LCC_TODO(
                    "Find containing loop named {}",
                    n->name()
                );
            }

            if (is<IntegerLiteral>(br->target())) {
                auto i = as<IntegerLiteral>(br->target());
                auto loop_position = i->value().value();
                auto loop_index = loop_position - 1;
                if (loop_index >= within_loops.size()) {
                    Error(
                        br->target()->location(),
                        "Integer is out of range (min 1, max {})",
                        within_loops.size()
                    );
                    br->set_sema_errored();
                    return false;
                }
                br->target() = within_loops.at(usz(loop_index));
                break;
            }

            Error(
                expr->location(),
                "Break target must be unspecified, an identifier, or an integer."
            );
        } break;

        // TODO: Deduplicate with `break`
        case Expr::Kind::Continue: {
            auto* br = as<ContinueExpr>(expr);

            if (within_loops.empty()) {
                Error(br->location(), "`continue` not within any loop");
                br->set_sema_errored();
                return false;
            }

            // Resolve break target

            // If no target specified, get containing loops. If there is more than one
            // containing loop, it is a program error.
            if (not br->target()) {
                if (within_loops.size() != 1) {
                    Error(
                        br->location(),
                        "`break` without a specified target within multiple loops;"
                        " specify a target by naming a loop or using an integer denoting how many loops to break out of"
                    );
                    br->set_sema_errored();
                    return false;
                }
                br->target() = within_loops.back();
                break;
            }

            // Otherwise, if a target is specified, ensure it is a valid target.
            if (is<NameRefExpr>(br->target())) {
                auto n = as<NameRefExpr>(br->target());
                LCC_TODO(
                    "Find containing loop named {}",
                    n->name()
                );
            }

            if (is<IntegerLiteral>(br->target())) {
                auto i = as<IntegerLiteral>(br->target());
                auto loop_position = i->value().value();
                auto loop_index = loop_position - 1;
                if (loop_index >= within_loops.size()) {
                    Error(
                        br->target()->location(),
                        "Integer is out of range (min 1, max {})",
                        within_loops.size()
                    );
                    br->set_sema_errored();
                    return false;
                }
                br->target() = within_loops.at(usz(loop_index));
                break;
            }

            Error(
                expr->location(),
                "Break target must be unspecified, an identifier, or an integer."
            );
        } break;

        case Expr::Kind::Apply: {
            auto* apply = as<ApplyExpr>(expr);

            if (not IsCallable(apply->function())) {
                Error(
                    apply->function()->location(),
                    "Expression given to 'apply' expression must be callable!"
                );
                return false;
            }

            // Ensure that all argument lists are the same length.
            usz encountered_size{0};
            bool have_encountered_size{false};
            for (auto arglist : apply->argument_lists()) {
                if (auto g = cast<GroupExpr>(arglist)) {
                    LCC_ASSERT(not g->expressions().empty());
                    if (have_encountered_size and g->expressions().size() != encountered_size) {
                        Error(
                            g->location(),
                            "Mismatched lengths of argument lists given to 'apply'."
                        );
                        return false;
                    }
                    have_encountered_size = true;
                    encountered_size = g->expressions().size();
                } else {
                    if (have_encountered_size and encountered_size != 1) {
                        Error(
                            arglist->location(),
                            "Expected a list of {} arguments in each argument list given to 'apply', but got a single expression.",
                            encountered_size
                        );
                        return false;
                    } else {
                        have_encountered_size = true;
                        encountered_size = 1;
                    }
                }
            }

            std::vector<Expr*> generated_calls{};
            for (usz index = 0; index < encountered_size; ++index) {
                std::vector<Expr*> call_arguments{};

                // Fetch 'index'th expression from argument list.
                for (auto arglist : apply->argument_lists()) {
                    if (not is<GroupExpr>(arglist)) {
                        LCC_ASSERT(encountered_size == 1);
                        call_arguments.emplace_back(arglist);
                        continue;
                    }
                    call_arguments.emplace_back(
                        as<GroupExpr>(arglist)->expressions().at(index)
                    );
                }

                auto generated_call_for_apply = new (mod) CallExpr(
                    apply->function(),
                    call_arguments,
                    apply->location()
                );
                generated_calls.emplace_back(generated_call_for_apply);
            }

            if (generated_calls.size() == 1)
                *expr_ptr = generated_calls.at(0);
            else {
                *expr_ptr = new (mod) GroupExpr(
                    generated_calls,
                    apply->location()
                );
                LCC_ASSERT(Analyse(expr_ptr));
            }
        } break;

        case Expr::Kind::Match: {
            auto* match = as<MatchExpr>(expr);

            if (match->names().size() != match->bodies().size())
                Diag::ICE("MatchExpr has mismatched amount of names and bodies");

            if (not Analyse(&match->object())) {
                expr->set_sema_errored();
                return false;
            }

            if (not is<SumType>(match->object()->type())) {
                Error(
                    match->object()->location(),
                    "Invalid type for match object: {}\n  (requires sum type)",
                    match->object()->type()
                );
                expr->set_sema_errored();
                return false;
            }

            // Ensure all names are parts of the composite type of the object.
            auto* s = as<SumType>(match->object()->type());
            auto it = rgs::find_if(match->names(), [&](const auto& name) {
                // If we find the member, continue. If we have a mismatch, return true.
                return s->member_by_name(name) == nullptr;
            });
            if (it != match->names().end()) {
                Error(
                    match->bodies().at(usz(it - match->names().begin()))->location(),
                    "Name given in match expression, `{}`, not present as part of the composite type we are matching on.",
                    *it
                );
                expr->set_sema_errored();
                return false;
            }

            if (
                not rgs::all_of(s->members(), [&](const auto& member) {
                    return rgs::any_of(match->names(), [&](const auto& name) {
                        return name == member.name;
                    });
                })
            ) {
                auto e = Error(match->location(), "Not all members of composite type handled in match");
                for (const auto& m : s->members()) {
                    if (not rgs::any_of(match->names(), [&](const auto& name) {
                            return name == m.name;
                        })) {
                        e.attach(Note(m.location, "Unhandled member: {}", m.name));
                    }
                }
                expr->set_sema_errored();
                return false;
            }

            // Analyse match bodies
            for (auto*& body : match->bodies()) {
                if (not Analyse(&body)) {
                    expr->set_sema_errored();
                    return false;
                }
            }

            Expr* if_expr = nullptr;
            auto body = match->bodies().rbegin();
            for (auto name = match->names().rbegin(); name != match->names().rend(); ++name, ++body) {
                LCC_ASSERT(body != match->bodies().rend(), "MatchExpr has mismatched amount of names and bodies");

                auto* member_access = new (mod) MemberAccessExpr(
                    match->object(),
                    *name,
                    (*body)->location()
                );
                auto* cond_expr = new (mod) UnaryExpr(
                    TokenKind::Has,
                    member_access,
                    false,
                    (*body)->location()
                );
                auto* then_expr = *body;

                // This is how we build the chain of ifs
                auto* otherwise_expr = if_expr;
                if_expr = new (mod) IfExpr(
                    cond_expr,
                    then_expr,
                    otherwise_expr,
                    match->location()
                );
                if (not Analyse(&if_expr)) {
                    expr->set_sema_errored();
                    return false;
                }
            }
            *expr_ptr = if_expr;
        } break;

        case Expr::Kind::Switch: {
            auto* sw = as<SwitchExpr>(expr);

            if (sw->names().size() != sw->bodies().size())
                Diag::ICE("SwitchExpr has mismatched amount of names and bodies");

            if (not Analyse(&sw->object())) {
                expr->set_sema_errored();
                return false;
            }

            if (not is<EnumType>(sw->object()->type())) {
                Error(
                    sw->object()->location(),
                    "Invalid type for switch object: {}\n  (requires enum type)",
                    sw->object()->type()
                );
                expr->set_sema_errored();
                return false;
            }

            // Ensure all names are parts of the composite type of the object.
            auto* e = as<EnumType>(sw->object()->type());
            auto it = rgs::find_if(sw->names(), [&](const auto& name) {
                // If we find the member, continue. If we have a mismatch, return true.
                return e->enumerator_by_name(name) == nullptr;
            });
            if (it != sw->names().end()) {
                // TODO: Provide list of names that would be valid.
                Error(
                    sw->bodies().at(usz(it - sw->names().begin()))->location(),
                    "Name given in switch expression, `{}`, not present as part of the enum type we are switching on.",
                    *it
                );
                expr->set_sema_errored();
                return false;
            }

            // Ensure all enumerators are handled in body expression.
            if (
                not rgs::all_of(e->enumerators(), [&](const auto& enumerator) {
                    return rgs::any_of(sw->names(), [&](const auto& name) {
                        return name == enumerator->name();
                    });
                })
            ) {
                auto err = Error(
                    sw->location(),
                    "Not all enumerators handled in switch"
                );
                for (const auto& enumerator : e->enumerators()) {
                    if (not rgs::any_of(sw->names(), [&](const auto& name) {
                            return name == enumerator->name();
                        })) {
                        err.attach(
                            Note(
                                enumerator->location(),
                                "Unhandled enumerator: {}",
                                enumerator->name()
                            )
                        );
                    }
                }
                expr->set_sema_errored();
                return false;
            }

            // Analyse match bodies
            for (auto*& body : sw->bodies()) {
                if (not Analyse(&body)) {
                    expr->set_sema_errored();
                    return false;
                }
            }

            Expr* if_expr = nullptr;
            auto body = sw->bodies().rbegin();
            for (auto name = sw->names().rbegin(); name != sw->names().rend(); ++name, ++body) {
                LCC_ASSERT(
                    body != sw->bodies().rend(),
                    "SwitchExpr has mismatched amount of names and bodies"
                );

                auto* enumerator = e->enumerator_by_name(*name);
                LCC_ASSERT(enumerator);
                auto enumerator_value = new (mod) ConstantExpr(
                    enumerator,
                    enumerator->value()
                );
                enumerator_value->type(e);

                auto* cond_expr = new (mod) BinaryExpr(
                    TokenKind::Eq,
                    sw->object(),
                    enumerator_value,
                    (*body)->location()
                );
                auto* then_expr = *body;

                // This is how we build the chain of ifs
                auto* otherwise_expr = if_expr;
                if_expr = new (mod) IfExpr(
                    cond_expr,
                    then_expr,
                    otherwise_expr,
                    sw->location()
                );
                if (not Analyse(&if_expr)) {
                    expr->set_sema_errored();
                    return false;
                }
            }
            *expr_ptr = if_expr;
        } break;

        /// For return expressions, make sure that the type of the
        /// argument, if any, matches that of the function containing
        /// the return expression.
        case Expr::Kind::Return: {
            /// Check the return value.
            auto* r = as<ReturnExpr>(expr);
            auto* ret_type = as<FuncType>(curr_func->type())->return_type();
            if (r->value() and not Analyse(&r->value(), ret_type)) {
                expr->set_sema_errored();
                return false;
            }

            // NOTE: Just for forget-to-free diagnostics.
            // If returned value is a dynamic array, remove that dynamic array's
            // declaration from the list of dangling dynamic arrays.
            if (
                r->value() and r->value()->ok() and r->value()->type()->is_dynamic_array()
            ) {
                if (auto* nameref = cast<NameRefExpr>(r->value()))
                    std::erase(curr_func->dangling_dynarrays(), nameref->target());
            }

            /// Make sure that it matches the return type.
            if (ret_type->is_void()) {
                /// Note we allow return expressions to have an operand so long
                /// as that operand has type void; this can be the case for e.g.
                /// calls to functions returning void.
                if (r->value() and r->value()->ok() and not r->value()->type()->is_void())
                    Error(r->location(), "Function returning void must not return a value");
            } else {
                if (not r->value()) Error(
                    r->location(),
                    "Non-void function must return a value"
                );
                else if (not Convert(&r->value(), ret_type)) Error(
                    r->location(),
                    "Type of return expression is not convertible to return type {}",
                    ret_type
                );
                LValueToRValue(&r->value());
            }
        } break;

        /// The condition of an if statement must be convertible to bool, and
        /// its type is the common type of the two branches.
        case Expr::Kind::If: {
            auto* i = as<IfExpr>(expr);
            if (not Analyse(&i->condition())) {
                expr->set_sema_errored();
                return false;
            }
            if (not Convert(&i->condition(), Type::Bool)) {
                Error(
                    i->condition()->location(),
                    "Invalid type for if condition: {}",
                    i->condition()->type()
                );
            }
            LValueToRValue(&i->condition());

            /// Analyse the branches.
            if (not Analyse(&i->then())) {
                expr->set_sema_errored();
                return false;
            }
            if (i->otherwise() and not Analyse(&i->otherwise())) {
                expr->set_sema_errored();
                return false;
            };

            if (not i->then()->ok() or (i->otherwise() and not i->otherwise()->ok()))
                i->set_sema_errored();

            // If both branches exist, and both branches are convertible to a common
            // type, then this IfExpr returns that common type. Otherwise, it's a void
            // expression.
            i->type(Type::Void);
            if (
                i->then() and i->otherwise()
                and not i->then()->type()->is_void()
                and not i->otherwise()->type()->is_void()
            ) {
                if (ConvertToCommonType(&i->then(), &i->otherwise())) {
                    // fmt::print("THEN\n");
                    // i->then()->print(true);
                    // fmt::print("OTHERWISE\n");
                    // i->otherwise()->print(true);
                    // fmt::print("Common type: {}\n", *i->then()->type());

                    i->type(i->then()->type());
                    // Do LValueToRValue conversion iff one branch is an lvalue.
                    // Otherwise, match lvalue-ness.
                    if (i->then()->is_lvalue() and i->otherwise()->is_lvalue())
                        i->set_lvalue();
                    else if (i->then()->is_lvalue())
                        LValueToRValue(&i->then());
                    else if (i->otherwise()->is_lvalue())
                        LValueToRValue(&i->otherwise());
                }
            }

            if (i->type()->is_void()) {
                Discard(&i->then());
                if (i->otherwise()) Discard(&i->otherwise());
            }
        } break;

        /// The type of a group is the type of its last expression. Type
        /// inference is only used for the last expression in the group.
        case Expr::Kind::Group: {
            auto* group = as<GroupExpr>(expr);
            if (group->expressions().empty()) {
                group->type(Type::Void);
                break;
            }

            for (auto& child : group->expressions()) {
                const bool last = child == group->expressions().back();
                if (not Analyse(&child, last ? expected_type : nullptr)) {
                    group->set_sema_errored();
                    return false;
                }
                // NOTE: We do NOT discard the child expression here, since a group
                // expression is thought to return all of it's expression's results.
                //
                // (4, 20) -(evaluated)-> !{4, 20}
            }

            if (not group->sema_errored()) {
                auto e_last = group->expressions().back();
                group->set_lvalue(e_last->is_lvalue());
                group->type(e_last->type());
            }
        } break;

        /// The type of a block is the type of its last expression. Type
        /// inference is only used for the last expression in the block.
        case Expr::Kind::Block: {
            auto* block = as<BlockExpr>(expr);
            if (block->children().empty()) {
                block->type(Type::Void);
                break;
            }

            for (auto*& child : block->children()) {
                const bool last = &child == block->last_expr();
                if (not Analyse(&child, last ? expected_type : nullptr)) {
                    block->set_sema_errored();
                    // NOTE: If, for some ungodly reason, we want to continue semantic
                    // analysis within a block after an expression within that block has
                    // already failed, we could /not/ return false here and keep going.
                    return false;
                }
                // The value of the block expression is the value of the last expression;
                // the results of the preceding expressions (if any), are unused, and can
                // therefore be discarded.
                if (not last and child->ok()) Discard(&child);
            }

            if (not block->sema_errored()) {
                block->set_lvalue(block->children().back()->is_lvalue());
                block->type(block->children().back()->type());
            }
        } break;

        case Expr::Kind::Template: {
            auto t = as<TemplateExpr>(expr);

            if (not t->params().size())
                Error(t->location(), "A template with no parameters is not allowed");

            // Analyse parameter types
            if (not rgs::any_of(
                    t->params_ref(),
                    [&](auto& p) {
                        // Custom compile-time type handling
                        if (auto n = cast<NamedType>(p.type); n and (n->name() == "expr" or n->name() == "type"))
                            return true;
                        return Analyse(&p.type);
                    }
                )) {
                expr->set_sema_errored();
                return false;
            }

            // Point target of NameRefExpr referencing template parameters to this TemplateExpr
            if (auto name = cast<NameRefExpr>(t->body())) name->target(expr);
            else {
                for (auto e : expr->children()) {
                    if (auto n = cast<NameRefExpr>(e)) {
                        auto found_it = rgs::find_if(
                            t->params(),
                            [&](auto p) { return n->name() == p.name; }
                        );
                        if (found_it != t->params().end()) {
                            n->target(expr);
                        }
                    }
                }
            }

            // TODO: If we know all of the parameters types, then we should be good to analyse the body.
            // Things that would change this are template parameters with "incomplete"
            // types (like the type type, or the expr type).

            // NOTE: TemplateExpr does not have a type, because it can't be operated
            // on like an expression, because it is a code generator.
        } break;

        /// This mainly handles explicit casts, which allow more
        /// conversions than implicit casts.
        ///
        /// We don’t ever mark this as errored because there is no
        /// type that we *cannot* cast to, and the type this expr
        /// is supposed to have is known.
        case Expr::Kind::Cast:
            AnalyseCast(as<CastExpr>(expr));
            break;

        /// Intrinsics need to be analysed individually.
        case Expr::Kind::IntrinsicCall:
            AnalyseIntrinsicCall(expr_ptr, as<IntrinsicCallExpr>(expr));
            break;

        /// This is handled by the overload resolution code. We do *not*
        /// pass in an expected type because we do not perform overload
        /// resolution on return types.
        case Expr::Kind::Call:
            AnalyseCall(expr_ptr, as<CallExpr>(expr));
            break;

        /// Analyse local and global variable declarations.
        case Expr::Kind::VarDecl: {
            auto* v = as<VarDecl>(expr);

            /// If this has an initialiser, analyse it.
            if (v->init()) {
                // Obviously, we can only perform top-down type inference if we’re not
                // already performing bottom-up inference. If the type is known, make sure
                // that we use a type that is legal in a declaration for inference.
                const bool infer_type = v->type()->is_unknown();

                if (
                    not Analyse(
                        &v->init(),
                        infer_type ? nullptr : DeclTypeDecay(v->type())
                    )
                ) v->set_sema_errored();

                // If we're using type inference, set the type of the expression to the
                // type of the initialising expression; unless there was an error, in
                // which case we error out.
                if (infer_type) {
                    if (v->init()->ok())
                        v->type(v->init()->type());
                    else {
                        v->set_sema_errored();
                        return false;
                    }
                }
            }

            /// Check that the type makes sense. In particular, if it is
            /// a function type, convert it to a function pointer type.
            v->type(DeclTypeDecay(v->type()));

            /// Make sure the initialiser is convertible to that type. Note
            /// that, if this fails, we do not mark this node as errored as
            /// its type is well-formed; it’s just the initialiser that has
            /// a problem.
            if (v->init()) {
                // An untyped compound literal initialiser is allowed for typed
                // declarations.
                auto* c = cast<CompoundLiteral>(v->init());
                // Set the type of the compound literal from the type of the declaration,
                // if the compound literal's type isn't already explicitly declared.
                if (c and c->type()->is_unknown()) {
                    *c->type_ref() = v->type();

                    // TODO: This should be handled via CompoundType. Since we're still
                    // waiting on that, this makes sure invalid code isn't produced silently.
                    if (is<ArrayType, DynamicArrayType, ArrayViewType>(c->type())) {
                        for (auto& m : c->values()) {
                            if (not Convert(&m.value, c->type()->elem())) {
                                Error(
                                    m.value->location(),
                                    "Compound Literal member of type {} is not convertible to array element type {}",
                                    m.value->type(),
                                    c->type()->elem()
                                );
                                c->set_sema_errored();
                                return false;
                            }
                        }
                    } else if (is<StructType>(c->type())) {
                        auto s = as<StructType>(c->type());
                        if (c->values().size() != s->members().size()) {
                            Error(
                                c->location(),
                                "Compound Literal has invalid expression count to be converted to {}",
                                c->type()
                            );
                            c->set_sema_errored();
                            return false;
                        }
                        for (unsigned i = 0; i < c->values().size(); ++i) {
                            auto& compound_literal_member_expression = c->values().at(i);
                            if (not Convert(&compound_literal_member_expression.value, s->members().at(i).type)) {
                                Error(
                                    compound_literal_member_expression.value->location(),
                                    "Compound Literal member of type {} is not convertible to struct member {} of type {}",
                                    compound_literal_member_expression.name,
                                    compound_literal_member_expression.value->type(),
                                    s->members().at(i).type
                                );
                                c->set_sema_errored();
                                return false;
                            }
                        }
                    } else {
                        Error(
                            c->location(),
                            "Sorry, but initialisation of values of {} type from compound literals is not yet supported",
                            c->type()
                        );
                        c->set_sema_errored();
                        return false;
                    }
                }

                // TODO: CompoundType
                // Has members. Each member has a type. The idea is this will be an
                // "intermediate" type between structs, arrays, compound literals, etc.
                // So, a StructType foo with byte members x and y is implicitly
                // convertible to a CompoundType with members byte and byte.
                // If all types of a compound type are (convertible to) the element type
                // of an array, then it can be converted to an array of the same size (or
                // a dynamic array, or array view).

                // TODO: I know the compound literal is *supposed* to be v->type() up
                // above, and that's why we set it. But, what if it isn't? What if the
                // compound's literal's expressions do not actually make the type that it
                // is expected to make?

                // compound literal initialiser for sum type
                //     foo :sum_t !{ .member expression }
                // should turn into
                //     foo :sum_t;
                //     foo.member := expression;
                auto* s = cast<SumType>(v->type());
                if (c and s) {
                    auto member = c->values().at(0);
                    usz member_index = usz(-1);
                    // TODO: Get index of named member
                    for (usz i = 0; i < s->members().size(); ++i) {
                        if (s->members().at(i).name == member.name) {
                            member_index = i;
                            break;
                        }
                    }
                    LCC_ASSERT(
                        member_index != usz(-1),
                        "Member {} does not exist in sum type",
                        member.name
                    );

                    std::vector<Expr*> replacement;

                    v->init() = nullptr;
                    replacement.push_back(v);

                    auto* member_access = new (mod) MemberAccessExpr(
                        v,
                        member.name,
                        v->location()
                    );
                    member_access->finalise(s->struct_type(), member_index);
                    member_access->type(s);

                    replacement.push_back(new (mod) BinaryExpr(
                        TokenKind::ColonEq,
                        member_access,
                        c->values().at(0).value,
                        v->location()
                    ));

                    *expr_ptr = new (mod) BlockExpr(replacement, v->location());

                    // Perform conversions (like lvalue to rvalue).
                    (void) Analyse(expr_ptr);
                } else {
                    if (not Convert(&v->init(), v->type())) Error(
                        v->init()->location(),
                        "Type of initialiser, {}, is not convertible to variable type {}",
                        v->init()->type(),
                        v->type()
                    );

                    // FIXME: This needed? Convert above should insert this.
                    LValueToRValue(&v->init());
                }
            }

            if (v->type()->is_dynamic_array() and not IsExportedLinkage(v->linkage()))
                curr_func->dangling_dynarrays().push_back(v);

            v->set_lvalue();

            if (not v->sema_errored())
                v->set_sema_done();

            if (v->ok() and not v->init()) {
                // TODO: Handle warning/, report/
                if (context->has_option("error/default-init")) {
                    Error(
                        v->location(),
                        "Default Initialization Occured"
                    );
                    v->set_sema_errored();
                    return false;
                }

                auto initializer = DefaultInitialize(v);
                if (not Analyse(&initializer))
                    return false;

                *expr_ptr = new (mod) GroupExpr({v, initializer}, v->location());
                LCC_ASSERT(Analyse(expr_ptr));
            }
        } break;

        /// These are handled by the code that also handles enums.
        case Expr::Kind::EnumeratorDecl:
            Diag::ICE(
                context,
                expr->location(),
                "Invalid semantic analysis of enumerator declaration (should have been handled in enum handling)"
            );
            LCC_UNREACHABLE();

        case Expr::Kind::CompoundLiteral: {
            auto* c = as<CompoundLiteral>(expr);

            /// Analyse all subexpressions.
            for (auto& member : c->values()) {
                if (Analyse(&member.value))
                    LValueToRValue(&member.value);
                else c->set_sema_errored();
            }

            if (c->sema_errored()) break;

            if ((not c->type() or c->type()->is_unknown()) and not expected_type) {
                // fmt::print("c->type():{}\n", fmt::ptr(c->type()));
                // if (c->type()) fmt::print("*c->type():{}\n", *c->type());
                // fmt::print("expected_type:{}\n", fmt::ptr(expected_type));
                // if (expected_type) fmt::print("*expected_type:{}\n", *expected_type);

                if (c->values().size() != 1) {
                    Error(
                        c->location(),
                        "Cannot infer type of Untyped Compound Literal with multiple subexpressions"
                    );
                    c->set_sema_errored();
                } else {
                    // Set type of compound literal to type of singular subexpression.
                    // "bubble up"
                    c->type(c->values().at(0).value->type());
                }
            }
            // If both c->type() and expected_type, Convert to expected_type.
            else if ((c->type() and not c->type()->is_unknown()) and expected_type) {
                if (not Convert(expr_ptr, expected_type)) {
                    Error(
                        c->location(),
                        "Type of compound literal {} is not convertible to expected type {}",
                        c->type(),
                        expected_type
                    );
                    c->set_sema_errored();
                }
            }

            if (not Analyse(c->type_ref())) {
                Diag::ICE(context, c->location(), "Failed to analyse type of compound literal");
                LCC_UNREACHABLE();
            }

            // Ensure named members exist in represented type.
            if (auto* union_t = cast<UnionType>(c->type())) {
                (void) union_t;
                LCC_TODO("Compound literal to union");
            } else if (auto* array_t = cast<ArrayType>(c->type())) {
                if (c->values().size() != array_t->dimension()) {
                    Error(
                        c->location(),
                        "Compound literal for array type must have number of member expressions equal to array dimension {}, but got {} instead\n",
                        array_t->dimension(),
                        c->values().size()
                    );
                    c->set_sema_errored();
                    break;
                }
                for (auto& m : c->values()) {
                    if (not Convert(&m.value, array_t->element_type())) {
                        Error(
                            m.value->location(),
                            "Every member of a compound literal for an array type must be convertible to the array element type, but {} is not convertible to {}",
                            m.value->type(),
                            array_t->element_type()
                        );
                        c->set_sema_errored();
                    }
                }
            } else if (auto* sum_t = cast<SumType>(c->type())) {
                if (c->values().size() != 1) {
                    Error(
                        c->location(),
                        "Compound literal for a sum type must have a *single*, named member expression"
                    );
                    c->set_sema_errored();
                    break;
                }

                auto& member = c->values().at(0);
                if (member.name.empty()) {
                    std::string valid_names{};
                    for (const auto& m : sum_t->members()) {
                        valid_names += m.name;
                        valid_names += ", ";
                    }
                    LCC_ASSERT(
                        not valid_names.empty(),
                        "Sum type {} has no members!",
                        *c->type()
                    );
                    // Get rid of trailing comma and space.
                    valid_names.pop_back();
                    valid_names.pop_back();
                    Error(
                        member.value->location(),
                        "Compound literal for {} must have a single, *named* member expression.\n"
                        "Otherwise, we wouldn't know which member you mean to initialise.\n"
                        "Possible names: {}",
                        c->type(),
                        valid_names
                    );
                    c->set_sema_errored();
                    break;
                }

            } else if (auto* struct_t = cast<StructType>(c->type())) {
                // TODO: Reorder children based on member indices of underlying struct
                // type so that IRGen doesn't have to jump around.
                std::vector<CompoundLiteral::Member> new_order{};
                new_order.reserve(c->values().size());
                for (usz i = 0; i < c->values().size(); ++i) {
                    const auto& member = c->values().at(i);
                    isz struct_member_index = isz(i);
                    if (not member.name.empty()) {
                        struct_member_index = struct_t->member_index_by_name(member.name);
                        if (struct_member_index == StructType::Member::BadIndex) {
                            Error(
                                member.value->location(),
                                "Named member {} of compound literal does not correspond to any member of {}",
                                member.name,
                                c->type()
                            );
                            c->set_sema_errored();
                            continue;
                        }
                    }
                    new_order.insert(
                        std::min(new_order.begin() + struct_member_index, new_order.end()),
                        CompoundLiteral::Member{member}
                    );
                }

                LCC_ASSERT(
                    new_order.size() == c->values().size(),
                    "Messed up sorting of named compound literal member expressions"
                );
                c->values() = new_order;

            } else {
                // Even if the member is named, the name wouldn't mean anything anyway
                // (since the represented type doesn't support the concept of named
                // members, i.e. an integer), so we replace the compound literal with a
                // single member expression with that expression.
                if (c->values().size() == 1) {
                    *expr_ptr = c->values().at(0).value;
                    break;
                }

                for (const auto& member : c->values()) {
                    if (not member.name.empty()) {
                        Warning(
                            member.value->location(),
                            "Ignoring name of compound literal member, as {} does not support the concept of named members",
                            c->type()
                        );
                    }
                }
            }

        } break;

        /// LHS must be a (pointer to a) struct, and the identifier must
        /// exist in the struct.
        case Expr::Kind::MemberAccess: {
            auto* m = as<MemberAccessExpr>(expr);
            /// If there is an error analysing the object, we don’t know
            /// its type and can thus not continue checking this.
            if (not Analyse(&m->object())) {
                m->set_sema_errored();
                break;
            }

            /// Accessing ‘members’ of modules.
            if (
                is<NameRefExpr>(m->object())
                and is<ModuleExpr>(as<NameRefExpr>(m->object())->target())
            ) {
                // m->name() == name of member we are accessing
                // m->object() == NameRef to module we are accessing
                auto* name_ref = as<NameRefExpr>(m->object());
                auto* module_expr = as<ModuleExpr>(name_ref->target());
                auto* referenced_module = module_expr->mod();
                LCC_ASSERT(referenced_module, "ModuleExpr invalid");
                auto* scope = referenced_module->global_scope();
                // Replace member access with a name ref
                *expr_ptr = new (mod) NameRefExpr(
                    m->name(),
                    scope,
                    m->location()
                );
                AnalyseNameRef(as<NameRefExpr>(*expr_ptr));
                break;
            }

            /// ‘object’ is actually a type name.
            if (
                is<NameRefExpr>(m->object())
                and is<TypeDecl>(as<NameRefExpr>(m->object())->target())
            ) {
                auto* t = as<TypeDecl>(as<NameRefExpr>(m->object())->target());

                /// Handle accessing enumerators.
                if (auto* e = cast<EnumType>(t->type())) {
                    auto it = rgs::find_if(
                        e->enumerators(),
                        [&](auto&& en) { return en->name() == m->name(); }
                    );
                    if (it == e->enumerators().end()) {
                        auto err = Error(m->location(), "Type {} has no enumerator named '{}'", e, m->name());
                        err.attach(Note(
                            e->location(),
                            "Available members:\n\t{}",
                            fmt::join(
                                vws::transform(
                                    e->enumerators(),
                                    [](auto d) { return d->name(); }
                                ),
                                "\n\t"
                            )
                        ));
                        m->set_sema_errored();
                        break;
                    }

                    auto* enumerator = *it;
                    if (enumerator->sema_errored()) {
                        m->set_sema_errored();
                        break;
                    }

                    if (not enumerator->ok()) {
                        Error(
                            m->location(),
                            "Enumerator {} cannot be used before it is defined",
                            enumerator->name()
                        );
                        m->set_sema_errored();
                        break;
                    }

                    m->type(enumerator->type());
                    m->set_sema_done();
                    *expr_ptr = new (mod) ConstantExpr(expr, enumerator->value());
                    break;
                }

                Error(
                    m->location(),
                    "Type '{}' does not have member '{}'",
                    *t->type(),
                    m->name()
                );
                m->set_sema_errored();
                return false;
            }

            /// Type must be a struct type (or something that represents one, like a
            /// DynamicArrayType or SumType)
            auto* stripped_object_type = m->object()->type()->strip_pointers_and_references();

            // Access to union member
            if (
                auto* union_type = cast<UnionType>(stripped_object_type)
            ) {
                auto& members = union_type->members();
                auto it = rgs::find_if(
                    members,
                    [&](auto& member) { return member.name == m->name(); }
                );
                if (it == members.end()) {
                    Error(m->location(), "Union {} has no member named '{}'", union_type, m->name());
                    m->set_sema_errored();
                    break;
                }

                auto* cast = new (mod) CastExpr(m->object(), it->type, CastKind::HardCast, m->location());
                cast->set_lvalue(m->object()->is_lvalue());
                *expr_ptr = cast;
                break;
            }

            // Access to sum type member
            if (auto* sum_type = cast<SumType>(stripped_object_type)) {
                auto& members = sum_type->members();
                auto it = rgs::find_if(members, [&](auto& member) { return member.name == m->name(); });
                if (it == members.end()) {
                    Error(m->location(), "Sum type {} has no member named '{}'", sum_type, m->name());
                    m->set_sema_errored();
                    break;
                }

                // The type of the sum type member access is the type of the accessed
                // member.
                m->type(it->type);

                m->finalise(
                    sum_type->struct_type(),
                    usz(std::distance(members.begin(), it))
                );

                m->set_lvalue();

                // fmt::print("\nOOHWEE\n");
                // mod.print(true);

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
                //
                // The following
                //   bar.x;
                // should turn into (if tag, then access)
                //   if (bar.tag = foo.tag.x)
                //     @(:cint.ptr &bar.data);
                //   else default_constant_expression foo.x;
                //
                // It might be interesting to require a constant expression initialiser in
                // sum type declarations and then have an `else` that returns that if the
                // accessed sum type has the wrong data in it.
                //
                // The following
                //   has bar.x;
                // should turn into
                //   bar.tag = foo.tag.x;

                break;
            }

            auto* struct_type = cast<StructType>(stripped_object_type);

            if (not struct_type and is<DynamicArrayType>(stripped_object_type))
                struct_type = as<DynamicArrayType>(stripped_object_type)->struct_type(mod);

            if (not struct_type and is<ArrayViewType>(stripped_object_type))
                struct_type = as<ArrayViewType>(stripped_object_type)->struct_type(mod);

            if (not struct_type) {
                auto e = Error(
                    m->object()->location(),
                    "LHS of member access must be a struct, but was {}",
                    m->object()->type()
                );
                // Special error for trying to access an enumerator off a value of an
                // enum.
                if (is<EnumType>(m->object()->type())) {
                    e.attach(
                        Note(
                            m->object()->type()->location(),
                            "If you meant to access an enumerator, access the type itself (not a value of the type)."
                        )
                    );
                    if (
                        is<NameRefExpr>(m->object())
                        and is<Decl>(as<NameRefExpr>(m->object())->target())
                    ) {
                        // TODO: fixes
                        e.attach(Note(
                            as<NameRefExpr>(m->object())->target()->location(),
                            "Did you mean to use `::` in this declaration?"
                        ));
                    }
                }

                m->set_sema_errored();
                break;
            }

            /// The struct type must contain the member.
            auto& members = struct_type->members();
            auto member_predicate = [&](auto& member) {
                return member.name == m->name();
            };
            auto it = rgs::find_if(members, member_predicate);
            if (it == members.end()) {
                // If the struct (or struct-like) type does not contain a member with the
                // exact name given, go on to check if any members are supplanted: if
                // there are supplanted members, look in their namespaces for the named
                // member (and eventually insert the necessary extra member access).
                std::function<bool(StructType::Member&)> supplanted_member_predicate = [&](auto& member) {
                    if (member.name == m->name()) return true;
                    if (member.supplanted) {
                        // Confidence Check
                        if (not is<StructType>(member.type)) {
                            Error(m->location(), "supplant does not support non-struct types (yet?): {}", member.type);
                            m->set_sema_errored();
                            return false;
                        }
                        auto supplanted_members = as<StructType>(member.type)->members();
                        auto supplanted_it = rgs::find_if(supplanted_members, supplanted_member_predicate);
                        return supplanted_it != supplanted_members.end();
                    }
                    return false;
                };

                it = rgs::find_if(members, supplanted_member_predicate);
                if (m->sema_errored()) break;
                if (it == members.end()) {
                    // Member access name doesn't match any member's name, nor the name of any
                    // member of any supplanted member.
                    auto e = Error(
                        m->location(),
                        "{} has no member named '{}'",
                        struct_type,
                        m->name()
                    );
                    if (struct_type->members().size()) {
                        e.attach(
                            Note(
                                m->location(),
                                "Valid members include: {}",
                                fmt::join(
                                    vws::transform(
                                        struct_type->members(),
                                        [&](StructType::Member& member) {
                                            if (member.supplanted)
                                                return "members of supplanted " + member.type->string();
                                            return member.name;
                                        }
                                    ),
                                    ","
                                )
                            )
                        );
                    } else {
                        // Struct type being accessed has NO members
                        e.attach(
                            Note(
                                struct_type->location(),
                                "{} has NO members!",
                                struct_type
                            )
                        );
                    }
                    m->set_sema_errored();
                    break;
                }

                // Member access to supplanted member
                // (m->object) . (m->name)  ->  (m->object) . (supplanted_member) . (m->name)
                auto* supplanted_member_access
                    = new (mod) MemberAccessExpr(
                        m->object(),
                        it->name,
                        m->location()
                    );
                auto* new_member_access
                    = new (mod) MemberAccessExpr(
                        supplanted_member_access,
                        m->name(),
                        m->location()
                    );
                *expr_ptr = new_member_access;
                if (not Analyse(expr_ptr)) {
                    m->set_sema_errored();
                    break;
                }
                break;
            }

            /// Set the struct and member index.
            m->finalise(struct_type, usz(std::distance(members.begin(), it)));

            /// Dereference pointers until we have an lvalue to struct. The
            /// member access is an lvalue, iff the struct is an lvalue.
            m->set_lvalue(ImplicitDereference(&m->object()));
            m->type(it->type);
        } break;

        case Expr::Kind::Sizeof: {
            auto* sizeof_expr = as<SizeofExpr>(expr);
            (void) Analyse(sizeof_expr->expr_ref());

            aint value{};
            if (auto* e_type = cast<TypeExpr>(sizeof_expr->expr()))
                value = e_type->contained_type()->size(context);
            else if (auto* typed_expr = cast<TypedExpr>(sizeof_expr->expr()))
                value = typed_expr->type()->size(context);
            else {
                Error(sizeof_expr->location(), "Unhandled expression in sizeof");
                expr->set_sema_errored();
                break;
            }

            *expr_ptr = new (mod) IntegerLiteral(value, expr->location());
        } break;

        case Expr::Kind::Alignof: {
            auto* alignof_expr = as<AlignofExpr>(expr);
            (void) Analyse(alignof_expr->expr_ref());

            aint value{};
            if (auto* e_type = cast<TypeExpr>(alignof_expr->expr()))
                value = e_type->contained_type()->size(context);
            else if (auto* typed_expr = cast<TypedExpr>(alignof_expr->expr()))
                value = typed_expr->type()->align(context);
            else Error(alignof_expr->location(), "Unhandled expression in alignof");

            *expr_ptr = new (mod) IntegerLiteral(value, expr->location());
        } break;

        /// Validate overload sets.
        case Expr::Kind::OverloadSet: {
            const auto& os = as<OverloadSet>(expr)->overloads();

            /// An overload set must not contain two overloads with the
            /// same parameter types. All function signatures have already
            /// been analysed, so we just need to compare them.
            for (usz i = 0; i < os.size(); i++) {
                auto* oi = os[i];
                auto oi_params = oi->param_types();
                for (usz j = i + 1; j < os.size(); j++) {
                    auto* oj = os[j];
                    auto oj_params = oj->param_types();

                    /// Different number of parameters means these two can’t be the same.
                    if (oi_params.size() != oj_params.size()) continue;

                    /// Compare the parameters.
                    usz k = 0;
                    for (; k < oi_params.size(); ++k) {
                        if (not Type::Equal(oi_params[isz(k)], oj_params[isz(k)]))
                            break;
                    }

                    /// If all of them are equal, then we have a problem.
                    if (k == oi_params.size()) {
                        // FIXME: There is a bug here, but I don't yet know what it is. I ran into
                        // a crash when defining two identical functions (format functions).
                        Error(
                            oi->location(),
                            "Overload set contains two overloads with the same parameter types, {} and {}",
                            oi_params[isz(k)],
                            oj_params[isz(k)]
                        );
                        Note(oj->location(), "Conflicting overload is here");
                        expr->set_sema_errored();
                    }
                }
            }
        } break;

        /// Unary prefix and postfix expressions.
        case Expr::Kind::Unary:
            AnalyseUnary(expr_ptr, as<UnaryExpr>(expr));
            break;

        /// Binary expressions.
        case Expr::Kind::Binary:
            AnalyseBinary(expr_ptr, as<BinaryExpr>(expr));
            break;

        /// Reference to a declared entity.
        case Expr::Kind::NameRef:
            AnalyseNameRef(as<NameRefExpr>(expr));
            break;

        /// Functions are analysed separately.
        case Expr::Kind::FuncDecl:
            LCC_ASSERT(expr->type()->is_function());
            break;

        case Expr::Kind::TemplatedFuncDecl:
            LCC_ASSERT(expr->type()->is_function());
            break;

        case Expr::Kind::Type: {
            auto e_type = as<TypeExpr>(expr);
            (void) Analyse(e_type->contained_type_ref());
            if (not e_type->contained_type()->ok())
                e_type->set_sema_errored();
            break;
        }

        /// The actual work here is analysing the type, so this is a no-op.
        case Expr::Kind::TypeDecl:
        case Expr::Kind::TypeAliasDecl:
        /// There isn’t really a way these could be malformed.
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::FractionalLiteral:
        case Expr::Kind::StringLiteral:
        /// These should only be created by sema and are thus no-ops.
        case Expr::Kind::Module:
        case Expr::Kind::EvaluatedConstant:
            break;
    }

    /// Do *not* use `expr` here, as it may have been replaced by something else.
    if (not (*expr_ptr)->sema_done_or_errored())
        (*expr_ptr)->set_sema_done();

    return (*expr_ptr)->ok();
}

void lcc::glint::Sema::RewriteToBinaryOpThenAssign(
    Expr** expr_ptr,
    TokenKind op,
    Expr* lhs,
    Expr* rhs,
    Location location
) {
    *expr_ptr = new (mod) BinaryExpr(
        TokenKind::ColonEq,
        lhs,
        new (mod) BinaryExpr(
            op,
            lhs,
            rhs,
            location
        ),
        location
    );
    AnalyseBinary(expr_ptr, as<BinaryExpr>(*expr_ptr));
}

void lcc::glint::Sema::AnalyseBinary(Expr** expr_ptr, BinaryExpr* b) {
    // Catch dynarray[index] on lhs before it gets rewritten.
    if (b->op() == TokenKind::PlusEq) {
        if (
            auto subscript = cast<BinaryExpr>(b->lhs());
            subscript and subscript->op() == TokenKind::Subscript
        ) {
            if (not Analyse(&subscript->lhs())) {
                subscript->set_sema_errored();
                return;
            }
            if (not Analyse(&subscript->rhs())) {
                subscript->set_sema_errored();
                return;
            }
            if (subscript->lhs()->type()->is_dynamic_array()) {
                // subscript of dynamic array on lhs of += is an insertion operation.
                auto dynarray_expr = subscript->lhs();
                auto index_expr = subscript->rhs();

                // Relevant dynamic array type
                auto dyn_t = as<DynamicArrayType>(subscript->lhs()->type());

                // Ensure rhs is convertible to dynamic array element type
                if (not Convert(&b->rhs(), dyn_t->elem())) {
                    Error(b->location(), "Cannot insert to {} with value of type {}", dyn_t, b->rhs()->type());
                    b->set_sema_errored();
                    return;
                }

                // template(dynarray : expr, index : expr, value : expr) {
                //   if index < 0 or index > dynarray.size, {
                //     exit 1;
                //   };
                //   ;; If we need to grow, do that
                //   dynarray_grow dynarray;
                //   ;; Copy size - index elements forward one element starting at given index
                //   memmove dynarray.data[index + 1], dynarray.data[index], dynarray.size - index;
                //   ;; Insert given element at index, now that everything is moved out of the
                //   ;; way.
                //   @dynarray[index] := value;
                //   dynarray.size += 1;
                // };

                std::vector<Expr*> exprs{};
                {
                    auto dyn_data
                        = new (mod) MemberAccessExpr(dynarray_expr, "data", {});
                    auto dyn_size
                        = new (mod) MemberAccessExpr(dynarray_expr, "size", {});

                    // index less than 0
                    auto cmp_zero
                        = new (mod) BinaryExpr(
                            TokenKind::Lt,
                            index_expr,
                            new (mod) IntegerLiteral(0, {}),
                            {}
                        );
                    // index greater than size
                    auto cmp_size
                        = new (mod) BinaryExpr(TokenKind::Gt, index_expr, dyn_size, {});

                    auto cmp_or
                        = new (mod) BinaryExpr(TokenKind::Or, cmp_zero, cmp_size, {});

                    // TODO: if oob_print or something. FIXME: Use frontend option.
                    auto puts_ref = new (mod) NameRefExpr(
                        "puts",
                        mod.global_scope(),
                        {}
                    );
                    std::string str_value = "Glint Runtime Error: oob dynamic array access";
                    if (b->location().seekable(context)) {
                        auto locinfo = b->location().seek_line_column(context);
                        str_value = fmt::format(
                            "{}:{}:{}: {}",
                            locinfo.line,
                            locinfo.col,
                            context->files().at(b->location().file_id)->path().lexically_normal().string(),
                            str_value
                        );
                    }
                    auto str = new (mod) StringLiteral(mod, str_value, {});
                    auto sub_str = new (mod) BinaryExpr(
                        TokenKind::Subscript,
                        str,
                        new (mod) IntegerLiteral(0, {}),
                        {}
                    );
                    auto then_print = new (mod) CallExpr(
                        puts_ref,
                        {sub_str},
                        {}
                    );

                    auto exit_ref = new (mod) NameRefExpr(
                        "exit",
                        mod.global_scope(),
                        {}
                    );
                    auto status_literal = new (mod) IntegerLiteral(1, {});
                    // TODO: When user defines oob_access handler, call that handler.
                    auto then_outofbounds = new (mod) CallExpr(
                        exit_ref,
                        {status_literal},
                        {}
                    );
                    auto then_block = new (mod) BlockExpr(
                        {then_print,
                         then_outofbounds},
                        {}
                    );

                    auto if_outofbounds = new (mod) IfExpr(
                        cmp_or,
                        then_block,
                        nullptr,
                        {}
                    );
                    exprs.emplace_back(if_outofbounds);

                    // Grow, if need be.
                    auto grow_if = new (mod) CallExpr(
                        named_template("dynarray_grow"),
                        {dynarray_expr},
                        {}
                    );
                    exprs.emplace_back(grow_if);

                    auto memmove_ref
                        = new (mod) NameRefExpr(
                            "memmove",
                            mod.global_scope(),
                            {}
                        );
                    // subscript dynarray_expr.data with index_expr + 1 offset
                    auto index_plusone
                        = new (mod) BinaryExpr(
                            TokenKind::Plus,
                            index_expr,
                            new (mod) IntegerLiteral(1, {}),
                            {}
                        );
                    auto memmove_dest = new (mod) BinaryExpr(
                        TokenKind::Subscript,
                        dyn_data,
                        index_plusone,
                        {}
                    );
                    // subscript dynarray_expr.data with index_expr
                    auto memmove_source = new (mod) BinaryExpr(
                        TokenKind::Subscript,
                        dyn_data,
                        index_expr,
                        {}
                    );
                    // subtract index_expr from dynarray_expr.size
                    auto memmove_size = new (mod) BinaryExpr(
                        TokenKind::Minus,
                        dyn_size,
                        index_expr,
                        {}
                    );
                    auto call_memmove = new (mod) CallExpr(
                        memmove_ref,
                        {memmove_dest, memmove_source, memmove_size},
                        {}
                    );
                    exprs.emplace_back(call_memmove);

                    // Subscript dynarray data with index expression
                    auto assign_lhs_subscript = new (mod) BinaryExpr(
                        TokenKind::Subscript,
                        dyn_data,
                        index_expr,
                        {}
                    );
                    // Dereference subscript
                    auto assign_lhs = new (mod) UnaryExpr(
                        TokenKind::Dereference,
                        assign_lhs_subscript,
                        false,
                        {}
                    );
                    // @dynarray[index] := value;
                    auto assign = new (mod) BinaryExpr(
                        TokenKind::ColonEq,
                        assign_lhs,
                        b->rhs(),
                        {}
                    );
                    exprs.emplace_back(assign);

                    // dynarray.size += 1;
                    auto increase_size = new (mod) BinaryExpr(
                        TokenKind::PlusEq,
                        dyn_size,
                        new (mod) IntegerLiteral(1, {}),
                        {}
                    );
                    exprs.emplace_back(increase_size);
                }

                *expr_ptr = new (mod) BlockExpr(exprs, b->location());
                (void) Analyse(expr_ptr);

                return;
            }
        }
    }

    // Give up if there is an error in either operand.
    if (not Analyse(&b->lhs()) or not Analyse(&b->rhs())) {
        b->set_sema_errored();
        return;
    }

#define lhs_t b->lhs()->type()
#define rhs_t b->rhs()->type()

    switch (b->op()) {
        case TokenKind::RightArrow:
            Error(
                b->location(),
                "Sorry, but {} doesn't do anything yet.",
                ToString(b->op())
            );
            b->set_sema_errored();
            break;

        case TokenKind::PlusEq:
            // NOTE: Dynamic array insert handled above
            // Handle dynamic array append.
            if (lhs_t->is_dynamic_array()) {
                // Ensure rhs is convertible to lhs element type
                if (not Convert(&b->rhs(), lhs_t->elem())) {
                    Error(b->location(), "Cannot append to {} with value of type {}", lhs_t, rhs_t);
                    b->set_sema_errored();
                    break;
                }
                // Generate following pseudo-code:
                //   dynarray_grow b->lhs();
                //   @b->lhs().data[b->lhs().size] := b->rhs();
                //   b->lhs().size += 1;

                // dynarray_grow b->lhs();
                auto grow_if = new (mod) CallExpr(named_template("dynarray_grow"), {b->lhs()}, {});

                // @b->lhs().data[b->lhs().size] := b->rhs()
                auto lhs_data = new (mod) MemberAccessExpr(b->lhs(), "data", {});
                auto lhs_size = new (mod) MemberAccessExpr(b->lhs(), "size", {});
                auto subscript_lhs = new (mod) BinaryExpr(TokenKind::Subscript, lhs_data, lhs_size, {});
                auto dereference_subscript = new (mod) UnaryExpr(TokenKind::Dereference, subscript_lhs, false, {});
                auto assign = new (mod) BinaryExpr(TokenKind::ColonEq, dereference_subscript, b->rhs(), {});

                // b->lhs().size += 1;
                auto update_size = new (mod) BinaryExpr(
                    TokenKind::PlusEq,
                    lhs_size,
                    new (mod) IntegerLiteral(1, {}),
                    {}
                );

                *expr_ptr = new (mod) BlockExpr({grow_if, assign, update_size}, b->location());
                (void) Analyse(expr_ptr);
                LCC_ASSERT((*expr_ptr)->ok(), "Dynamic Array Prepend failed sema (oops)");

                break;
            }
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::Plus, b);
            break;

        case TokenKind::MinusEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::Minus, b);
            break;

        case TokenKind::StarEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::Star, b);
            break;

        case TokenKind::SlashEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::Slash, b);
            break;

        case TokenKind::PercentEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::Percent, b);
            break;

        case TokenKind::AmpersandEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::BitAND, b);
            break;

        case TokenKind::PipeEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::BitOR, b);
            break;

        case TokenKind::CaretEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::BitXOR, b);
            break;

        case TokenKind::LBrackEq:
            RewriteToBinaryOpThenAssign(expr_ptr, TokenKind::Subscript, b);
            break;

        case TokenKind::TildeEq: {
            if (not lhs_t->is_dynamic_array()) {
                Error(
                    b->location(),
                    "Lhs of prepend operator {} must be a dynamic array, but is {} instead",
                    ToString(b->op()),
                    b->lhs()->type()
                );
                b->set_sema_errored();
                break;
            }

            // Ensure rhs is convertible to lhs element type
            if (not Convert(&b->rhs(), lhs_t->elem())) {
                Error(b->location(), "Cannot prepend to {} with value of type {}", lhs_t, rhs_t);
                b->set_sema_errored();
                break;
            }

            // Generate following pseudo-code:
            //   dynarray_grow b->lhs();
            //   memmove b->lhs().data[1], b->lhs().data[0], b->lhs().size;
            //   @b->lhs().data := b->rhs();
            //   b->lhs().size += 1;

            auto dyn_data = new (mod) MemberAccessExpr(b->lhs(), "data", {});
            auto dyn_size = new (mod) MemberAccessExpr(b->lhs(), "size", {});

            // dynarray_grow b->lhs();
            auto grow_if = new (mod) CallExpr(named_template("dynarray_grow"), {b->lhs()}, {});

            // memmove b->lhs().data[1], b->lhs().data, b->lhs().size();
            auto memmove_ref
                = new (mod) NameRefExpr("memmove", mod.global_scope(), {});
            auto memmove_dest
                = new (mod) BinaryExpr(TokenKind::Subscript, dyn_data, new (mod) IntegerLiteral(1, {}), {});
            auto memmove_source
                = dyn_data;
            auto memmove_size
                = dyn_size;
            auto call_memmove = new (mod) CallExpr(
                memmove_ref,
                {memmove_dest, memmove_source, memmove_size},
                {}
            );

            // @b->lhs().data := b->rhs();
            auto dereference_subscript = new (mod) UnaryExpr(TokenKind::Dereference, dyn_data, false, {});
            auto assign = new (mod) BinaryExpr(TokenKind::ColonEq, dereference_subscript, b->rhs(), {});

            // b->lhs().size += 1;
            auto update_size = new (mod) BinaryExpr(
                TokenKind::PlusEq,
                dyn_size,
                new (mod) IntegerLiteral(1, {}),
                {}
            );

            *expr_ptr = new (mod) BlockExpr({grow_if, call_memmove, assign, update_size}, b->location());
            (void) Analyse(expr_ptr);
        } break;

        case TokenKind::And:
        case TokenKind::Or: {
            LValueToRValue(&b->lhs());
            LValueToRValue(&b->rhs());

            /// Both types must be integers or booleans.
            if (not lhs_t->is_integer(true) or not rhs_t->is_integer(true)) {
                Error(b->location(), "Cannot perform logical operation on {} and {}", lhs_t, rhs_t);
                b->set_sema_errored();
                return;
            }

            /// Convert both operands to booleans.
            if (not Convert(&b->lhs(), Type::Bool)) {
                Error(
                    b->location(),
                    "Binary logical operator {} on {} and {}: cannot convert lhs, of type {}, to {}",
                    ToString(b->op()),
                    lhs_t,
                    rhs_t,
                    lhs_t,
                    Type::Bool
                );
                b->set_sema_errored();
                return;
            }
            if (not Convert(&b->rhs(), Type::Bool)) {
                Error(
                    b->location(),
                    "Binary logical operator {} on {} and {}: cannot convert rhs, of type {}, to {}",
                    ToString(b->op()),
                    lhs_t,
                    rhs_t,
                    lhs_t,
                    Type::Bool
                );
                b->set_sema_errored();
                return;
            }

            /// The result type is bool.
            b->type(Type::Bool);
        } break;

        /// Pointer or array subscript.
        case TokenKind::Subscript: {
            (void) Convert__RemoveReferences(&b->lhs());
            auto* ty = b->lhs()->type();

            if (is<ArrayViewType, DynamicArrayType>(ty)) {
                // rewrite lhs[rhs] as lhs.data[rhs]

                auto member_access = new (mod) MemberAccessExpr(b->lhs(), "data", {});
                auto subscript = new (mod) BinaryExpr(TokenKind::Subscript, member_access, b->rhs(), {});

                // TODO: if (bounds_check)...
                // insert: if rhs >= size, exit 1;
                auto size_member_access = new (mod) MemberAccessExpr(b->lhs(), "size", {});
                auto condition = new (mod) BinaryExpr(TokenKind::Ge, b->rhs(), size_member_access, {});
                auto exit_ref = new (mod) NameRefExpr("exit", mod.global_scope(), {});
                auto status_literal = new (mod) IntegerLiteral(1, {});
                auto then = new (mod) CallExpr(exit_ref, {status_literal}, {});
                auto if_ = new (mod) IfExpr(condition, then, nullptr, {});

                auto block = new (mod) BlockExpr({if_, subscript}, {});

                *expr_ptr = block;
                (void) Analyse(expr_ptr);
                return;
            }

            if (not is<PointerType, ArrayType>(ty)) {
                // TODO: if (function-exists "_GlintOpOverloadLBracket") -> do that
                // auto functions = mod.function(fmt::format("_XGlintOpOverload{}", b->op()));
                // if (not functions.empty()) {
                //     // overload of operator exists, just call it.
                // }

                Error(
                    b->location(),
                    "LHS of subscript must be a pointer or array, but was {}",
                    b->lhs()->type()
                );
                b->set_sema_errored();
                return;
            }

            /// Result type is the pointer type or a pointer to the array element.
            b->type(is<PointerType>(ty) ? ty : Ptr(as<ArrayType>(ty)->element_type()));

            // An lvalue to a pointer subscript means we have to do lvalue to rvalue
            // conversion on that pointer, so we can subscript from it's value (not
            // from the location the pointer is at).
            if (is<PointerType>(ty) and b->lhs()->is_lvalue())
                LValueToRValue(&b->lhs());

            /// The RHS must be an integer.
            LValueToRValue(&b->rhs());
            if (not Convert(&b->rhs(), Type::Int)) {
                Error(b->rhs()->location(), "RHS of subscript must be an integer");
                b->set_sema_errored();
                return;
            }

            /// If it is an integer, try to evaluate it for bounds checking.
            if (
                auto* arr = cast<ArrayType>(ty);
                arr and arr->size() and arr->size()->ok() and arr->size()->kind() == Expr::Kind::EvaluatedConstant
            ) {
                EvalResult res;
                if (b->rhs()->evaluate(context, res, false)) {
                    if (
                        res.as_int().is_negative()
                        or res.as_int() >= as<ConstantExpr>(arr->size())->value().as_int().value()
                    ) {
                        Error(b->location(), "Array subscript out of bounds");
                        b->set_sema_errored();
                        break;
                    }

                    /// Since we already have the result, store it for later.
                    b->rhs() = new (mod) ConstantExpr(b->rhs(), res);
                }
            }
        } break;

        /// Pointer arithmetic is handled by the subscript operator,
        /// so these are all just regular arithmetic.
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
        case TokenKind::Plus:
        case TokenKind::Minus:
        case TokenKind::Shl:
        case TokenKind::Shr:
        case TokenKind::BitAND:
        case TokenKind::BitOR:
        case TokenKind::BitXOR: {
            LValueToRValue(&b->lhs());
            LValueToRValue(&b->rhs());

            /// Convert both operands to their common type.
            if (not ConvertToCommonType(&b->lhs(), &b->rhs())) {
                Error(
                    b->location(),
                    "Cannot perform {} on {} and {}; operand types not convertible to common type",
                    ToString(b->op()),
                    lhs_t,
                    rhs_t,
                    lhs_t,
                    rhs_t
                );
                b->set_sema_errored();
                return;
            }

            // Both types must be integers or floats (rather, in the set of real
            // numbers, ℝ)..
            if (
                not (lhs_t->is_integer() or Type::Equal(Type::Float, lhs_t))
                or not (rhs_t->is_integer() or Type::Equal(Type::Float, rhs_t))
            ) {
                // TODO: if (function-exists "_GlintOpOverload<OpString>") -> do that

                Error(
                    b->location(),
                    "Cannot perform {} on {} and {}",
                    ToString(b->op()),
                    lhs_t,
                    rhs_t
                );
                b->set_sema_errored();
                return;
            }

            /// The result type is the common type.
            b->type(lhs_t);
        } break;

        /// Comparisons are all handled the same.
        case TokenKind::Eq:
        case TokenKind::Ne:
        case TokenKind::Lt:
        case TokenKind::Gt:
        case TokenKind::Le:
        case TokenKind::Ge: {
            LValueToRValue(&b->lhs());
            LValueToRValue(&b->rhs());

            /// If both operands are integers, convert them to their common type.
            if (
                ((lhs_t->is_integer() or lhs_t->is_enum())
                 and (rhs_t->is_integer() or rhs_t->is_enum()))
            ) {
                if (not ConvertToCommonType(&b->lhs(), &b->rhs())) {
                    Error(
                        b->location(),
                        "Cannot compare {} and {} (operands type mismatch)",
                        lhs_t,
                        rhs_t
                    );
                    b->set_sema_errored();
                    return;
                }
            }

            /// Bool can only be compared with bool.
            else if (lhs_t->is_bool() and rhs_t->is_bool()) {
                /** No-op **/
            }

            /// If both operands are pointers, they must be the same type.
            else if (lhs_t->is_pointer() and rhs_t->is_pointer()) {
                if (not Type::Equal(lhs_t, rhs_t)) {
                    Error(
                        b->location(),
                        "Cannot compare unrelated pointer types {} and {}",
                        lhs_t,
                        rhs_t
                    );
                }
            }

            /// Other comparisons are not allowed.
            else {
                // TODO: if (function-exists "_GlintOpOverload<OpString>") -> do that
                // Also make sure it returns something convertible to bool.

                Error(
                    b->location(),
                    "Cannot compare {} and {} (types not allowed)",
                    lhs_t,
                    rhs_t
                );
            }

            /// Comparisons return bool.
            b->type(Type::Bool);
        } break;

        /// Assignment.
        case TokenKind::ColonEq: {
            // This removes REFERENCES, _not pointers_. Attempts to produce an lvalue.
            (void) Convert__RemoveReferences(&b->lhs());

            if (not b->lhs()->is_lvalue()) {
                // FIXME: This error message is cryptic at best. What the fuck is someone
                // supposed to do about it? We could at least point them to Glint
                // declaration documentation...
                Error(b->location(), "LHS of assignment must be an lvalue");
                b->set_sema_errored();
                return;
            }

            /// The type of the assignment is the same as the type of the lvalue.
            /// NOTE: As long as the lhs of an assignment is an lvalue, we don’t mark
            /// the assignment as errored, because we know what its type is going to
            /// be, irrespective of whether the assignment is valid or not.
            b->type(lhs_t);

            // Assignment to an lvalue always yields an lvalue.
            b->set_lvalue();

            // Disallow assigning to a sum type directly.
            auto* lhs_type = lhs_t;
            if (auto* sum_type = cast<SumType>(lhs_type)) {
                if (auto* m = cast<MemberAccessExpr>(b->lhs())) {
                    // Use member access to fetch type from sum type
                    lhs_type = sum_type->members().at(m->member()).type;
                } else {
                    // FIXME This isn't perfect, as ideally referencing a sum type anywhere
                    // except a member access should be an error, but we shouldn't have to add
                    // explicit checks absolutely everywhere that the thing we're dealing with
                    // isn't a sum type that isn't a member access.
                    Error(
                        b->lhs()->location(),
                        "Cannot assign to a sum type; access one of it's members using ``.''"
                    );
                    b->set_sema_errored();
                    return;
                }
            }

            // FIXME: THIS SHOULD BE HANDLED BY Convert (but it isn't and I don't yet
            // know why, so, yeah).
            // QUESTION: This needs to be done /before/ updating sum type stuff. That way we
            // actually load the sum type that we are trying to get the value from...
            // LValueToRValue(&b->rhs());

            /// The RHS must be assignable to the LHS.
            if (not Convert(&b->rhs(), lhs_type)) {
                Error(
                    b->rhs()->location(),
                    "Type {} is not convertible to type {}",
                    rhs_t,
                    lhs_type
                );
                return;
            }

        } break;

        // Handled elsewhere
        case TokenKind::ColonColon:
        // NOT a binary operator
        case TokenKind::Invalid:
        case TokenKind::Eof:
        case TokenKind::LParen:
        case TokenKind::RParen:
        case TokenKind::RBrack:
        case TokenKind::LBrace:
        case TokenKind::RBrace:
        case TokenKind::BangLBrace:
        case TokenKind::Comma:
        case TokenKind::Colon:
        case TokenKind::Semicolon:
        case TokenKind::Dot:
        case TokenKind::Tilde:
        case TokenKind::Exclam:
        case TokenKind::At:
        case TokenKind::Hash:
        case TokenKind::PlusPlus:
        case TokenKind::MinusMinus:
        case TokenKind::StarStar:
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
        case TokenKind::Int:
        case TokenKind::UInt:
        case TokenKind::Float:
        case TokenKind::ArbitraryInt:
        case TokenKind::Sizeof:
        case TokenKind::Alignof:
        case TokenKind::Has:
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
        case TokenKind::Match:
        case TokenKind::Switch:
        case TokenKind::Print:
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
        case TokenKind::ByteLiteral:
        case TokenKind::Template:
        case TokenKind::Typeof:
        case TokenKind::Ampersand:
        case TokenKind::Pipe:
        case TokenKind::Caret:
        case TokenKind::Apply:
        case TokenKind::BitNOT:
        case TokenKind::Continue:
        case TokenKind::Break:
            Diag::ICE("Invalid binary operator '{}'", ToString(b->op()));
            LCC_UNREACHABLE();
    }

#undef lhs_t
#undef rhs_t
}

// `100 x;` -> 100 * x
//   CallExpr(
//       ConstantExpr 100
//       NameRefExpr x
//   )
// becomes
//   BinaryExpr(
//       '*'
//       ConstantExpr 100
//       NameRefExpr x
// )
//
// `100 x y` -> 100 * x * y
//   CallExpr(
//       ConstantExpr 100
//       NameRefExpr x
//       NameRefExpr y
//   )
// becomes
//   BinaryExpr(
//       '*'
//       ConstantExpr 100
//       BinaryExpr(
//           NameRefExpr x
//           NameRefExpr y
//       )
// )
void lcc::glint::Sema::AnalyseCall_Integer(Expr** expr_ptr, CallExpr* expr) {
    LCC_ASSERT(expr_ptr and *expr_ptr and expr);
    LCC_ASSERT(
        expr->callee()->type()->is_integer(),
        "Invalid arguments of call to \"analyse integer call\" function"
    );

    // NOTE: Call of integer with zero arguments by deproceduring should not
    // be valid syntax, but this handles `100();` just in case.
    if (expr->args().empty() and not HasSideEffects(expr)) {
        Warning(
            expr->location(),
            "Expression result unused"
        );
        return;
    }

    auto* rhs = expr->args().back();
    // Basically, I'm trying to get i to equal the index behind the back of
    // args(), that way we can fold the back two elements into one.
    for (auto i = (isz) expr->args().size() - 2; i >= 0; --i) {
        auto* lhs = expr->args().at((usz) i);
        rhs = new (mod) BinaryExpr(
            TokenKind::Star,
            lhs,
            rhs,
            {lhs->location(), rhs->location()}
        );
    }

    *expr_ptr = new (mod) BinaryExpr(
        TokenKind::Star,
        expr->callee(),
        rhs,
        expr->location()
    );

    (void) Analyse(expr_ptr);
}

void lcc::glint::Sema::AnalyseCall_Type(Expr** expr_ptr, CallExpr* expr) {
    LCC_ASSERT(
        is<TypeExpr>(expr->callee())
            or (is<NameRefExpr>(expr->callee())
                and as<NameRefExpr>(expr->callee())->target()
                and is<TypeDecl>(
                    as<NameRefExpr>(expr->callee())->target()
                )),
        "Invalid arguments of call to \"analyse type call\" function"
    );

    Type* called_type{};
    if (is<TypeExpr>(expr->callee()))
        called_type = as<TypeExpr>(expr->callee())->contained_type();
    else
        called_type = as<TypeDecl>(as<NameRefExpr>(expr->callee())->target())->type();
    LCC_ASSERT(called_type);

    // Calling a type expression with a single compound literal argument is a
    // declaration that the compound literal is expected to be of that type.
    if (
        expr->args().size() == 1
        and is<CompoundLiteral>(expr->args().at(0))
    ) {
        // Replace the type expression with the now-properly-typed compound
        // literal.
        *expr_ptr = new (mod) CompoundLiteral(
            as<CompoundLiteral>(expr->args().at(0))->values(),
            expr->location(),
            called_type
        );
        // Perform conversions (like a compound literal with a single unnamed
        // argument being converted to it's argument expression).
        (void) Analyse(expr_ptr);
        return;
    }

    for (auto*& arg : expr->args())
        (void) Analyse(&arg);

    /// If any of the arguments errored, we do too.
    if (rgs::any_of(expr->args(), &Expr::sema_errored)) {
        expr->set_sema_errored();
        return;
    }

    if (expr->args().size() == 1) {
        // Calling a type expression with a single argument is a declaration that
        // the expression is expected to be of that type.
        LCC_ASSERT(
            not is<CompoundLiteral>(expr->args().at(0)),
            "Call of type expression with single compound literal argument should have been handled above"
        );

        // Do conversions like lvalue to rvalue and stuffs.
        (void) Convert(&expr->args().at(0), called_type);

        *expr_ptr = new (mod) CastExpr(
            expr->args().at(0),
            called_type,
            CastKind::HardCast,
            expr->location()
        );
    } else {
        *expr_ptr = new (mod) CompoundLiteral(
            expr->args(),
            expr->location(),
            called_type
        );
    }
    return;
}

namespace lcc::glint::detail {
auto expand_template_parameter_references_type(const TemplateExpr* t, const std::vector<Expr*> args, Type** current_type) -> void {
    // Update members of structs and such for type expressions.
    if (auto t_name = cast<NamedType>(*current_type)) {
        // The current type is the argument
        auto found_it = rgs::find_if(
            t->params(),
            [&](auto p) { return t_name->name() == p.name; }
        );
        if (found_it != t->params().end()) {
            auto parameter_index = std::distance(t->params().begin(), found_it);
            // TODO?: Ensure template parameter's constraint type is "valid" (i.e.
            // "expr" or "type" with argument being a type expression).
            auto argument = args.at(usz(parameter_index));
            *current_type = as<TypeExpr>(argument)->contained_type();
        }
    } else {
        for (auto t_child : (*current_type)->types_ref())
            expand_template_parameter_references_type(t, args, t_child);
    }
};
auto expand_template_parameter_references(const TemplateExpr* t, const std::vector<Expr*> args, Expr** current_expr) -> void {
    // Fixup expression's type, if necessary.
    if (auto e = cast<TypedExpr>(*current_expr)) {
        expand_template_parameter_references_type(t, args, e->type_ref());
    }
    // Fixup types within type expressions.
    if (auto e_type = cast<TypeExpr>(*current_expr)) {
        expand_template_parameter_references_type(t, args, e_type->contained_type_ref());
    }

    // Expand template using call arguments as template arguments.
    // Replace reference to parameter with argument
    if (auto e_name = cast<NameRefExpr>(*current_expr)) {
        // The current expression is the argument
        auto found_it = rgs::find_if(
            t->params(),
            [&](auto p) { return e_name->name() == p.name; }
        );
        if (found_it != t->params().end()) {
            auto parameter_index = std::distance(t->params().begin(), found_it);
            auto argument = args.at(usz(parameter_index));
            *current_expr = argument;
        }
    } else {
        // Search template body for template parameter references and fix them up
        // into their actual argument.
        for (auto e : (*current_expr)->children_ref())
            expand_template_parameter_references(t, args, e);
    }
};
} // namespace lcc::glint::detail

void lcc::glint::Sema::AnalyseCall_Template(Expr** expr_ptr, CallExpr* expr) {
    LCC_ASSERT(
        is<TemplateExpr>(expr->callee())
            or ( //
                is<NameRefExpr>(expr->callee())
                and is<VarDecl>(as<NameRefExpr>(expr->callee())->target())
                and is<TemplateExpr>(as<VarDecl>(as<NameRefExpr>(expr->callee())->target())->init())
            ),
        "Invalid arguments of call to \"analyse template call\" function"
    );

    // "Place" expr->args() within CLONE of template body

    TemplateExpr* t = cast<TemplateExpr>(expr->callee());
    if (not t)
        t = as<TemplateExpr>(
            as<VarDecl>(as<NameRefExpr>(expr->callee())->target())->init()
        );

    // Ensure types of arguments apply to constraint types given by template
    // parameters.
    if (expr->args().size() != t->params().size()) {
        Error(
            expr->location(),
            "Incorrect number of arguments for template expansion. Expected {} instead of {}",
            t->params().size(),
            expr->args().size()
        );
        expr->set_sema_errored();
    }
    for (usz i = 0, end = std::min(expr->args().size(), t->params().size()); i < end; i++) {
        auto param_t = t->params().at(i).type;
        // Any expression is valid for "expr" compile-time type.
        // TODO: Should "expr" type cover type expressions? I can't think of a
        // time where you could use either a type expression or a different
        // expression in the same place...
        if (
            auto n = cast<NamedType>(param_t);
            n and n->name() == "expr"
        ) continue;

        // Only type expressions are valid for "type" compile-time type.
        if (
            auto n = cast<NamedType>(param_t);
            n and n->name() == "type"
        ) {
            if (not is<TypeExpr>(expr->args().at(i))) {
                Error(
                    expr->args().at(i)->location(),
                    "Expected argument to be a type expression during template expression, but instead got {}",
                    expr->args().at(i)->string(context->option_use_colour())
                );
                expr->set_sema_errored();
                return;
            }
            continue;
        }

        if (not Convert(expr->args().data() + i, t->params().at(i).type)) {
            // got
            auto* from = expr->args().at(i)->type();
            // expected
            auto* to = t->params().at(i).type;

            if (from->is_dynamic_array() and to->is_array()) {
                Error(
                    expr->args().at(i)->location(),
                    "Creation of a fixed array has to be done manually, with elements copied in from a dynamic array.\n"
                    "This way, /you/ can ensure that /all/ of the array's elements are initialised.\n",
                    from,
                    to
                );
                expr->set_sema_errored();
            } else {
                Error(
                    expr->args().at(i)->location(),
                    "Type of argument {} is not convertible to parameter type {}",
                    from,
                    to
                );
                expr->set_sema_errored();
            }
            return;
        }
    }

    // Clone template body...
    auto body = Expr::Clone(mod, context, t->body());

    // Replace template parameters with template arguments.
    detail::expand_template_parameter_references(t, expr->args(), &body);

    // Now that the body has been expanded, it's location is actually the
    // location of the call expression that it expanded from (not the template
    // it was expanded from).
    // TODO: We probably want to keep track of expanded templates, at least
    // for location purposes in diagnostics.
    body->location((*expr_ptr)->location());
    // Replace call with expanded template body
    *expr_ptr = body;
    // Analyse expanded template body
    if (not Analyse(expr_ptr)) {
        (*expr_ptr)->set_sema_errored();
        return;
    }
}

auto lcc::glint::Sema::AnalyseOverload(OverloadSet* expr, std::vector<Expr*> args) -> Result<FuncDecl*> {
    for (auto*& arg : args) (void) Analyse(&arg);

    // If any of the arguments errored, we can’t resolve this.
    if (rgs::any_of(args, &Expr::sema_errored)) {
        expr->set_sema_errored();
        // TODO: Er, ideally we would be able to return not a diagnostic but also
        // not a func decl...
        return Error(expr->location(), "Invalid argument of overload set");
    }

    std::vector<FuncDecl*> O = expr->overloads();
    std::vector<FuncDecl*> O_unchanged = O;

    // *** 0
    //
    // Skip anything that is not a function reference, or any function
    // references previously resolved.

    // *** 1
    //
    // Collect all functions with the same name as the function being
    // resolved into an *overload set* O. We cannot filter out any
    // functions just yet.
    //
    // If the overload set size is zero, it is an error (no function may be resolved).
    //
    // It is advisable to ensure any invariants you require across function
    // overloads, given the semantics of the language. For example, in
    // Intercept, all overloads of a function must have the same return type.

    // *** 2
    //
    // If the parent expression is a call expression, and the function being
    // resolved is the callee of the call, then:

    // **** 2a
    //
    // Typecheck all arguments of the call that are not unresolved function
    // references themselves. Note: This takes care of resolving nested calls.

    // **** 2b
    //
    // Remove from O all functions that have a different number of
    // parameters than the call expression has arguments.
    std::vector<FuncDecl*> invalid{};
    for (auto* f : O) {
        if (f->param_types().size() != args.size()) {
            // Note(
            //     f->location(),
            //     "Candidate {} removed because parameter count doesn't match given arguments: {} vs {}",
            //     f->type(),
            //     f->param_types().size(),
            //     args.size()
            // );
            invalid.emplace_back(f);
        }
    }
    for (auto* f : invalid) std::erase(O, f);
    invalid.clear();

    // **** 2c
    //
    // Let A_1, ... A_n be the arguments of the call expression.
    //
    // For candidate C in O, let P_1, ... P_n be the parameters of C. For each
    // argument A_i of the call, iff it is not an unresolved function, check
    // if it is convertible to P_i. Remove C from O if it is not. Note down
    // the number of A_i’s that required a (series of) implicit conversions to
    // their corresponding P_i’s. Also collect unresolved function references.

    // For candidate C in O,
    for (auto* C : O) {
        // let P_... be the parameters of C.
        for (auto* P : C->param_types()) {
            // For each argument A_i,
            for (auto*& A : args) {
                // check if it is convertible to P_i.
                auto conversion_score = TryConvert(&A, P);
                // TODO: Record score to choose lowest if multiple in overload set at the
                // end. Currently approximated via type equal check.
                if (not conversion_score.possible()) {
                    // Note(
                    //     A->location(),
                    //     "Argument type {} is not convertible to parameter type {}\n",
                    //     A->type(),
                    //     P
                    // );

                    // Remove C from O if it is not.
                    invalid.emplace_back(C);
                }
            }
        }
    }
    for (auto* f : invalid) std::erase(O, f);
    invalid.clear();

    // **** 2e
    //
    // If there are unresolved function references:

    // ***** 2eα
    //
    // Collect their overload sets.

    // ***** 2eβ
    // Remove from O all candidates C that do no accept any overload of this
    // argument as a parameter.

    // ***** 2eγ
    //
    // Remove from O all functions except those with the least number of
    // implicit conversions as per step 2d.

    // ***** 2eδ
    //
    // Resolve the function being resolved.

    // ***** 2eε
    //
    // For each argument, remove from its overload set all candidates that are
    // not equivalent to the type of the corresponding parameter of the
    // resolved function.

    // ***** 2eζ
    //
    // Resolve the argument.

    // **** 2f
    //
    // Remove from O all functions except those with the least number of
    // implicit conversions as per step 2d.

    // *** 3
    //
    // Otherwise, depending on the type of the parent expression:

    // **** 3a
    //
    // If the parent expression is a unary prefix expression with operator
    // address-of, then replace the parent expression with the unresolved
    // function and go to step 2/3 depending on the type of the new parent.

    // **** 3b
    //
    // If the parent expression is a declaration, and the lvalue is not of
    // function pointer type, this is a type error. Otherwise, remove from O
    // all functions that are not equivalent to the lvalue being assigned to.

    // **** 3c
    //
    // If the parent expression is an assignment expression, then if we are
    // the LHS, then this is a type error, as we cannot assign to a function
    // reference.
    //
    // If the lvalue is not of function pointer type, this is a type error.
    //
    // Otherwise, remove from O all functions that are not equivalent to the lvalue being assigned to.

    // **** 3d
    //
    // If the parent expression is a return expression, and the return type of
    // the function F containing that return expression is not of function
    // pointer type, this is a type error. Otherwise, remove from O all
    // functions that are not equivalent to the return type of F.

    // **** 3e
    //
    // If the parent expression is a cast expression, and the result type of
    // the cast is a function or function pointer type, remove from O all
    // functions that are not equivalent to that type.

    // **** 3f
    //
    // Otherwise, do nothing.

    // FIXME: See step 2c for more info.
    // This is a last hail-mary attempt to narrow down an overload set not
    // just to convertible arguments, but to ones that are exactly equal.
    // But don't remove all of them!
    if (O.size() > 1) {
        for (auto* C : O) {
            for (auto* P : C->param_types()) {
                for (auto* A : args) {
                    if (not Type::Equal(A->type(), P)) {
                        // Note(
                        //     A->location(),
                        //     "Argument type {} is not equal to parameter type {}\n",
                        //     A->type(),
                        //     P
                        // );

                        // Remove C from O if it is not.
                        invalid.emplace_back(C);
                    }
                }
            }
        }
        // Only remove if we won't be removing all of them.
        if (invalid.size() != O.size())
            for (auto* f : invalid) std::erase(O, f);
        invalid.clear();
    }

    // *** 4
    //
    // Resolve the function reference.
    //
    // For the most part, entails finding the one function that is still
    // marked viable in the overload set.

    if (not invalid.empty()) {
        Diag::ICE("Overload resolution has leftover invalid. This usually means the developer has forgotten to remove them from the overload set, so make sure that is happening and then clear the invalid list.");
        LCC_UNREACHABLE();
    }

    if (O.size() != 1) {
        Diag e;
        if (O.empty()) {
            e = Error(
                expr->location(),
                "Given arguments {} didn't match any of the possible candidates in the overload set",
                fmt::join(
                    vws::transform(args, [&](auto A) { return fmt::format("{}", *A->type()); }),
                    ", "
                )
            );
            expr->set_sema_errored();
        } else {
            e = Error(
                expr->location(),
                "Unresolved function overload: ambiguous, here are the possible candidates"
            );
            expr->set_sema_errored();
        }

        for (auto* C : O_unchanged)
            e.attach(Note(C->location(), "Candidate defined here"));

        return e;
    }

    return O.at(0);
}

void lcc::glint::Sema::AnalyseCall(Expr** expr_ptr, CallExpr* expr) {
    /// If the callee is a name ref, check for builtins first.
    if (auto* name = cast<NameRefExpr>(expr->callee())) {
        auto n = name->name();

        /// Check if this is the name of a builtin.
        static const StringMap<IntrinsicKind> builtin_names{
            {"__builtin_debugtrap", IntrinsicKind::BuiltinDebugtrap},
            {"__builtin_filename", IntrinsicKind::BuiltinFilename},
            {"__builtin_inline", IntrinsicKind::BuiltinInline},
            {"__builtin_line", IntrinsicKind::BuiltinLine},
            {"__builtin_memcpy", IntrinsicKind::BuiltinMemCopy},
            {"__builtin_memset", IntrinsicKind::BuiltinMemSet},
            {"__builtin_syscall", IntrinsicKind::BuiltinSyscall},
        };
        if (auto kind = builtin_names.find(n); kind != builtin_names.end()) {
            /// We copy the arguments and leave the original expression unchanged
            /// since this node may be references in multiple places, all of which
            /// may need to be patched, and there is no good way of doing that
            /// without copying each use individually.
            auto* intrinsic = new (mod) IntrinsicCallExpr(
                kind->second,
                expr->args()
            );

            /// Make sure to actually analyse this intrinsic, as it will otherwise
            /// just be marked as done without actually being analysed.
            *expr_ptr = intrinsic;
            (void) Analyse(expr_ptr);
            return;
        }

        if (n == "__glintprint") {
            // This is the group of expressions we will be replacing the print call
            // expression with.
            std::vector<Expr*> exprs{};
            for (auto& arg : expr->args()) {
                if (not Analyse(&arg)) {
                    expr->set_sema_errored();
                    return;
                }

                // print x:void -> ERROR
                if (Type::Equal(arg->type(), Type::Void)) {
                    arg->print(context->option_use_colour());
                    expr->set_sema_errored();
                    Error(
                        arg->location(),
                        "Argument to print has void type; no formatter available"
                    );
                    continue;
                }

                // print x:byte -> putchar x
                bool arg_is_byte = arg->type()->is_byte();
                if (arg_is_byte) {
                    auto print_call = new (mod) CallExpr(
                        new (mod) NameRefExpr(
                            "putchar",
                            mod.global_scope(),
                            expr->location()
                        ),
                        {arg},
                        expr->location()
                    );
                    exprs.emplace_back(print_call);
                    continue;
                }

                // Just call puts on byte pointers
                // print x:byte.ptr -> puts x
                bool arg_is_pointer_to_byte = arg->type()->is_pointer()
                                          and Type::Equal(arg->type()->elem(), Type::Byte);
                if (arg_is_pointer_to_byte) {
                    auto puts_call = new (mod) CallExpr(
                        new (mod) NameRefExpr(
                            "puts",
                            mod.global_scope(),
                            expr->location()
                        ),
                        {arg},
                        expr->location()
                    );
                    exprs.emplace_back(puts_call);
                    continue;
                }

                // Don't format dynamic byte arrays...
                // print x:[byte] -> puts x.data
                bool arg_is_dynamic_array_of_byte
                    = arg->type()->strip_references()->is_dynamic_array()
                  and Type::Equal(arg->type()->strip_references()->elem(), Type::Byte);
                if (arg_is_dynamic_array_of_byte) {
                    auto print_call = new (mod) CallExpr(
                        named_template("print__putchar_each"),
                        {arg},
                        {}
                    );
                    exprs.emplace_back(print_call);
                    continue;
                }

                // Don't format fixed byte arrays (like string literals)
                // print x:[byte 4] -> puts x[0]
                bool arg_is_fixed_array_of_byte
                    = arg->type()->strip_references()->is_array()
                  and Type::Equal(arg->type()->strip_references()->elem(), Type::Byte);
                if (arg_is_fixed_array_of_byte) {
                    auto size = as<ArrayType>(arg->type()->strip_references())->dimension();
                    // Don't print (implicit) trailing NULL byte for string literals...
                    if (is<StringLiteral>(arg))
                        size -= 1;
                    auto print_call = new (mod) CallExpr(
                        named_template("putchar_each"),
                        {arg,
                         new (mod) IntegerLiteral(size, {})},
                        expr->location()
                    );
                    exprs.emplace_back(print_call);
                    continue;
                }

                // Handle array view of byte.
                // print x:[byte view] -> puts x.data
                bool arg_is_view_of_byte
                    = arg->type()->strip_references()->is_view()
                  and Type::Equal(arg->type()->strip_references()->elem(), Type::Byte);
                if (arg_is_view_of_byte) {
                    auto print_call = new (mod) CallExpr(
                        named_template("print__putchar_each"),
                        {arg},
                        {}
                    );
                    exprs.emplace_back(print_call);
                    continue;
                }

                // Handle enum value (print name of value).
                bool arg_is_enum
                    = is<EnumType>(arg->type()->strip_references());
                if (arg_is_enum) {
                    auto enum_type = as<EnumType>(arg->type()->strip_references());
                    LCC_ASSERT(enum_type->enumerators().size());

                    // If the value is known at compile time, just print the name of the enum
                    // that we know it is.
                    if (
                        auto enum_expression = cast<ConstantExpr>(arg);
                        enum_expression
                    ) {
                        auto enum_value = enum_expression->value().as_int();

                        auto e_decl = enum_type->enumerator_by_value(enum_value);
                        LCC_ASSERT(e_decl and e_decl->name().size());

                        auto string_literal = new (mod) StringLiteral(
                            mod,
                            e_decl->name(),
                            e_decl->location()
                        );

                        auto print_call = new (mod) CallExpr(
                            named_template("putchar_each"),
                            {string_literal,
                             new (mod) IntegerLiteral(e_decl->name().size(), {})},
                            arg->location()
                        );

                        exprs.emplace_back(print_call);
                        continue;
                    }

                    // Map dynamic enum value to it's corresponding string literal.
                    // TODO: Use `switch` once we have it. Binary search?
                    //
                    // For every enumerator in the type...
                    //     if arg = enumerator, puts "enumerator"[0];

                    // The failure case (value is not equal to any possible enumerator)
                    std::string str_value = "Glint Runtime Error: attempt to print enum value that is not associated with a declared enumerator";
                    if (arg->location().seekable(context)) {
                        auto locinfo = arg->location().seek_line_column(context);
                        str_value = fmt::format(
                            "{}:{}:{}: {}",
                            locinfo.line,
                            locinfo.col,
                            context
                                ->files()
                                .at(arg->location().file_id)
                                ->path()
                                .lexically_normal()
                                .string(),
                            str_value
                        );
                    }
                    auto str = new (mod) StringLiteral(mod, str_value, {});
                    auto sub_str = new (mod) BinaryExpr(
                        TokenKind::Subscript,
                        str,
                        new (mod) IntegerLiteral(0, {}),
                        {}
                    );
                    auto invalid_print = new (mod) CallExpr(
                        new (mod) NameRefExpr(
                            "puts",
                            mod.global_scope(),
                            expr->location()
                        ),
                        {sub_str},
                        {}
                    );

                    auto status_literal = new (mod) IntegerLiteral(1, {});
                    auto exit_call = new (mod) CallExpr(
                        new (mod) NameRefExpr(
                            "exit",
                            mod.global_scope(),
                            {}
                        ),
                        {status_literal},
                        {}
                    );

                    Expr* otherwise = new (mod) GroupExpr({invalid_print, exit_call}, {});

                    for (auto e : enum_type->enumerators()) {
                        auto test_value = new (mod) ConstantExpr(
                            enum_type->underlying_type(),
                            EvalResult(e->value()),
                            {}
                        );
                        auto eq_condition = new (mod) BinaryExpr(
                            TokenKind::Eq,
                            test_value,
                            arg,
                            {}
                        );

                        auto string_literal = new (mod) StringLiteral(
                            mod,
                            e->name(),
                            e->location()
                        );

                        auto print_call = new (mod) CallExpr(
                            named_template("putchar_each"),
                            {string_literal,
                             new (mod) IntegerLiteral(e->name().size(), {})},
                            arg->location()
                        );

                        auto if_value_matches = new (mod) IfExpr(
                            eq_condition,
                            print_call,
                            otherwise,
                            {}
                        );

                        otherwise = if_value_matches;
                    }
                    LCC_ASSERT(otherwise);

                    exprs.emplace_back(otherwise);
                    continue;
                }

                // Otherwise, format argument
                // print x -> { tmp :: format x; puts tmp.data; -tmp; }
                auto format_call = new (mod) CallExpr(
                    new (mod) NameRefExpr(
                        "format",
                        mod.top_level_scope(),
                        expr->location()
                    ),
                    {arg},
                    expr->location()
                );
                auto format_decl_name = mod.unique_name("fmttmp_");
                auto scope = name->scope();
                auto format_decl = scope->declare(
                    context,
                    std::move(format_decl_name),
                    new (mod) VarDecl(
                        format_decl_name,
                        new (mod) DynamicArrayType(Type::Byte, nullptr),
                        format_call,
                        &mod,
                        Linkage::LocalVar,
                        arg->location()
                    )
                );
                // formattmp :[byte] = format arg;
                exprs.emplace_back(*format_decl);

                // template(dynarray : expr)
                //   cfor
                //       i :: 0;
                //       i < dynarray.size;
                //       i += 1;
                //     putchar @dynarray[i];
                auto print_call = new (mod) CallExpr(
                    named_template("print__putchar_each"),
                    {*format_decl},
                    {}
                );
                exprs.emplace_back(print_call);

                // -formattmp;
                auto unary = new (mod) UnaryExpr(
                    TokenKind::Minus,
                    new (mod) NameRefExpr(
                        format_decl->name(),
                        scope,
                        {}
                    ),
                    false,
                    {}
                );

                exprs.emplace_back(unary);
            }

            *expr_ptr = new (mod) BlockExpr(exprs, expr->location());
            (void) Analyse(expr_ptr);
            return;
        }

        // Otherwise, resolve the NameRefExpr to it's target.
        // Notably, this does NOT analyse the target; it just finds it.
        AnalyseNameRef(name);
    }

    // If the callee is a struct template, this is a struct template
    // expansion.
    if (
        (
            is<TypeExpr>(expr->callee())
            and is<TemplatedStructType>(as<TypeExpr>(expr->callee())->contained_type())
        )
        or ( //
            is<NameRefExpr>(expr->callee())
            and as<NameRefExpr>(expr->callee())->target()
            and is<VarDecl>(as<NameRefExpr>(expr->callee())->target())
            and as<VarDecl>(as<NameRefExpr>(expr->callee())->target())->init()
            and is<TemplatedStructType>(as<VarDecl>(as<NameRefExpr>(expr->callee())->target())->init()->type())
        )
    ) {
        auto t = cast<TemplatedStructType>(
            as<TypeExpr>(expr->callee())->contained_type()
        );
        if (not t) {
            t = as<TemplatedStructType>(
                as<VarDecl>(as<NameRefExpr>(expr->callee())->target())->init()->type()
            );
        }

        // struct(t : type) { v : t } int;
        //   -> struct { v : int };
        // via:
        // (template(t : type) struct { v : t }) int;
        //                     ^^^^^^^^^^^^^^^^

        // Copy because StructType constructor moves members.
        auto members = t->members();

        auto generated_struct
            = new (mod) StructType(t->scope(), members, {});
        auto generated_struct_expr
            = new (mod) TypeExpr(mod, generated_struct, {});

        auto generated_template = new (mod) TemplateExpr(
            generated_struct_expr,
            t->params(),
            {}
        );

        // Pass struct template arguments to struct template's underlying template
        // to get a type expression containing the generated struct.

        auto generated_call = new (mod) CallExpr(
            generated_template,
            expr->args(),
            expr->location()
        );
        *expr_ptr = generated_call;
        LCC_ASSERT(Analyse(expr_ptr));

        return;
    }

    /// If analysing the callee fails, we can’t do anything else.
    if (not Analyse(&expr->callee())) {
        expr->set_sema_errored();
        return;
    }

    // If the callee is a template, this is a template expansion.
    // Search Terms: template expansion, expand, ast macro
    if (
        is<TemplateExpr>(expr->callee())
        or ( //
            is<NameRefExpr>(expr->callee())
            and is<VarDecl>(as<NameRefExpr>(expr->callee())->target())
            and as<VarDecl>(as<NameRefExpr>(expr->callee())->target())->init()
            and is<TemplateExpr>(as<VarDecl>(as<NameRefExpr>(expr->callee())->target())->init())
        )
    ) {
        AnalyseCall_Template(expr_ptr, expr);
        return;
    }

    // If the callee is a type expression, this is a type instantiation.
    // TODO: This NameRefExpr check is probably a sign of something more
    // sinister going on, but I can't exactly pinpoint it right now.
    if (
        is<TypeExpr>(expr->callee())
        or ( //
            is<NameRefExpr>(expr->callee())
            and as<NameRefExpr>(expr->callee())->target()
            and is<TypeDecl>(as<NameRefExpr>(expr->callee())->target())
        )
    ) {
        AnalyseCall_Type(expr_ptr, expr);
        return;
    }

    for (auto*& arg : expr->args()) (void) Analyse(&arg);

    // If any of the arguments errored, we can’t resolve this.
    if (rgs::any_of(expr->args(), &Expr::sema_errored)) {
        expr->set_sema_errored();
        return;
    }

    // If the callee is an integer, multiply all the arguments.
    if (auto* callee_ty = expr->callee()->type(); callee_ty->is_integer()) {
        AnalyseCall_Integer(expr_ptr, expr);
        return;
    }

    // If the callee is an overload set, perform overload resolution.
    if (is<OverloadSet>(expr->callee()) or expr->callee()->type()->is_overload_set()) {
        OverloadSet* O{nullptr};
        if (is<OverloadSet>(expr->callee())) {
            O = as<OverloadSet>(expr->callee());
        } else if (expr->callee()->type()->is_overload_set()) {
            LCC_ASSERT(is<NameRefExpr>(expr->callee()));

            auto* set = as<NameRefExpr>(expr->callee())->target();
            LCC_ASSERT(is<OverloadSet>(set));

            O = as<OverloadSet>(set);
        }

        auto resolved = AnalyseOverload(O, expr->args());
        if (not resolved) return;
        expr->callee() = *resolved;
    }

    // If the callee is a function pointer, dereference it.
    if (
        auto* ty = expr->callee()->type();
        ty->is_pointer() and ty->elem()->is_function()
    ) InsertImplicitCast(&expr->callee(), ty->elem());

    // If the type is not already a function type, we can’t call this.
    if (not expr->callee()->type()->is_function()) {
        auto e = Error(
            expr->callee()->location(),
            "Cannot call non-function type {}",
            expr->callee()->type()
        );
        expr->set_sema_errored();

        // So, when writing Glint, if you leave out a semi-colon separator (you
        // devil, you), it can turn a regular expression into a call. So, if we
        // detect that this uncallable callee expression is on one line and the
        // first argument is on an entirely different line, we can suggest adding
        // a semi-colon after the callee expression.
        if (not expr->args().empty()) {
            auto callee_location = expr->callee()->location();
            auto first_arg_location = expr->args()[0]->location();
            if (
                context
                and Location::on_different_lines(
                    *context,
                    callee_location,
                    first_arg_location
                )
            ) {
                // TODO: I would love to be able to fix this for them and just emit a
                // warning, but here's the issue I ran into: if we just replace *expr_ptr
                // with a block expression with the callee and then the arguments, it
                // often forms an invalid AST as the argument expressions should be
                // inserted /after/ we return the callee expression by itself. So, for
                // this to work, we'd need to replace *expr_ptr with the callee
                // expression, and somehow push the argument expressions onto some sort of
                // stack to fetch from later. Because that is so complicated and bug-prone
                // to implement, I'll wait until I really feel like it.
                e.attach(Note(
                    GetRightmostLocation(expr->callee()),
                    "You probablly forgot a ';' around here somewhere. "
                    "We thought about inserting it for you but got worried you'd get annoyed."
                ));
            }
        }

        return;
    }

    // TODO: If any child type of resolved FuncType contains `auto` somewhere
    // within it, "deduce auto" (replace relevant auto with the relevant
    // argument type from the call site). With our new FuncType, check if it
    // has already been instantiated as a templated function for the called
    // function (we need the name of the called function, as well, or at least
    // some way to uniquely identify it every time); if it has, we should
    // insert a call to that instantiation of the function. If it hasn't, we
    // should instantiate a version of the function and record it in the
    // cache, then insert a call to that instantiation.
    // TODO: This NameRefExpr check is probably a sign of something more
    // sinister going on, but I can't exactly pinpoint it right now.
    if (
        is<TemplatedFuncDecl>(expr->callee())
        or ( //
            is<NameRefExpr>(expr->callee())
            and as<NameRefExpr>(expr->callee())->target()
            and is<TemplatedFuncDecl>(as<NameRefExpr>(expr->callee())->target())
        )
    ) {
        auto templated_declaration = cast<TemplatedFuncDecl>(expr->callee());
        if (not templated_declaration)
            templated_declaration = as<TemplatedFuncDecl>(as<NameRefExpr>(expr->callee())->target());

        auto deduced_function_type = TemplatedFuncDecl::deduce(
            mod,
            templated_declaration->function_type(),
            expr->args()
        );

        const bool debug_templates
            = context and context->has_option("debug-templates");
        if (debug_templates) {
            fmt::print(
                "Deduced function type: {}\n",
                *((Type*) deduced_function_type)
            );
            fmt::print(
                "From call:\n",
                *((Type*) deduced_function_type)
            );
            expr->print(context->option_use_colour());
        }

        auto function_declaration
            = templated_declaration->find_instantiation(deduced_function_type);

        if (not function_declaration) {
            function_declaration = templated_declaration->make_instantiation(
                mod,
                *context,
                deduced_function_type
            );
        }

        LCC_ASSERT(function_declaration);

        expr->callee() = function_declaration;
    }

    // The type of the call is the return type of the function.
    auto* func_type = cast<FuncType>(expr->callee()->type());
    LCC_ASSERT(func_type);
    expr->type(func_type->return_type());

    // Check that there are as many arguments as parameters.
    if (expr->args().size() != func_type->params().size()) {
        Error(
            expr->location(),
            "Incorrect number of arguments for function. Expected {} instead of {}",
            func_type->params().size(),
            expr->args().size()
        );
    }

    /// Check that the arguments are convertible to the parameter types. This
    /// is one of the few places where we allow reference binding, so perform
    /// lvalue-to-rvalue conversion only if the parameter type is not a reference
    /// type. This is all handled transparently by Convert().
    for (usz i = 0, end = std::min(expr->args().size(), func_type->params().size()); i < end; i++) {
        if (
            not Convert(
                expr->args().data() + i,
                func_type->params().at(i).type
            )
        ) {
            // got
            auto* from = expr->args().at(i)->type();
            // expected
            auto* to = func_type->params().at(i).type;

            if (from->is_dynamic_array() and to->is_array()) {
                Error(
                    expr->args().at(i)->location(),
                    "Creation of a fixed array has to be done manually, with elements copied in from a dynamic array.\n"
                    "This way, /you/ can ensure that /all/ of the array's elements are initialised.\n",
                    from,
                    to
                );
            } else {
                std::string fname{};
                if (auto fdecl = cast<FuncDecl>(expr->callee()))
                    fname = fdecl->name();
                else if (
                    auto nameref = cast<NameRefExpr>(expr->callee());
                    nameref and is<Decl>(nameref->target())
                ) fname = as<Decl>(nameref->target())->name();

                Error(
                    expr->args().at(i)->location(),
                    "Type of argument {} is not convertible to parameter type {}{} (parameter {} in function signature {})",
                    from,
                    to,
                    (not fname.empty()) ? fmt::format(" in function {}", fname) : "",
                    i,
                    func_type
                );
            }
        }
    }
}

void lcc::glint::Sema::AnalyseCast(CastExpr* c) {
    /// Implicit casts and lvalue-to-rvalue conversions are only ever created
    /// by sema, so we know they’re fine.
    /// FIXME: The above sounds like a bunch of malarkey made up by an arrogant
    /// mad-man.
    if (c->is_implicit_cast() or c->is_lvalue_to_rvalue()
        or c->is_lvalue_to_ref() or c->is_ref_to_lvalue()) {
        c->set_lvalue(c->is_ref_to_lvalue());
        return;
    }

    /// If analysis of the operand failed, we don’t know its
    /// type and thus have no way of checking whether the cast
    /// makes sense.
    if (not Analyse(&c->operand(), c->type())) return;

    /// If the types are implicitly convertible, then the cast
    /// is fine. If this fails, it will still perform lvalue to
    /// rvalue conversion on the operand, which is exactly what
    /// we want.
    if (Convert(&c->operand(), c->type())) return;

    /// All conversions that rely on references have already been
    /// taken care of by Convert(), so we don’t care about references
    /// anymore at this point.
    ///
    /// Thus, the type we’re casting to must not be a reference type.
    auto* from = c->operand()->type();
    auto* to = c->type();
    if (to->is_reference()) {
        Error(c->location(), "Invalid cast of rvalue to reference type");
        return;
    }

    /// Explicitly casting from enums/integers to integers and
    /// enums/integers to booleans and booleans to integers is allowed.
    if ((from->is_integer(true) or from->is_enum()) and to->is_integer(true)) return;

    /// Casting from pointers to integers and pointers to booleans is allowed.
    if (from->is_pointer() and to->is_integer(true)) return;

    /// Helper to allow only hard casts.
    auto HardCast = [&] {
        if (not c->is_hard_cast()) Error(
            c->location(),
            "Cast from {} to {} is unsafe. If this is intended, use 'as!' instead",
            from,
            to
        );
    };

    /// Hard casts from integers to enums are allowed.
    if (from->is_integer(true) and to->is_enum()) return HardCast();

    /// Hard casts between pointers and from pointers to integers are
    /// allowed. Note that, if the pointers are compatible, the call to
    /// Convert() above will have already taken care of this case, so we
    /// don’t need to check for that here.
    if (to->is_pointer() and (from->is_integer() or from->is_pointer())) return HardCast();

    /// Hard casts between types that have the same size are allowed.
    if (from->size(context) == to->size(context) and c->is_hard_cast()) return;

    /// Any other casts are currently not allowed.
    Error(c->location(), "Invalid cast from {} to {}", from, to);
}

void lcc::glint::Sema::AnalyseIntrinsicCall(Expr** expr_ptr, IntrinsicCallExpr* expr) {
    switch (expr->intrinsic_kind()) {
        case IntrinsicKind::BuiltinDebugtrap: {
            if (not expr->args().empty())
                Error(expr->location(), "__builtin_debugtrap() takes no arguments");
            expr->type(Type::Void);
        } break;

        case IntrinsicKind::BuiltinFilename: {
            if (not expr->args().empty())
                Error(expr->location(), "__builtin_filename() takes no arguments");

            /// Get the name of the file containing this call.
            std::string filename = "<unknown>";
            if (expr->location().seekable(context))
                filename = context->files()[expr->location().file_id]->path().filename().string();

            /// Create a string literal containing the filename.
            auto* str = new (mod) StringLiteral(mod, filename, expr->location());
            expr->type(str->type());
            expr->set_sema_done();
            *expr_ptr = new (mod) ConstantExpr(expr, str);
        } break;

        case IntrinsicKind::BuiltinInline: {
            /// This takes one argument, and it must be a call expression.
            if (expr->args().size() != 1)
                Error(expr->location(), "__builtin_inline() takes exactly one argument");

            /// Analyse the call.
            auto*& call = expr->args().front();
            if (not Analyse(&call)) expr->set_sema_errored();
            if (not is<CallExpr>(call)) Error(
                call->location(),
                "Argument to __builtin_inline() must be a (non-builtin) function call"
            );

            /// Return type is the type of the callee.
            if (call->ok()) expr->type(call->type());
        } break;

        case IntrinsicKind::BuiltinLine: {
            if (not expr->args().empty()) Error(
                expr->location(),
                "__builtin_line() takes no arguments"
            );
            expr->type(Type::Int);
            expr->set_sema_done();

            /// If possible, seek to the location, if not we just insert 0.
            i64 line = 0;
            if (expr->location().seekable(context)) line = i64(expr->location().seek_line_column(context).line);
            *expr_ptr = new (mod) ConstantExpr(expr, line);
        } break;

        case IntrinsicKind::BuiltinMemCopy: {
            /// This takes two pointer and a size argument.
            if (expr->args().size() != 3)
                Error(expr->location(), "__builtin_memcpy() takes exactly three arguments");

            /// Analyse the arguments.
            for (auto*& arg : expr->args()) (void) Analyse(&arg);
            if (not ConvertOrError(&expr->args()[0], Type::VoidPtr)) return;
            if (not ConvertOrError(&expr->args()[1], Type::VoidPtr)) return;
            if (not ConvertOrError(&expr->args()[2], Type::Int)) return;
            LValueToRValue(&expr->args()[0]);
            LValueToRValue(&expr->args()[1]);
            LValueToRValue(&expr->args()[2]);

            /// Unlike C’s memcpy()/memmove(), this returns nothing.
            expr->type(Type::Void);
        } break;

        case IntrinsicKind::BuiltinMemSet: {
            /// This takes two pointer and a size argument.
            if (expr->args().size() != 3)
                Error(expr->location(), "__builtin_memset() takes exactly three arguments");

            /// Analyse the arguments.
            for (auto*& arg : expr->args()) (void) Analyse(&arg);
            if (not ConvertOrError(&expr->args()[0], Type::VoidPtr)) return;
            if (not ConvertOrError(&expr->args()[1], Type::Byte)) return;
            if (not ConvertOrError(&expr->args()[2], Type::Int)) return;
            LValueToRValue(&expr->args()[0]);
            LValueToRValue(&expr->args()[1]);
            LValueToRValue(&expr->args()[2]);

            /// Unlike C’s memset(), this returns nothing.
            expr->type(Type::Void);
        } break;

        case IntrinsicKind::BuiltinSyscall: {
            /// This has 1-7 integer-sized arguments and returns an integer.
            if (expr->args().empty() or expr->args().size() > 7)
                Error(expr->location(), "__builtin_syscall() takes between 1 and 7 arguments");

            /// Arguments must be integers or pointers.
            for (auto*& arg : expr->args()) {
                (void) Analyse(&arg);
                InsertPointerToIntegerCast(&arg);
                if (not ConvertOrError(&arg, Type::Int)) return;
                LValueToRValue(&arg);
            }

            /// Syscalls all return integer.
            expr->type(Type::Int);
        } break;
    }
}

void lcc::glint::Sema::AnalyseNameRef(NameRefExpr* expr) {
    // Look up the thing in its scope, if there is no definition of the symbol
    // in its scope, search its parent scopes until we find one.
    auto* const scope = expr->scope();
    std::vector<Decl*> syms = scope->find_recursive(expr->name());

    // If we’re at the global scope and there still is no symbol, then this
    // symbol is apparently not declared.
    if (syms.empty()) {
        /// Search imported modules here.
        for (const auto& ref : mod.imports()) {
            if (
                expr->name() == ref.name
                or (expr->name() == ref.aliased_name)
            ) {
                // Set expr->target() and expr->type() to something reasonable.
                auto* module_expr = new (mod) ModuleExpr(
                    ref.module,
                    expr->location()
                );
                expr->target(module_expr);
                expr->type(Type::Void);
                return;
            }
        }

        // Attempt to help out the Glint programmer by finding the closest match
        // of an existing declaration to what they typed.
        // NOTE: The more similar two strings are, the more their distances
        // approach zero.
        Decl* least_distance_decl = nullptr;
        size_t least_distance{size_t(-1)};
        for (auto* decl : scope->all_symbols_recursive()) {
            auto distance = utils::optimal_string_alignment_distance(expr->name(), decl->name());
            LCC_ASSERT(
                distance,
                "If distance from '{}' to '{}' was zero, then symbol would have been found."
                " Likely error in distance calculation OR you passed a different name to declare() than the given declaration had.\n",
                expr->name(),
                decl->name()
            );
            if (distance < least_distance) {
                least_distance_decl = decl;
                least_distance = distance;
            }
        }
        // ¡AUTO-SPELLCHECK!
        // For identifiers that are unknown yet so, so close to an existing, valid
        // declaration, we just treat them like they were spelled right,
        // targetting that declaration.
        // This doesn't work well with strings below three characters, as the
        // maximum possible distance is often below or equal to our threshold
        // distance, so we don't apply it to short identifiers.
        // Also, it is confusing when it changes the length, so we require that
        // the replaced declaration has the same length as the given identifier.
        // Basically, this means that the only real possible swap is when two
        // single characters within a word are transposed (acbd instead of abcd).
        if (least_distance == 1
            and expr->name().size() > 2
            and expr->name().size() == least_distance_decl->name().size()) {
            Warning(
                expr->location(),
                "You typed '{}'; we are treating it as '{}' because it's so close",
                expr->name(),
                least_distance_decl->name()
            )
                .attach(Note(
                    least_distance_decl->location(),
                    "Declared here"
                ));
            expr->target(least_distance_decl);
            expr->type(least_distance_decl->type());
            if (least_distance_decl->is_lvalue()) expr->set_lvalue();
            return;
        }

        auto err = Error(
            expr->location(),
            "Unknown symbol '{}' in scope at {}",
            expr->name(),
            scope->chain_string()
        );

        for (auto s = expr->scope(); s; s = s->parent()) {
            std::vector<std::string_view> symbol_names{};
            for (auto sym : s->all_symbols())
                symbol_names.emplace_back(sym->name());
            err.attach(Note(
                s->location(),
                "Searched this scope ({})... {}",
                fmt::ptr(s),
                fmt::join(symbol_names, ", ")
            ));
        }

        // TODO: What is this note, what does it mean, and why is it here? How
        // might one trigger it? To me, the top level scope is always, well, at
        // the top level, above every other scope. So, it should be searched for
        // every scope within a module. I guess this might make sense if you have
        // a top level variable in another module and want it to be global, but...
        // just import the module??? I have no idea what this means or why it is
        // here.
        // If there is a declaration of this variable in the top-level scope, tell
        // the user that they may have forgotten to make it static.
        auto top_level = mod.top_level_scope()->find(expr->name());
        if (not top_level.empty()) {
            err.attach(Note(
                top_level.at(0)->location(),
                "FIXME I HAVE NO IDEA WHAT THIS ERROR MEANS OR WHY ITS HERE SORRY ABOUT THAT!"
            ));
        }

        // The distance between a short name only has a few possibilities, so we
        // lower the maximum distance.
        constexpr usz short_name_distance_max = 1;
        bool short_name
            = least_distance_decl and least_distance_decl->name().size() < 5;

        // It is unlikely the programmer mistyped multiple extra characters in a
        // name /on accident/, so we ensure the lengths are within some distance
        // of each other.
        bool length_close
            = least_distance_decl
          and std::abs(
                  i64(least_distance_decl->name().size() - expr->name().size())
              ) < 3;

        // If there is a short name, ensure it's distance is below or equal to the
        // maximum distance. Without this, things like `bar` get suggested to be
        // replaced with `fas`, and that just doesn't really make sense to me.
        if (least_distance_decl and length_close and (not short_name or least_distance <= short_name_distance_max)) {
            err.attach(Note(
                least_distance_decl->location(),
                "Maybe you meant '{}', defined here?",
                least_distance_decl->name()
            ));
        }

        expr->set_sema_errored();
        return;
    }

    // Either there is exactly one node that is not a function, or, there may
    // be one or more nodes with that name that are functions. In the case of
    // a non-function node, resolve to that node.
    if (not is<FuncDecl>(syms.at(0))) {
        LCC_ASSERT(
            syms.size() == 1,
            "If a symbol resolves to a non-function declaration,"
            " there must only be one declaration associated with the symbol."
            " Glint does not support any other kind of overloading."
        );

        Expr* e = syms.at(0);

        if (not e->ok()) {
            auto err = Error(
                expr->location(),
                "Reference to '{}' before it has been declared (and therefore initialized).\n"
                "Allowing this access would mean allowing potentially uninitialized data to alter your program!",
                expr->name()
            );
            if (e->location().is_valid())
                err.attach(Note(e->location(), "Declared here"));

            // TODO: Suggestions:
            //   1) Move the declaration of 'x' to be before this access.
            //   2) Move the "expression containing this access" to after the
            //      declaration of 'x'.
            //      Where "the expression containing this access" refers to the previous
            //      sibling of the declaration of 'x' that has this access as a child.
            expr->set_sema_errored();
            return;
        }

        if (e->sema() == SemaNode::State::NoLongerViable) {
            Error(
                expr->location(),
                "Reference to a name, {}, that is no longer viable; probably a use-after-free thing",
                expr->name()
            );
        }

        // If sema is in progress for the declaration, and there is a name ref we
        // are trying to resolve that points to the declaration, it means the
        // declared object is being used in it's own initialiser, which doesn't
        // make sense.
        if (e->sema() == SemaNode::State::InProgress) {
            Error(
                expr->location(),
                "Cannot use '{}' in its own initialiser",
                expr->name()
            );
            expr->set_sema_errored();
            return;
        }

        expr->target(syms.at(0));
        expr->type(syms.at(0)->type());
        if (syms.at(0)->is_lvalue())
            expr->set_lvalue();
        return;
    }

    // In the other case, collect all functions with that name and create an
    // overload set for them.
    std::vector<FuncDecl*> overloads{};
    overloads.reserve(syms.size());
    for (auto& sym : syms)
        overloads.emplace_back(as<FuncDecl>(sym));

    // If there is only one function, resolve it directly to that function.
    if (overloads.size() == 1) {
        expr->target(overloads[0]);
        expr->type(overloads[0]->type());
        return;
    }

    // Create a new overload set and analyse it. This will make sure there are
    // no redeclarations etc.
    Expr* overload_set = new (mod) OverloadSet(overloads, expr->location());
    (void) Analyse(&overload_set);
    if (overload_set->sema_errored()) expr->set_sema_errored();

    // The type of an overload set is special because its actual type will depend
    // on the context. Roughly, the `OverloadSet` type is convertible to any
    // of the function types in the set, or pointers to them.
    expr->target(overload_set);
    expr->type(Type::OverloadSet);
}

void lcc::glint::Sema::AnalyseUnary(Expr** expr_ptr, UnaryExpr* u) {
    // No-op if there is an error in the operand.
    if (not Analyse(&u->operand())) {
        u->set_sema_errored();
        return;
    }

    // Unary postfix operators.
    if (u->is_postfix()) {
        /// We currently don’t have postfix operators.
        Diag::ICE("Invalid postfix operator {}", ToString(u->op()));
        LCC_UNREACHABLE();
    }

#define operand_type u->operand()->type()

    // Unary prefix operators.
    switch (u->op()) {
        // Get the address of an lvalue or function.
        case TokenKind::Ampersand: {
            if (not u->operand()->is_lvalue()) {
                // FIXME: Better, non-cryptic error message.
                Error(u->location(), "Cannot take address of rvalue");
                u->set_sema_errored();
                break;
            }

            u->type(Ptr(operand_type));
        } break;

        /// Convert a pointer to an lvalue.
        case TokenKind::Dereference: {
            /// The pointer itself must be an rvalue.
            LValueToRValue(&u->operand());
            if (not is<PointerType>(operand_type)) {
                Error(u->location(), "Cannot dereference non-pointer type {}", operand_type);
                u->set_sema_errored();
                break;
            }

            u->type(
                as<PointerType>(operand_type)->element_type()
            );
            u->set_lvalue();
        } break;

        // Negate an integer or free a dynamic array.
        case TokenKind::Minus: {
            if (operand_type->is_dynamic_array()) {
                u->type(Type::Void);
                LCC_ASSERT(
                    is<NameRefExpr>(u->operand()),
                    "Sorry, only handle NameRefExpr when freeing dynamic arrays"
                );
                auto* target = as<NameRefExpr>(u->operand())->target();

                // NOTE: If referenced again, will cause a used-but-no-longer-viable
                // diagnostic (catches use-after-free).
                target->set_sema_no_longer_viable();

                // NOTE: For forget-to-free diagnostics.
                std::erase(curr_func->dangling_dynarrays(), target);

                break;
            }

            LValueToRValue(&u->operand());

            if (
                not operand_type->is_integer()
                or not Convert(&u->operand(), Type::Int)
            ) {
                Error(
                    u->location(),
                    "Operand of unary prefix operator {} must be of a dynamic array type, or convertible to an integer, but was {}",
                    ToString(u->op()),
                    operand_type
                );
                u->set_sema_errored();
                break;
            }

            u->type(operand_type);
        } break;

        case TokenKind::MinusMinus:
        case TokenKind::PlusPlus: {
            if (not u->operand()->is_lvalue()) {
                // TODO: If it's not an lvalue, should we just return "operand + 1"
                // instead of "operand := operand + 1"? This would make ++1 = 2.
                Error(
                    u->location(),
                    "Operand of unary prefix operator {} (increment) must be assignable (an lvalue)\n"
                    "If you think this should work and should increment without doing an assignment, let me know.\n",
                    ToString(u->op())
                );
                u->set_sema_errored();
                break;
            }

            // Unary prefix operator -> binary operator IIFE.
            auto binary_op = [](TokenKind k) {
                switch (k) {
                    default: Diag::ICE(
                        "You added a case up above but didn't handle it down here"
                    );
                    case TokenKind::PlusPlus: return TokenKind::Plus;
                    case TokenKind::MinusMinus: return TokenKind::Minus;
                }
            }(u->op());
            // We let the binary expression type checking handle the, well, type
            // checking.
            RewriteToBinaryOpThenAssign(
                expr_ptr,
                binary_op,
                u->operand(),
                new (mod) IntegerLiteral(1, u->location()),
                u->location()
            );
        } break;

        /// Bitwise-not an integer.
        case TokenKind::BitNOT: {
            LValueToRValue(&u->operand());
            if (
                not operand_type->is_integer()
                or not Convert(&u->operand(), Type::Int)
            ) {
                Error(
                    u->location(),
                    "Operand of unary prefix operator {} must be convertible to {}, but was {}",
                    ToString(u->op()),
                    Type::Int,
                    operand_type
                );
                u->set_sema_errored();
                break;
            }

            u->type(operand_type);
        } break;

        /// Negate a bool, integer, or pointer.
        case TokenKind::Exclam: {
            LValueToRValue(&u->operand());
            // Logical negation accepts pointer types, or any non-pointer type that is
            // convertible to an integer.
            if (
                not is<PointerType>(operand_type)
                and not Convert(&u->operand(), Type::Bool)
            ) {
                Error(
                    u->location(),
                    "Operand of unary prefix operator {} must be of a pointer type or convertible to {}, but was {}",
                    ToString(u->op()),
                    Type::Bool,
                    operand_type
                );
                u->set_sema_errored();
                break;
            }

            /// The result of logical negation is always a bool.
            u->type(Type::Bool);
        } break;

        // Check if a sum type currently stores a given member.
        case TokenKind::Has: {
            auto member_access = cast<MemberAccessExpr>(u->operand());
            if (not member_access) {
                Error(
                    u->operand()->location(),
                    "Expected a member access to a sum type as the operand to {0}, i.e. `{0} sum.x`.",
                    ToString(u->op())
                );
                u->set_sema_errored();
                break;
            }
            if (not is<SumType>(member_access->object()->type())) {
                Error(
                    u->operand()->location(),
                    "Operand of unary prefix operator {} must access a sum type, but was {}",
                    ToString(u->op()),
                    member_access->object()->type()
                );
                u->set_sema_errored();
                break;
            }

            // The result of 'has' is a boolean value.
            u->type(Type::Bool);
        } break;

        case TokenKind::Invalid:
        case TokenKind::Eof:
        case TokenKind::Apply:
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
        case TokenKind::StarStar:
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
        case TokenKind::Match:
        case TokenKind::Switch:
        case TokenKind::Print:
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
        case TokenKind::ByteLiteral:
        case TokenKind::Template:
        case TokenKind::Typeof:
        case TokenKind::Tilde:
        case TokenKind::BitAND:
        case TokenKind::BitOR:
        case TokenKind::BitXOR:
        case TokenKind::Continue:
        case TokenKind::Break:
            Diag::ICE("Invalid prefix operator {}", ToString(u->op()));
            LCC_UNREACHABLE();
    }

#undef operand_type
}
