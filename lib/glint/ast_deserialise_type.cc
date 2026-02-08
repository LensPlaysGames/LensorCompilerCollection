#include <glint/ast.hh>
#include <glint/module_description.hh>
#include <glint/sema.hh>

#include <lcc/context.hh>
#include <lcc/utils.hh>

#include <array>
#include <cstring>
#include <limits>
#include <string>
#include <utility>
#include <vector>

void lcc::glint::Module::scope_walk(lcc::Context* ctx, Expr* e, Scope* current_scope) {
    LCC_ASSERT(e);
    LCC_ASSERT(current_scope);

    switch (e->kind()) {
        // No scope changes
        case Expr::Kind::Alignof:
        case Expr::Kind::Binary:
        case Expr::Kind::Call:
        case Expr::Kind::Cast:
        case Expr::Kind::CompoundLiteral:
        case Expr::Kind::EvaluatedConstant:
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::FractionalLiteral:
        case Expr::Kind::StringLiteral:
        case Expr::Kind::IntrinsicCall:
        case Expr::Kind::MemberAccess:
        case Expr::Kind::Module:
        case Expr::Kind::OverloadSet:
        case Expr::Kind::Return:
        case Expr::Kind::Sizeof:
        case Expr::Kind::Type:
        case Expr::Kind::Unary:
        case Expr::Kind::Apply:
        case Expr::Kind::Group:
            break;

        case Expr::Kind::NameRef: {
            as<NameRefExpr>(e)->scope(current_scope);
        } break;

        // These expressions open a new scope
        case Expr::Kind::Block: {
            // A block defines a new scope, and all child expressions are within that
            // scope.
            auto block_scope = new (*this) Scope(current_scope);
            for (auto c : as<BlockExpr>(e)->children())
                scope_walk(ctx, c, block_scope);
        } break;

        case Expr::Kind::Template: {
            // A template defines a new scope, and declares it's parameters in that scope.
            auto template_scope = new (*this) Scope(current_scope);

            // Declare parameters in template's scope.
            auto e_template = as<TemplateExpr>(e);
            for (auto template_parameter : e_template->params()) {
                auto template_parameter_decl = new (*this) VarDecl(
                    template_parameter.name,
                    template_parameter.type,
                    nullptr,
                    this,
                    Linkage::Internal,
                    {}
                );
                auto name = template_parameter_decl->name();
                LCC_ASSERT(
                    current_scope->declare(
                        ctx,
                        std::move(name),
                        template_parameter_decl
                    )
                );
            }

            // Walk body.
            scope_walk(ctx, e_template->body(), template_scope);

        } break;

        case Expr::Kind::EnumeratorDecl:
        case Expr::Kind::For:
        case Expr::Kind::FuncDecl:
        case Expr::Kind::TemplatedFuncDecl:
        case Expr::Kind::If:
        case Expr::Kind::Match:
        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::TypeDecl:
        case Expr::Kind::VarDecl:
        case Expr::Kind::While:
            LCC_TODO("Scope walking during deserialisation");
    }
}

auto lcc::glint::Module::deserialise(
    lcc::Context* context,
    std::vector<u8> module_metadata_blob
) -> bool {
    using lcc::utils::from_bytes;

    LCC_ASSERT(context);

    // We need at least enough bytes for a header, for a zero-exports module
    // (if that is even allowed past sema).
    if (module_metadata_blob.size() < sizeof(ModuleDescription::Header))
        return false;

    // Copy the header from the binary metadata blob.
    ModuleDescription::Header hdr{};
    std::memcpy(
        &hdr,
        module_metadata_blob.data(),
        sizeof(ModuleDescription::Header)
    );

    // Verify header has expected values.
    if (hdr.version != 1) {
        fmt::print(
            "ERROR: Could not deserialise: Invalid version {} in header\n",
            hdr.version
        );
        return false;
    }
    if (
        hdr.magic[0] != ModuleDescription::magic_byte0
        or hdr.magic[1] != ModuleDescription::magic_byte1
        or hdr.magic[2] != ModuleDescription::magic_byte2
    ) {
        fmt::print(
            "ERROR: Could not deserialise: "
            "Invalid magic bytes in header: {} {} {} (expected {} {} {})\n",
            hdr.magic[0],
            hdr.magic[1],
            hdr.magic[2],
            ModuleDescription::magic_byte0,
            ModuleDescription::magic_byte1,
            ModuleDescription::magic_byte2
        );
        return false;
    }

    // Starting at the expression table offset, parse all expressions. Stop
    // after parsing the amount of expressions specified in the header.

    // Write resolved Type* to Type** fixup corresponding to deserialised type
    // at index.
    struct TypeFixup {
        Type** fixup{};
        ModuleDescription::TypeIndex index;
    };
    std::vector<TypeFixup> fixups{};

    std::vector<Expr*> exprs{};
    {
        auto expr_count = hdr.expr_count;
        auto expr_offset = hdr.expr_table_offset;
        exprs.reserve(expr_count);

        const auto read_t = [&]<typename T> [[nodiscard]] (T _) {
            constexpr auto size = sizeof(T);
            std::array<u8, size> array{};
            for (unsigned i = 0; i < size; ++i)
                array[i] = module_metadata_blob.at(expr_offset++);
            return from_bytes<T>(array);
        };

        for (decltype(expr_count) expr_index = 0; expr_index < expr_count; ++expr_index) {
            auto tag = module_metadata_blob.at(expr_offset++);
            // TODO: Ensure kind is within range/a valid kind.
            auto kind = Expr::Kind(tag);
            switch (kind) {
                case Expr::Kind::EvaluatedConstant:
                    Diag::ICE("Evaluated constant should be binary encoded as some sort of literal");

                /// deserialise
                /// NameRefExpr
                ///     type   :TypeIndex
                ///     length :u16
                ///     name   :u8[length]
                case Expr::Kind::NameRef: {
                    auto ty = read_t(ModuleDescription::TypeIndex());

                    auto length = read_t(u16());
                    LCC_ASSERT(length, "NameRefExpr must not have zero length name");

                    std::string deserialised_name{};
                    for (u32 i = 0; i < length; ++i)
                        deserialised_name += char(module_metadata_blob.at(expr_offset++));

                    auto n = new (*this) NameRefExpr(
                        deserialised_name,
                        nullptr,
                        {}
                    );

                    if (ty != ModuleDescription::bad_type_index)
                        fixups.emplace_back(n->type_ref(), ty);

                    exprs.insert(exprs.begin() + expr_index, n);
                } break;

                // deserialise
                // IntegerLiteral
                //     type :TypeIndex
                //     value :u64
                case Expr::Kind::IntegerLiteral: {
                    auto t_index = read_t(ModuleDescription::TypeIndex());
                    auto value = read_t(u64());

                    auto i = new (*this) IntegerLiteral(value, Type::Unknown, {});
                    fixups.emplace_back(i->type_ref(), t_index);

                    exprs.insert(exprs.begin() + expr_index, i);
                } break;

                // FractionalLiteral
                //     value :FixedPointNumber
                case Expr::Kind::FractionalLiteral: {
                    auto value = read_t(FixedPointNumber());
                    auto f = new (*this) FractionalLiteral(value, {});
                    exprs.insert(exprs.begin() + expr_index, f);
                } break;

                // StringLiteral
                //     length   :u32
                //     contents :u8[length]
                case Expr::Kind::StringLiteral: {
                    auto length = read_t(u32());
                    LCC_ASSERT(
                        expr_offset + length < module_metadata_blob.size(),
                        "Invalid StringLiteral length: exceeds bounds of binary metadata blob."
                    );
                    // FIXME: We could probably just construct a string_view instead of
                    // building a string.
                    std::string deserialised_name{};
                    for (decltype(length) i = 0; i < length; ++i)
                        deserialised_name += char(module_metadata_blob.at(expr_offset++));

                    // This currently doesn't work, because the string gets interned into
                    // the /imported/ module, but we need this expression to act as if it is a
                    // part of the module /importing it/. Otherwise, we wouldn't need to (de)
                    // serialise the expression ever, if it wasn't going to be used in the
                    // context of the module importing it.
                    auto s = new (*this) StringLiteral(*this, deserialised_name, {});
                    exprs.insert(exprs.begin() + expr_index, s);
                } break;

                // TypeExpr:
                //     type :TypeIndex
                case Expr::Kind::Type: {
                    auto t_index = read_t(ModuleDescription::TypeIndex());

                    auto t = new (*this) TypeExpr(*this, Type::Unknown, {});
                    fixups.emplace_back(t->contained_type_ref(), t_index);

                    exprs.insert(exprs.begin() + expr_index, t);
                } break;

                // TemplateExpr
                //     body :ExprIndex
                //     param_count :u8
                //     param_data  :(type : TypeIndex, name_length :u16, name :u8[name_length])[param_count]
                case Expr::Kind::Template: {
                    auto body_index = read_t(ModuleDescription::ExprIndex());
                    LCC_ASSERT(body_index != ModuleDescription::bad_expr_index);
                    auto body = exprs.at(body_index);

                    auto param_count = read_t(u8());
                    std::vector<TemplateExpr::Param> parameters{};
                    for (decltype(param_count) i = 0; i < param_count; ++i) {
                        auto param_type_index = read_t(ModuleDescription::TypeIndex());

                        auto param_name_length = read_t(u16());
                        std::string param_name{};
                        for (decltype(param_name_length) j = 0; j < param_name_length; ++j)
                            param_name += (char) module_metadata_blob.at(expr_offset++);

                        parameters.emplace_back(
                            TemplateExpr::Param{
                                param_name,
                                Type::Unknown,
                                {}
                            }
                        );
                        fixups.emplace_back(&parameters.back().type, param_type_index);
                    }

                    auto t = new (*this) TemplateExpr(
                        body,
                        std::move(parameters),
                        {}
                    );
                    exprs.insert(exprs.begin() + expr_index, t);
                } break;

                // ReturnExpr
                //     value :ExprIndex (-1 if not present)
                case Expr::Kind::Return: {
                    auto e_index = read_t(ModuleDescription::ExprIndex());
                    Expr* r_value{};
                    if (e_index != ModuleDescription::bad_expr_index)
                        r_value = exprs.at(e_index);

                    auto r = new (*this) ReturnExpr(r_value, {});
                    exprs.insert(exprs.begin() + expr_index, r);
                } break;

                // ApplyExpr
                //     function :ExprIndex
                //     arglists_count :u8
                //     arglists :ExprIndex[arglists_count]
                case Expr::Kind::Apply: {
                    auto callee_index = read_t(ModuleDescription::ExprIndex());
                    auto arglists_count = read_t(u8());
                    std::vector<Expr*> arglists{};
                    for (u8 i = 0; i < arglists_count; ++i) {
                        auto arglist_index = read_t(ModuleDescription::ExprIndex());
                        arglists.emplace_back(exprs.at(arglist_index));
                    }

                    auto callee = exprs.at(callee_index);

                    auto a = new (*this) ApplyExpr(callee, arglists, {});
                    exprs.insert(exprs.begin() + expr_index, a);
                } break;

                // MemberAccessExpr
                //     type :TypeIndex
                //     object :ExprIndex
                //     name_length :u16
                //     name :u8[name_length]
                case Expr::Kind::MemberAccess: {
                    auto t_index = read_t(ModuleDescription::TypeIndex());
                    auto object_index = read_t(ModuleDescription::ExprIndex());
                    auto name_length = read_t(u16());
                    std::string name{};
                    for (decltype(name_length) i = 0; i < name_length; ++i)
                        name += char(module_metadata_blob.at(expr_offset++));

                    auto object = exprs.at(object_index);

                    auto m = new (*this) MemberAccessExpr(object, name, {});
                    if (t_index != ModuleDescription::bad_type_index)
                        fixups.emplace_back(m->type_ref(), t_index);

                    exprs.insert(exprs.begin() + expr_index, m);
                } break;

                // CastExpr
                //     cast_kind :u8
                //     to :TypeIndex
                //     from :ExprIndex
                case Expr::Kind::Cast: {
                    auto cast_kind = read_t(u8());
                    auto to_type_index = read_t(ModuleDescription::TypeIndex());
                    auto from_expr_index = read_t(ModuleDescription::ExprIndex());

                    // TODO: Verify cast_kind

                    auto from_expr = exprs.at(from_expr_index);
                    auto c = new (*this) CastExpr(
                        from_expr,
                        nullptr,
                        (CastKind) cast_kind,
                        {}
                    );
                    fixups.emplace_back(c->type_ref(), to_type_index);

                    exprs.insert(exprs.begin() + expr_index, c);
                } break;

                case Expr::Kind::Sizeof:
                case Expr::Kind::Alignof:
                case Expr::Kind::Unary:
                    LCC_TODO("Implement deserialisation of expression kind {}", ToString(kind));

                case Expr::Kind::Binary:
                case Expr::Kind::If:
                case Expr::Kind::While:
                case Expr::Kind::For:
                    LCC_TODO("Implement deserialisation of expression kind {}", ToString(kind));

                case Expr::Kind::Call:
                case Expr::Kind::IntrinsicCall:
                case Expr::Kind::CompoundLiteral:
                case Expr::Kind::Block:
                case Expr::Kind::Group:
                case Expr::Kind::Match:
                    LCC_TODO("Implement deserialisation of expression kind {}", ToString(kind));

                case Expr::Kind::Module:
                case Expr::Kind::EnumeratorDecl:
                case Expr::Kind::FuncDecl:
                case Expr::Kind::TemplatedFuncDecl:
                case Expr::Kind::OverloadSet:
                case Expr::Kind::TypeAliasDecl:
                case Expr::Kind::TypeDecl:
                case Expr::Kind::VarDecl:
                    LCC_TODO("Implement deserialisation of expression kind {}", ToString(kind));
            }
        }
    }

    // for (auto e : exprs) {
    //     fmt::print("Deserialised expression:\n");
    //     e->print(true);
    // }

    // Fixup/build scope information in deserialised expressions.
    {
        // The deserialised module gets it's own scope.
        //
        // [GLOBAL]
        // |-- [PROGRAM TOP LEVEL]
        // `-- [DESERIALISED MODULE]
        //
        // This way, the deserialised stuff may still access the global scope, and
        // itself, but not the program stuff. (A module reaching into a program
        // may seem useful for things like making the module consumer define
        // option variables, but, there are better ways to do that imo).
        auto the_global_scope = global_scope();
        Scope* module_scope = new (*this) Scope(the_global_scope);

        for (auto e : exprs)
            scope_walk(context, e, module_scope);
    }
    // Starting at the type table offset, parse all types. Stop after parsing
    // the amount of types specified in the header.
    LCC_ASSERT(
        types.size() <= std::numeric_limits<ModuleDescription::TypeIndex>::max(),
        "Too many types, cannot deserialise with current integer type of type index"
    );
    const auto types_zero_index = (ModuleDescription::TypeIndex) types.size();

    auto type_count = hdr.type_count;
    auto type_offset = hdr.type_table_offset;
    types.reserve(types.size() + type_count);

    // NOTE: Please, PLEASE take care when referencing 'types'. The ZERO INDEX
    // is VERY IMPORTANT and MUST be taken into account, lest the code be
    // full 'o' bugs.
    const auto type_at = [&] [[nodiscard]] (ModuleDescription::TypeIndex i) {
        LCC_ASSERT(i != ModuleDescription::bad_type_index);
        return types.at(types_zero_index + i);
    };

    const auto read_t = [&]<typename T> [[nodiscard]] (T _) {
        constexpr auto size = sizeof(T);
        std::array<u8, size> array{};
        for (unsigned i = 0; i < size; ++i)
            array[i] = module_metadata_blob.at(type_offset++);
        return from_bytes<T>(array);
    };

    for (decltype(type_count) type_index = 0; type_index < type_count; ++type_index) {
        auto tag = module_metadata_blob.at(type_offset++);
        // TODO: Ensure kind is within range/a valid kind.
        auto kind = Type::Kind(tag);
        switch (kind) {
            // NamedType: length :u16, name :u8[length]
            case Type::Kind::Named: {
                auto length = read_t(u16());
                LCC_ASSERT(
                    length,
                    "Deserialised named type has zero-length name"
                );

                std::string deserialised_name{};
                for (u32 i = 0; i < length; ++i)
                    deserialised_name += char(module_metadata_blob.at(type_offset++));

                LCC_ASSERT(
                    not deserialised_name.empty(),
                    "Deserialised named type has empty name"
                );

                // FIXME: This may need to be top level scope, not entirely sure the
                // semantics of this yet.
                new (*this) NamedType(deserialised_name, global_scope(), {});

                // FIXME: Removed for hack regarding serialising templates before 'expr'
                // and 'type' become actual types.
                // LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // BuiltinType: builtin_kind :u8
            case Type::Kind::Builtin: {
                auto builtin_kind_value = module_metadata_blob.at(type_offset++);
                // clang-format off
                LCC_ASSERT(
                    builtin_kind_value == +BuiltinType::BuiltinKind::Bool
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Byte
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Int
                    or builtin_kind_value == +BuiltinType::BuiltinKind::UInt
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Void
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Unknown
                    or builtin_kind_value == +BuiltinType::BuiltinKind::OverloadSet,
                    "Invalid builtin kind value {}", builtin_kind_value
                );
                // clang-format on
                LCC_ASSERT(
                    builtin_kind_value != +BuiltinType::BuiltinKind::OverloadSet,
                    "Cannot deserialise overload sets; sorry"
                );
                auto builtin_kind = BuiltinType::BuiltinKind(builtin_kind_value);
                // Purely for the side-effect of recording the type in the module.
                (void) BuiltinType::Make(*this, builtin_kind, {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // FFIType: ffi_kind :u16
            case Type::Kind::FFIType: {
                auto ffi_kind_value = read_t(u16());
                // Verify/validate ffi kind value
                LCC_ASSERT(
                    ffi_kind_value == +FFIType::FFIKind::CChar
                        or ffi_kind_value == +FFIType::FFIKind::CSChar
                        or ffi_kind_value == +FFIType::FFIKind::CUChar
                        or ffi_kind_value == +FFIType::FFIKind::CShort
                        or ffi_kind_value == +FFIType::FFIKind::CUShort
                        or ffi_kind_value == +FFIType::FFIKind::CInt
                        or ffi_kind_value == +FFIType::FFIKind::CUInt
                        or ffi_kind_value == +FFIType::FFIKind::CLong
                        or ffi_kind_value == +FFIType::FFIKind::CULong
                        or ffi_kind_value == +FFIType::FFIKind::CLongLong
                        or ffi_kind_value == +FFIType::FFIKind::CULongLong,
                    "Invalid Deserialised FFI Kind Value: {}",
                    ffi_kind_value
                );
                auto ffi_kind = FFIType::FFIKind(ffi_kind_value);
                // Purely for the side-effect of recording the type in the module.
                (void) FFIType::Make(*this, ffi_kind, {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            case Type::Kind::ArrayView: {
                auto elem_type_index = read_t(ModuleDescription::TypeIndex());
                (void) new (*this) ArrayViewType(type_at(elem_type_index), {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            case Type::Kind::DynamicArray: {
                auto elem_type_index = read_t(ModuleDescription::TypeIndex());
                // TODO: Size expression
                (void) new (*this) DynamicArrayType(type_at(elem_type_index), nullptr, {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            case Type::Kind::Pointer: {
                auto elem_type_index = read_t(ModuleDescription::TypeIndex());
                (void) new (*this) PointerType(type_at(elem_type_index), {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            case Type::Kind::Reference: {
                auto elem_type_index = read_t(ModuleDescription::TypeIndex());
                (void) new (*this) ReferenceType(type_at(elem_type_index), {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // IntegerType: bitwidth :u16, is_signed :u8
            case Type::Kind::Integer: {
                auto bitwidth = read_t(u16());
                LCC_ASSERT(bitwidth != 0, "IntegerType: Bitwidth of zero is invalid");

                auto is_signed = module_metadata_blob.at(type_offset++);
                LCC_ASSERT(
                    is_signed == 0 or is_signed == 1,
                    "Expected is_signed to be a boolean value, but got {}\n",
                    (unsigned) is_signed
                );

                (void) new (*this) IntegerType(bitwidth, is_signed, {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // ArrayType: element_type_index :TypeIndex, element_count :u64
            case Type::Kind::Array: {
                auto element_type_index = read_t(ModuleDescription::TypeIndex());
                auto element_count = read_t(u64());

                // FIXME: Should we leave stuff like this for sema to catch?
                LCC_ASSERT(element_count != 0, "ArrayType with zero elements is invalid");

                auto e_size = new (*this) IntegerLiteral(element_count, {});
                auto t_elem = type_at(element_type_index);

                (void) new (*this) ArrayType(t_elem, e_size, {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // FunctionType:
            //     attributes :u32
            //     param_count :u16
            //     param_types :TypeIndex[param_count]
            //     return_type :TypeIndex
            //     param_names :(param_name_length :u16, param_name :u8[param_name_length])[param_count]
            case Type::Kind::Function: {
                // Attributes
                auto attributes = read_t(u32());

                // Parameter Count
                auto param_count = read_t(u16());

                // Parameter Type Indices
                std::vector<ModuleDescription::TypeIndex> param_type_indices{};
                for (usz param_index = 0; param_index < param_count; ++param_index) {
                    auto param_type_index = read_t(ModuleDescription::TypeIndex());
                    param_type_indices.emplace_back(param_type_index);
                }

                // Return Type Index
                auto return_type_index = read_t(ModuleDescription::TypeIndex());

                // Parameter Names
                std::vector<std::string> param_names{};
                for (usz param_index = 0; param_index < param_count; ++param_index) {
                    auto param_name_length = read_t(u16());

                    std::string param_name{};
                    param_name.reserve(param_name_length);
                    for (unsigned i = 0; i < param_name_length; ++i)
                        param_name += char(module_metadata_blob.at(type_offset++));

                    param_names.emplace_back(std::move(param_name));
                }

                std::vector<FuncType::Param> params{};
                {
                    usz param_i{0};
                    for (auto param_type_index : param_type_indices) {
                        params.emplace_back(
                            param_names.at(param_i),
                            type_at(param_type_index),
                            Location{}
                        );
                        ++param_i;
                    }
                }

                auto function = new (*this) FuncType(
                    params,
                    type_at(return_type_index),
                    {},
                    {}
                );

                const auto set_attr = [&](FuncAttr f) {
                    if (attributes & (u32(1) << u32(f)))
                        function->set_attr(f);
                };

                static_assert(
                    +FuncAttr::COUNT == 11,
                    "Exhaustive handling of function attributes in module deserialisation"
                );
                set_attr(FuncAttr::Const);
                set_attr(FuncAttr::Discardable);
                set_attr(FuncAttr::Flatten);
                set_attr(FuncAttr::Inline);
                set_attr(FuncAttr::NoInline);
                set_attr(FuncAttr::NoMangle);
                set_attr(FuncAttr::NoOpt);
                set_attr(FuncAttr::NoReturn);
                set_attr(FuncAttr::Pure);
                set_attr(FuncAttr::ReturnsTwice);
                set_attr(FuncAttr::Used);

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // deserialise()
            // SumType:
            //     member_count :u16
            //     member_types :TypeIndex[member_count]
            //     member_data  :(name_length :u16, member_name :u8[name_length])[member_count]
            case Type::Kind::Sum: {
                auto member_count = read_t(u16());

                // Member Type Indices
                std::vector<ModuleDescription::TypeIndex> member_type_indices{};
                for (usz member_index = 0; member_index < member_count; ++member_index) {
                    auto member_type_index = read_t(ModuleDescription::TypeIndex());
                    member_type_indices.emplace_back(member_type_index);
                }

                // Member Names
                std::vector<std::string> member_names{};
                for (usz member_index = 0; member_index < member_count; ++member_index) {
                    auto member_name_length = read_t(u16());

                    std::string member_name{};
                    member_name.reserve(member_name_length);
                    for (unsigned i = 0; i < member_name_length; ++i)
                        member_name += char(module_metadata_blob.at(type_offset++));

                    member_names.emplace_back(std::move(member_name));
                }

                // Member Types
                std::vector<SumType::Member> members{};
                members.reserve(member_type_indices.size());
                usz member_i{0};
                for (auto member_type_index : member_type_indices) {
                    // TODO: Get default expression, somehow (requires (de)serialisation of
                    // Glint Expr*).
                    members.emplace_back(
                        member_names.at(member_i),
                        type_at(member_type_index),
                        nullptr,
                        Location{}
                    );
                    ++member_i;
                }

                // FIXME: What should parent scope of sum type scope be?
                auto scope = new (*this) Scope(nullptr);

                // Declare members in scope
                for (auto& m : members) {
                    auto member_decl = new (*this) VarDecl(
                        std::string(m.name),
                        m.type,
                        nullptr,
                        this,
                        Linkage::Internal,
                        {}
                    );
                    auto decl = scope->declare(
                        context,
                        std::string(m.name),
                        member_decl
                    );
                    LCC_ASSERT(decl);
                }

                (void) new (*this) SumType(scope, std::move(members), {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // EnumType:
            //     underlying_type_index :TypeIndex
            //     enum_decl_count :u32
            //     enum_decls :u64[enum_decl_count]
            //     enum_names :(name_length :u16, name: u8[name_length])[enum_decl_count]
            // FIXME: Ideally we should get the size of the value (enum_decls element
            // type) from the underlying type, instead of just assuming it fits in 64
            // bits... But... yeah.
            case Type::Kind::Enum: {
                // Get underlying type index
                auto underlying_type_index = read_t(ModuleDescription::TypeIndex());

                // Get enum decl count
                auto enum_decl_count = read_t(u32());

                // Get enum values
                std::vector<u64> enum_values{};
                enum_values.reserve(enum_decl_count);
                for (unsigned value_index = 0; value_index < enum_decl_count; ++value_index) {
                    // TODO: Don't just assume it fits in 64 bits.
                    auto value = read_t(u64());
                    enum_values.emplace_back(value);
                }

                // Get enum names
                std::vector<std::string> enum_names{};
                enum_names.reserve(enum_decl_count);
                for (unsigned name_index = 0; name_index < enum_decl_count; ++name_index) {
                    auto name_length = read_t(u16());

                    std::string name{};
                    name.reserve(name_length);
                    for (auto i = 0; i < name_length; ++i)
                        name += (char) module_metadata_blob.at(type_offset++);

                    enum_names.emplace_back(name);
                }

                // FIXME: What should parent of imported enum scope be?
                auto scope = new (*this) Scope(nullptr);
                std::vector<EnumeratorDecl*> enumerator_decls{};

                for (unsigned i = 0; i < enum_names.size(); ++i) {
                    auto enum_decl = new (*this) EnumeratorDecl(
                        std::string(enum_names.at(i)),
                        new (*this) ConstantExpr(
                            new (*this) IntegerLiteral(enum_values.at(i), {}),
                            enum_values.at(i)
                        ),
                        {}
                    );

                    enumerator_decls.emplace_back(enum_decl);
                }

                (void) new (*this) EnumType(
                    scope,
                    type_at(underlying_type_index),
                    std::move(enumerator_decls),
                    {}
                );
                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));
            } break;

            // deserialise()
            // StructType:
            //     size_in_bytes :u16
            //     align_in_bytes :u16
            //     member_count :u16
            //     member_types :TypeIndex[member_count]
            //     member_data :(byte_offset :u16, name_length :u16, member_name :u8[name_length])[member_count]
            case Type::Kind::Struct: {
                // Get size in bytes
                auto struct_byte_size = read_t(u16());

                // Get align in bytes
                auto struct_byte_align = read_t(u16());

                // Get member count
                auto struct_member_count = read_t(u16());

                // Get member type indices
                std::vector<ModuleDescription::TypeIndex> member_type_indices{};
                for (unsigned i = 0; i < struct_member_count; ++i) {
                    // Get member type index
                    auto member_type_index = read_t(ModuleDescription::TypeIndex());
                    member_type_indices.emplace_back(member_type_index);
                }

                // Get member data, including byte offsets and names.
                std::vector<u16> member_byte_offsets{};
                std::vector<std::string> member_names{};
                for (unsigned i = 0; i < struct_member_count; ++i) {
                    // Get member byte offset
                    auto member_byte_offset = read_t(u16());
                    member_byte_offsets.emplace_back(member_byte_offset);

                    // Get member name length
                    auto member_name_length = read_t(u16());

                    // Get member name
                    std::string member_name{};
                    for (unsigned j = 0; j < member_name_length; ++j)
                        member_name += (char) module_metadata_blob.at(type_offset++);

                    member_names.emplace_back(std::move(member_name));
                }

                std::vector<StructType::Member> members{};
                members.reserve(member_type_indices.size());
                usz member_i{0};
                for (auto member_type_index : member_type_indices) {
                    // TODO: serialise/deserialise supplanted boolean for each member.
                    members.emplace_back(
                        member_names.at(member_i),
                        type_at(member_type_index),
                        Location{},
                        member_byte_offsets.at(member_i),
                        false
                    );
                    ++member_i;
                }

                // FIXME: What should parent scope of struct type scope be?
                auto scope = new (*this) Scope(nullptr);

                // Declare members in scope
                for (auto& m : members) {
                    // TODO: Maybe parse default init expression from module metadata, once we
                    // can serialise expressions...
                    auto member_decl = new (*this) VarDecl(
                        std::string(m.name),
                        m.type,
                        nullptr,
                        this,
                        Linkage::Internal,
                        {}
                    );
                    auto decl = scope->declare(
                        context,
                        std::string(m.name),
                        member_decl
                    );
                    LCC_ASSERT(decl);
                }

                auto s = new (*this) StructType(scope, members, {});
                LCC_ASSERT(lcc::glint::Sema::AnalyseType(context, *this, &types.back()));

                s->byte_size(struct_byte_size);
                s->alignment(struct_byte_align);
            } break;

            // UnionType:
            //     member_count :u16
            //     member_types :TypeIndex[member_count]
            //     member_data  :(name_length :u16, member_name :u8[name_length])[member_count]
            case Type::Kind::Union: {
                auto member_count = read_t(u16());

                // Member Type Indices
                std::vector<ModuleDescription::TypeIndex> member_type_indices{};
                for (usz member_index = 0; member_index < member_count; ++member_index) {
                    auto member_type_index = read_t(ModuleDescription::TypeIndex());
                    member_type_indices.emplace_back(member_type_index);
                }

                // Member Names
                std::vector<std::string> member_names{};
                for (usz member_index = 0; member_index < member_count; ++member_index) {
                    auto member_name_length = read_t(u16());

                    std::string member_name{};
                    member_name.reserve(member_name_length);
                    for (unsigned i = 0; i < member_name_length; ++i)
                        member_name += char(module_metadata_blob.at(type_offset++));

                    member_names.emplace_back(std::move(member_name));
                }

                std::vector<UnionType::Member> members{};
                members.reserve(member_type_indices.size());
                usz member_i{0};
                for (auto member_type_index : member_type_indices) {
                    members.emplace_back(
                        member_names.at(member_i),
                        type_at(member_type_index),
                        Location{}
                    );
                    ++member_i;
                }

                // FIXME: What should parent scope of struct type scope be?
                auto scope = new (*this) Scope(nullptr);

                // Declare members in scope
                for (auto& m : members) {
                    // TODO: Maybe parse default init expression from module metadata, once we
                    // can serialise expressions...
                    auto member_decl = new (*this) VarDecl(
                        std::string(m.name),
                        m.type,
                        nullptr,
                        this,
                        Linkage::Internal,
                        {}
                    );
                    auto decl = scope->declare(
                        context,
                        std::string(m.name),
                        member_decl
                    );
                    LCC_ASSERT(decl);
                }

                (void) new (*this) UnionType(scope, members, {});

                LCC_ASSERT(Sema::AnalyseType(context, *this, &types.back()));

            } break;

            case Type::Kind::TemplatedStruct:
                Diag::ICE("Sema should have replaced TemplatedStructType");

            case Type::Kind::Typeof:
                Diag::ICE("Sema should have replaced TypeofType with the type of it's contained expression");

            case Type::Kind::Type:
                Diag::ICE("Sema should have replaced TypeType with it's contained type");
        }
    }

    // for (auto [i, t] : vws::enumerate(types)) {
    //     if (i < types_zero_index) continue;
    //     fmt::print("Deserialised type: {}\n", t->string(true));
    // }

    // Now that types are deserialised, go back through deserialised
    // expressions and resolve types.
    for (auto f : fixups)
        *f.fixup = type_at(f.index);

    // for (auto e : exprs) {
    //     // TODO: Analyse?
    //     fmt::print("Deserialised expression (after type fixups):\n");
    //     e->print(true);
    // }

    // Starting after the header, begin parsing declarations. Stop after
    // parsing the amount of declarations specified in the header.
    auto offset = sizeof(ModuleDescription::Header);
    auto decl_count = hdr.declaration_count;
    while (decl_count--) {
        ModuleDescription::DeclarationHeader decl_hdr{};
        std::memcpy(
            &decl_hdr,
            module_metadata_blob.data() + offset,
            sizeof(decl_hdr)
        );
        offset += sizeof(decl_hdr);

        u8 name_length = module_metadata_blob.at(offset++);

        std::string name{};
        for (decltype(offset) i = 0; i < name_length; ++i)
            name += char(*(module_metadata_blob.data() + offset + i));
        offset += name_length;

        auto* ty = type_at(decl_hdr.type_index);

        // FIXME: Should it be top level scope instead of global?
        auto* scope = global_scope();
        switch (ModuleDescription::DeclarationHeader::Kind(decl_hdr.kind)) {
            case ModuleDescription::DeclarationHeader::Kind::INVALID:
            default:
                LCC_ASSERT(false, "Invalid declaration kind in declaration header: {}", decl_hdr.kind);

            case ModuleDescription::DeclarationHeader::Kind::TYPE:
            case ModuleDescription::DeclarationHeader::Kind::TYPE_ALIAS:
            case ModuleDescription::DeclarationHeader::Kind::ENUMERATOR:
            case ModuleDescription::DeclarationHeader::Kind::VARIABLE:
            case ModuleDescription::DeclarationHeader::Kind::FUNCTION:
            case ModuleDescription::DeclarationHeader::Kind::TEMPLATE:
                break;
        }
        // NOTE: Also look above for exhaustive handling of decl header kinds.
        switch (ModuleDescription::DeclarationHeader::Kind(decl_hdr.kind)) {
            case ModuleDescription::DeclarationHeader::Kind::INVALID:
                LCC_ASSERT(false, "Invalid declaration kind in declaration header: {}", decl_hdr.kind);

            // Created from Expr::Kind::TypeDecl
            case ModuleDescription::DeclarationHeader::Kind::TYPE: {
                LCC_ASSERT(
                    is<DeclaredType>(ty),
                    "Can't make TypeDecl from a Type that is not derived from DeclaredType"
                );
                auto type_decl = new (*this) TypeDecl(this, name, as<DeclaredType>(ty), {});
                type_decl->set_sema_done();
                auto decl = scope->declare(context, std::string(name), type_decl);
            } break;

            // Created from Expr::Kind::TypeAliasDecl
            case ModuleDescription::DeclarationHeader::Kind::TYPE_ALIAS: {
                auto* type_alias_decl = new (*this) TypeAliasDecl(name, ty, {});
                type_alias_decl->set_sema_done();
                auto decl = scope->declare(context, std::string(name), type_alias_decl);
            } break;

                // Created from Expr::Kind::VarDecl
            case ModuleDescription::DeclarationHeader::Kind::TEMPLATE:
            case ModuleDescription::DeclarationHeader::Kind::VARIABLE: {
                // FIXME: Should possibly be reexported.
                auto* var_decl = new (*this) VarDecl(
                    name,
                    ty,
                    nullptr,
                    this,
                    Linkage::Imported,
                    {}
                );
                var_decl->set_lvalue();
                var_decl->set_sema_done();
                if (decl_hdr.expr_index != ModuleDescription::bad_expr_index)
                    var_decl->init() = exprs.at(decl_hdr.expr_index);
                auto decl = scope->declare(context, std::string(name), var_decl);
            } break;

            // Created from Expr::Kind::FuncDecl
            case ModuleDescription::DeclarationHeader::Kind::FUNCTION: {
                LCC_ASSERT(
                    is<FuncType>(ty),
                    "Cannot create FuncDecl when deserialised type, {}, is not a function",
                    *ty
                );
                auto* func_decl = new (*this) FuncDecl(
                    name,
                    as<FuncType>(ty),
                    nullptr,
                    scope,
                    this,
                    Linkage::Imported,
                    {}
                );
                // FIXME: Not sure if imported function declaration is an lvalue? I mean,
                // we can't assign to it...
                func_decl->set_sema_done();
                auto decl = scope->declare(context, std::string(name), func_decl);
            } break;

            // Created from Expr::Kind::EnumeratorDecl
            // FIXME: Is it possible to export a single enum decl? I'm pretty sure the
            // only place an enum decl can happen is within an enum type.
            case ModuleDescription::DeclarationHeader::Kind::ENUMERATOR: {
                LCC_TODO("Make an EnumeratorDecl in the global scope");
            }
        }
    }

    std::string module_name{};
    const auto name_begin = module_metadata_blob.begin() + (decltype(module_metadata_blob)::difference_type) hdr.name_offset;
    for (auto i = name_begin; *i; ++i)
        module_name += (char) *i;

    auto init_function_decl = global_scope()->declare(
        context,
        Module::InitFunctionName(module_name),
        new (*this) FuncDecl(
            Module::InitFunctionName(module_name),
            new (*this) FuncType(
                {},
                Type::Void,
                {{FuncAttr::NoMangle, true}},
                {}
            ),
            nullptr,
            global_scope(),
            this,
            Linkage::Imported,
            {}
        )
    );
    LCC_ASSERT(init_function_decl);

    return true;
}
