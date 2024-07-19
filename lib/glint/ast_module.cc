#include <glint/ast.hh>
#include <glint/module_description.hh>
#include <lcc/file.hh>
#include <lcc/utils.hh>

#include <string>
#include <string_view>
#include <vector>

lcc::glint::Module::Module(
    File* file,
    std::string module_name,
    bool is_logical_module
) : _name{std::move(module_name)},
    _is_module{is_logical_module},
    file{file} {
    FuncType* ty{};

    /// Create the type of the top-level function.
    if (is_logical_module) {
        ty = new (*this) FuncType({}, BuiltinType::Void(*this), {}, {});
    } else {
        auto cchar_ty = FFIType::CChar(*this);
        auto cint_ty = FFIType::CInt(*this);
        auto char_ptr = new (*this) PointerType{new (*this) PointerType{cchar_ty}};
        ty = new (*this) FuncType{
            {
                {"__argc__", cint_ty, {}},
                {"__argv__", char_ptr, {}},
                {"__envp__", char_ptr, {}},
            },

            /// We currently set main() to return `int` since that’s natural for
            /// most programs. In the future, our runtime should define main() and
            /// this function should be something that’s called by it.
            Type::Int,
            {},
            {},
        };
    }

    Location loc{};
    if (file) {
        loc = Location{
            0,
            (decltype(Location::len)) file->size(),
            (decltype(Location::file_id)) file->file_id() //
        };
    }
    top_level_function
        = new (*this) FuncDecl{
            is_logical_module ? fmt::format("_XGlint__init_{}", name()) : "main",
            ty,
            new (*this) BlockExpr{{}, {}},
            nullptr,
            this,
            Linkage::Exported,
            loc,
            CallConv::C,
        };
}

lcc::glint::Module::~Module() {
    for (auto* node : nodes) delete node;
    for (auto* type : types) delete type;
    for (auto* scope : scopes) delete scope;
    for (auto& import : _imports) delete import.module;
}

void lcc::glint::Module::add_top_level_expr(Expr* node) {
    as<BlockExpr>(top_level_function->body())->add(node);
}

auto lcc::glint::Module::intern(std::string_view str) -> usz {
    auto it = rgs::find(strings, str);
    if (it != strings.end()) { return usz(it - strings.begin()); }
    strings.emplace_back(str);
    return strings.size() - 1;
}

/// ================
/// Serialisation
/// ================

// FIXME: This should probably be backwards for big endian machines, afaik.
template <typename T>
std::array<lcc::u8, sizeof(T) / sizeof(lcc::u8)> to_bytes(const T object) {
    std::array<lcc::u8, sizeof(T)> out{};
    const lcc::u8* begin = reinterpret_cast<const lcc::u8*>(&object);
    const lcc::u8* end = begin + (sizeof(T));
    std::copy(begin, end, out.begin());
    return out;
}

// FIXME: This should probably be backwards for big endian machines, afaik.
// requires std::is_trivially_constructible?
template <typename T>
T from_bytes(std::array<lcc::u8, sizeof(T)> bytes) {
    T out{};
    std::copy(bytes.begin(), bytes.end(), &out);
    return out;
}

lcc::u16 lcc::glint::Module::serialise(std::vector<u8>& out, std::vector<Type*>& cache, Type* ty) {
    auto found = rgs::find(cache, ty);
    if (found != cache.end())
        return u16(found - cache.begin());

    LCC_ASSERT(
        cache.size() < 0xffff,
        "Too many types, cannot serialise in binary metadata format version 1"
    );
    auto type_index = ModuleDescription::TypeIndex(cache.size());
    cache.push_back(ty);

    u8 tag = u8(ty->kind());
    out.push_back(tag);

    switch (ty->kind()) {
        // DynamicArrayType: element_type_index :TypeIndex
        case Type::Kind::DynamicArray: {
            auto* type = as<DynamicArrayType>(ty);

            // Write `element_type_index: TypeIndex`, keeping track of byte index into
            // binary metadata blob where it can be found again.
            auto element_type_index_offset = out.size();
            auto element_type_index = ModuleDescription::TypeIndex(-1);
            auto type_index_bytes = to_bytes(element_type_index);
            out.insert(out.end(), type_index_bytes.begin(), type_index_bytes.end());

            auto element_type = serialise(out, cache, type->element_type());

            auto* element_type_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + element_type_index_offset);
            *element_type_ptr = element_type;
        } break;

        // NamedType: length :u32, name :u8[length]
        case Type::Kind::Named: {
            auto* type = as<NamedType>(ty);
            u16 name_length = u16(type->name().length());
            auto name_length_bytes = to_bytes(name_length);
            // Write `length: u16`
            out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
            // Write `name : u8[length]`
            out.insert(out.end(), type->name().begin(), type->name().end());
        } break;

        // PointerType, ReferenceType: type_index :TypeIndex
        case Type::Kind::Pointer:
        case Type::Kind::Reference: {
            auto referenced_type_index = ModuleDescription::TypeIndex(-1);
            auto type_index_bytes = to_bytes(referenced_type_index);

            // Write `type_index: u16`, keeping track of byte index into binary
            // metadata blob where it can be found again.
            u32 referenced_type_index_offset = u32(out.size());
            out.insert(out.end(), type_index_bytes.begin(), type_index_bytes.end());

            // Serialise the referenced type.
            referenced_type_index = serialise(out, cache, ty->elem());

            // Go back and fixup the referenced type index from -1 to the actual
            // proper value.
            // FIXME: Is it possible to /not/ do this reinterpret cast? I guess we
            // could manually fuddle with bytes given the index since we know
            // endianness and size. Yeah, that's probably what we should do.
            auto* referenced_type_index_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + referenced_type_index_offset);
            *referenced_type_index_ptr = referenced_type_index;
        } break;

        // IntegerType: bitwidth :u16, is_signed :u8
        case Type::Kind::Integer: {
            IntegerType* type = as<IntegerType>(ty);
            LCC_ASSERT(
                type->bit_width() <= 0xffff,
                "Cannot encode over-large bitwidth of integer type {}",
                *ty
            );

            u16 bitwidth = u16(type->bit_width());
            auto bitwidth_bytes = to_bytes(bitwidth);

            u8 is_signed = type->is_signed() ? 1 : 0;

            out.insert(out.end(), bitwidth_bytes.begin(), bitwidth_bytes.end());
            out.push_back(is_signed);
        } break;

        // BuiltinType: builtin_kind :u8
        case Type::Kind::Builtin: {
            BuiltinType* type = as<BuiltinType>(ty);
            LCC_ASSERT(
                type->builtin_kind() != BuiltinType::BuiltinKind::OverloadSet,
                "Cannot serialise overload sets; sorry"
            );
            u8 builtin_kind = u8(type->builtin_kind());
            out.push_back(builtin_kind);
        } break;

        // FFIType: ffi_kind :u16
        case Type::Kind::FFIType: {
            FFIType* type = as<FFIType>(ty);
            u16 ffi_kind = u16(type->ffi_kind());
            auto ffi_kind_bytes = to_bytes(ffi_kind);
            out.insert(out.end(), ffi_kind_bytes.begin(), ffi_kind_bytes.end());
        } break;

        // FunctionType:
        //     attributes :u32
        //     param_count :u16
        //     param_types :u16[param_count]
        //     return_type :u16
        //     param_names :(param_name_length :u16, param_name :u8[param_name_length])[param_count]
        case Type::Kind::Function: {
            FuncType* type = as<FuncType>(ty);
            u32 attributes{};
            const auto set_attr_bit = [&](FuncAttr f) {
                if (type->has_attr(f)) {
                    static constexpr auto one = decltype(attributes)(1);
                    attributes |= one << u32(f);
                }
            };
            // Probably don't need all of these. If we run out of bits, we could
            // remove some that aren't needed.
            set_attr_bit(FuncAttr::Const);
            set_attr_bit(FuncAttr::Discardable);
            set_attr_bit(FuncAttr::Flatten);
            set_attr_bit(FuncAttr::Inline);
            set_attr_bit(FuncAttr::NoInline);
            set_attr_bit(FuncAttr::NoMangle);
            set_attr_bit(FuncAttr::NoOpt);
            set_attr_bit(FuncAttr::NoReturn);
            set_attr_bit(FuncAttr::Pure);
            set_attr_bit(FuncAttr::ReturnsTwice);
            set_attr_bit(FuncAttr::Used);

            u16 param_count = u16(type->params().size());

            auto attributes_bytes = to_bytes(attributes);
            out.insert(out.end(), attributes_bytes.begin(), attributes_bytes.end());

            auto param_count_bytes = to_bytes(param_count);
            out.insert(out.end(), param_count_bytes.begin(), param_count_bytes.end());

            // Allocate enough room to represent parameter types, once we are able to
            // serialise them, keeping track of where they are so we can fix them up
            // later.
            auto param_types_offset = out.size();
            out.insert(out.end(), param_count * sizeof(ModuleDescription::TypeIndex), 0);

            auto return_type_offset = out.size();
            out.insert(out.end(), sizeof(ModuleDescription::TypeIndex), 0);

            // Write parameter names
            for (auto& param : type->params()) {
                auto name_length = u16(param.name.length());
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
                out.insert(out.end(), param.name.begin(), param.name.end());
            }

            // Serialise parameter types and fixup parameter type indices previously
            // allocated.
            std::vector<ModuleDescription::TypeIndex> param_types{};
            for (auto param : type->params())
                param_types.push_back(serialise(out, cache, param.type));

            auto* param_types_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + param_types_offset);
            for (auto param_type : param_types)
                *param_types_ptr++ = param_type;

            // Serialise return type and fixup return type index previously allocated.
            auto return_type = serialise(out, cache, type->return_type());
            auto* return_type_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + return_type_offset);
            *return_type_ptr = return_type;
        } break;

        // EnumType:
        //     underlying_type_index :u16
        //     enum_decl_count :u32
        //     enum_decls :u64[enum_decl_count]
        // FIXME: Ideally we should get the size of the value (enum_decls element
        // type) from the underlying type, instead of just assuming it fits in 64
        // bits.
        case Type::Kind::Enum: {
            auto type = as<EnumType>(ty);

            auto underlying_type_offset = out.size();
            out.insert(out.end(), sizeof(ModuleDescription::TypeIndex), 0);

            // TODO: Assert that underlying type can fit in 64 bits. But we don't have
            // context here (yet).

            u32 enum_decl_count = u32(type->enumerators().size());
            auto enum_decl_count_bytes = to_bytes(enum_decl_count);
            out.insert(out.end(), enum_decl_count_bytes.begin(), enum_decl_count_bytes.end());

            for (auto* enum_decl : type->enumerators()) {
                u64 value = enum_decl->value().value();
                auto value_bytes = to_bytes(value);
                out.insert(out.end(), value_bytes.begin(), value_bytes.end());
            }

            auto underlying_type = serialise(out, cache, type->underlying_type());

            auto* underlying_type_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + underlying_type_offset);
            *underlying_type_ptr = underlying_type;
        } break;

        // ArrayType: element_type_index :u16, element_count :u64
        case Type::Kind::Array: {
            auto type = as<ArrayType>(ty);

            auto element_type_offset = out.size();
            out.insert(out.end(), sizeof(ModuleDescription::TypeIndex), 0);

            u64 element_count = u64(type->dimension());
            auto element_count_bytes = to_bytes(element_count);
            out.insert(out.end(), element_count_bytes.begin(), element_count_bytes.end());

            auto element_type = serialise(out, cache, type->element_type());

            auto* element_type_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + element_type_offset);
            *element_type_ptr = element_type;
        } break;

        // StructType:
        //     size_in_bytes :u16
        //     align_in_bytes :u16
        //     member_count :u16
        //     member_types :TypeIndex[member_count]
        //     member_data :(byte_offset :u16, name_length :u16, member_name :u8[name_length])[member_count]
        case Type::Kind::Struct: {
            auto type = as<StructType>(ty);

            auto size_in_bytes = u16(type->byte_size());
            auto size_in_bytes_bytes = to_bytes(size_in_bytes);
            out.insert(out.end(), size_in_bytes_bytes.begin(), size_in_bytes_bytes.end());

            auto align_in_bytes = u16(type->alignment());
            auto align_in_bytes_bytes = to_bytes(align_in_bytes);
            out.insert(out.end(), align_in_bytes_bytes.begin(), align_in_bytes_bytes.end());

            auto& members = type->members();

            auto member_count = u16(members.size());
            auto member_count_bytes = to_bytes(member_count);
            out.insert(out.end(), member_count_bytes.begin(), member_count_bytes.end());

            // Allocate enough room to represent member types, once we are able to
            // serialise them, keeping track of where they are so we can fix them up
            // later.
            auto member_types_offset = out.size();
            out.insert(out.end(), member_count * sizeof(ModuleDescription::TypeIndex), 0);

            // Member Data
            for (const auto& member : members) {
                auto byte_offset = u16(member.byte_offset);
                auto byte_offset_bytes = to_bytes(byte_offset);
                out.insert(out.end(), byte_offset_bytes.begin(), byte_offset_bytes.end());

                auto name_length = member.name.length();
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());

                out.insert(out.end(), member.name.begin(), member.name.end());
            }

            // Serialise member types and fixup member type indices previously
            // allocated.
            std::vector<ModuleDescription::TypeIndex> member_types{};
            for (auto& member : members)
                member_types.push_back(serialise(out, cache, member.type));

            auto* member_types_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + member_types_offset);
            for (auto& member_type : member_types)
                *member_types_ptr++ = member_type;
        } break;

        // TODO:
        case Type::Kind::Union: {
            LCC_TODO("Serialise union type (it's easy, just haven't yet)");
        } break;
    }

    return type_index;
}

std::vector<lcc::u8> lcc::glint::Module::serialise() {
    ModuleDescription::Header hdr{};
    std::vector<u8> declarations{};
    std::vector<u8> types{};
    std::vector<u8> serialised_name{};

    // Decls and types collected from exports.
    std::vector<Type*> type_cache{};
    for (auto* decl : exports) {
        // Decl: DeclHeader, length :u8, name :u8[length]

        // Prepare declaration header
        ModuleDescription::TypeIndex type_index = serialise(types, type_cache, decl->type());
        ModuleDescription::DeclarationHeader decl_hdr{
            u16(ModuleDescription::DeclarationHeader::get_kind(decl)),
            type_index};
        auto decl_hdr_bytes = to_bytes(decl_hdr);

        // Prepare length
        LCC_ASSERT(
            decl->name().length() <= 0xff,
            "Exported declaration has over-long name and cannot be encoded in binary format"
        );
        u8 length = u8(decl->name().length());

        declarations.insert(declarations.end(), decl_hdr_bytes.begin(), decl_hdr_bytes.end());
        declarations.push_back(length);
        declarations.insert(declarations.end(), decl->name().begin(), decl->name().end());
    }

    // TODO: Make name serialisable
    LCC_ASSERT(
        name().size(),
        "Cannot serialise unnamed module"
    );
    serialised_name.insert(
        serialised_name.end(),
        name().begin(),
        name().end()
    );
    LCC_ASSERT(
        std::find(serialised_name.begin(), serialised_name.end(), '\0') == serialised_name.end(),
        "Cannot serialise module with NULL character in name"
    );
    serialised_name.push_back('\0');

    // Final header fixups now that nothing will change.
    hdr.size = u32(sizeof(ModuleDescription::Header) + declarations.size() + types.size() + serialised_name.size());
    hdr.type_table_offset = u32(sizeof(ModuleDescription::Header) + declarations.size());
    hdr.name_offset = u32(sizeof(ModuleDescription::Header) + declarations.size() + types.size());
    hdr.declaration_count = u16(exports.size());
    hdr.type_count = u16(type_cache.size());

    // Convert header to byte representation that is easy to serialise.
    auto hdr_bytes = to_bytes(hdr);

    std::vector<u8> out{};
    out.insert(out.end(), hdr_bytes.begin(), hdr_bytes.end());
    out.insert(out.end(), declarations.begin(), declarations.end());
    out.insert(out.end(), types.begin(), types.end());
    out.insert(out.end(), serialised_name.begin(), serialised_name.end());
    return out;
}

bool lcc::glint::Module::deserialise(lcc::Context* ctx, std::vector<u8> module_metadata_blob) {
    // We need at least enough bytes for a header, for a zero-exports module
    // (if that is even allowed past sema).
    if (module_metadata_blob.size() < sizeof(ModuleDescription::Header))
        return false;

    // Copy the header from the binary metadata blob.
    ModuleDescription::Header hdr{};
    std::memcpy(&hdr, module_metadata_blob.data(), sizeof(ModuleDescription::Header));

    // Verify header has expected values.
    if (hdr.version != 1) {
        fmt::print("ERROR: Could not deserialise: Invalid version {} in header\n", hdr.version);
        return false;
    }
    if (hdr.magic[0] != ModuleDescription::magic_byte0 or hdr.magic[1] != ModuleDescription::magic_byte1 or hdr.magic[2] != ModuleDescription::magic_byte2) {
        fmt::print("ERROR: Could not deserialise: Invalid magic bytes in header: {} {} {}\n", hdr.magic[0], hdr.magic[1], hdr.magic[2]);
        return false;
    }

    // Starting at the type table offset, parse all types. Stop after parsing
    // the amount of types specified in the header.
    auto type_count = hdr.type_count;
    auto type_offset = hdr.type_table_offset;
    auto types_zero_index = types.size();
    types.reserve(types.size() + type_count);

    // NOTE: Big issue here is forward references. i.e. a pointer type at
    // index 3 that stores a pointee type index of 4. So, we need to know the
    // Type* of any given index /before/ we start parsing any of the types.
    // This is sort of difficult in C++ with stupid fucking inheritance.
    struct BasicFixup {
        // Type index of the type that needs fixing up.
        ModuleDescription::TypeIndex fixup_type_index;
    };
    struct PointerFixup : BasicFixup {
        ModuleDescription::TypeIndex pointee_type_index;

        PointerFixup(
            ModuleDescription::TypeIndex fixup_type,
            ModuleDescription::TypeIndex pointee_type
        ) : BasicFixup(fixup_type),
            pointee_type_index(pointee_type) {}
    };
    struct ReferenceFixup : BasicFixup {
        ModuleDescription::TypeIndex referent_type_index;

        ReferenceFixup(
            ModuleDescription::TypeIndex fixup_type,
            ModuleDescription::TypeIndex referent_type
        ) : BasicFixup(fixup_type),
            referent_type_index(referent_type) {}
    };
    std::vector<PointerFixup> ptr_fixups{};
    std::vector<ReferenceFixup> ref_fixups{};
    for (decltype(type_count) type_index = 0; type_index < type_count; ++type_index) {
        auto tag = module_metadata_blob.at(type_offset++);
        auto kind = Type::Kind(tag);
        switch (kind) {
            // NamedType: length :u32, name :u8[length]
            case Type::Kind::Named: {
                static constexpr auto length_size = sizeof(u32);
                std::array<u8, length_size> length_array{};
                for (unsigned i = 0; i < length_size; ++i)
                    length_array[i] = module_metadata_blob.at(type_offset++);
                u32 length = from_bytes<u32>(length_array);

                LCC_ASSERT(name().size(), "Deserialised named type has zero-length name");

                std::string deserialised_name{};
                for (u32 i = 0; i < length; ++i)
                    deserialised_name += char(module_metadata_blob.at(type_offset++));

                LCC_ASSERT(deserialised_name.size(), "Deserialised named type has empty name");

                // FIXME: This may need to be top level scope, not entirely sure the
                // semantics of this yet.
                new (*this) NamedType(deserialised_name, global_scope(), {});
            } break;

            // BuiltinType: builtin_kind :u8
            case Type::Kind::Builtin: {
                auto builtin_kind_value = module_metadata_blob.at(type_offset++);
                // clang-format off
                LCC_ASSERT(
                    builtin_kind_value == +BuiltinType::BuiltinKind::Bool
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Byte
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Int
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Unknown
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Void
                    or builtin_kind_value == +BuiltinType::BuiltinKind::OverloadSet,
                    "Invalid builtin kind value {}", builtin_kind_value
                );
                // clang-format on
                LCC_ASSERT(
                    builtin_kind_value != +BuiltinType::BuiltinKind::OverloadSet,
                    "Cannot deserialise overload sets; sorry"
                );
                auto builtin_kind = BuiltinType::BuiltinKind(builtin_kind_value);
                BuiltinType::Make(*this, builtin_kind, {});
            } break;

            // FFIType: ffi_kind :u16
            case Type::Kind::FFIType: {
                static constexpr auto ffi_kind_size = sizeof(u16);
                std::array<u8, ffi_kind_size> length_array{};
                for (unsigned i = 0; i < ffi_kind_size; ++i)
                    length_array[i] = module_metadata_blob.at(type_offset++);
                u16 ffi_kind_value = from_bytes<u16>(length_array);
                auto ffi_kind = FFIType::FFIKind(ffi_kind_value);
                FFIType::Make(*this, ffi_kind, {});
            } break;

            // PointerType, ReferenceType: type_index :TypeIndex
            case Type::Kind::Pointer:
            case Type::Kind::Reference: {
                static constexpr auto ref_type_index_size = sizeof(ModuleDescription::TypeIndex);
                std::array<u8, ref_type_index_size> ref_type_index_array{};
                for (unsigned i = 0; i < ref_type_index_size; ++i)
                    ref_type_index_array[i] = module_metadata_blob.at(type_offset++);
                auto ref_type_index = from_bytes<ModuleDescription::TypeIndex>(ref_type_index_array);

                types.push_back(nullptr);
                if (kind == Type::Kind::Pointer)
                    ptr_fixups.push_back({type_index, ref_type_index});
                else if (kind == Type::Kind::Reference)
                    ref_fixups.push_back({type_index, ref_type_index});
            } break;

            case Type::Kind::Array:
            case Type::Kind::DynamicArray:
            case Type::Kind::Function:
            case Type::Kind::Union:
            case Type::Kind::Enum:
            case Type::Kind::Struct:
            case Type::Kind::Integer:
                LCC_TODO("Parse type kind {} from binary module metadata", ToString(kind));
                break;
        }
    }

    for (auto ptr_fixup : ptr_fixups) {
        LCC_ASSERT(ptr_fixup.fixup_type_index < types.size());
        LCC_ASSERT(ptr_fixup.pointee_type_index < types.size());
        types[ptr_fixup.fixup_type_index] = types[ptr_fixup.pointee_type_index];
    }
    for (auto ref_fixup : ref_fixups) {
        LCC_ASSERT(ref_fixup.fixup_type_index < types.size());
        LCC_ASSERT(ref_fixup.referent_type_index < types.size());
        types[ref_fixup.fixup_type_index] = types[ref_fixup.referent_type_index];
    }

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

        auto* ty = types.at(types_zero_index + decl_hdr.type_index);

        // FIXME: Should it be top level scope instead of global?
        auto* scope = global_scope();
        switch (ModuleDescription::DeclarationHeader::Kind(decl_hdr.kind)) {
            // Created from Expr::Kind::TypeDecl
            case ModuleDescription::DeclarationHeader::Kind::TYPE: {
                LCC_ASSERT(
                    is<DeclaredType>(ty),
                    "Can't make TypeDecl from a Type that is not derived from DeclaredType"
                );
                auto type_decl = new (*this) TypeDecl(this, name, as<DeclaredType>(ty), {});
                type_decl->set_sema_done();
                auto decl = scope->declare(ctx, std::string(name), type_decl);
            } break;

            // Created from Expr::Kind::TypeAliasDecl
            case ModuleDescription::DeclarationHeader::Kind::TYPE_ALIAS: {
                auto* type_alias_decl = new (*this) TypeAliasDecl(name, ty, {});
                type_alias_decl->set_sema_done();
                auto decl = scope->declare(ctx, std::string(name), type_alias_decl);
            } break;

            // Created from Expr::Kind::VarDecl
            case ModuleDescription::DeclarationHeader::Kind::VARIABLE: {
                // FIXME: Should possibly be reexported.
                auto* var_decl = new (*this) VarDecl(name, ty, nullptr, this, Linkage::Imported, {});
                var_decl->set_sema_done();
                auto decl = scope->declare(ctx, std::string(name), var_decl);
            } break;

            // Created from Expr::Kind::FuncDecl
            case ModuleDescription::DeclarationHeader::Kind::FUNCTION: {
                LCC_ASSERT(
                    is<FuncType>(ty),
                    "Cannot create FuncDecl when deserialised type is not a function"
                );
                auto* func_decl = new (*this) FuncDecl(name, as<FuncType>(ty), nullptr, scope, this, Linkage::Imported, {});
                func_decl->set_sema_done();
                auto decl = scope->declare(ctx, std::string(name), func_decl);
            } break;

            // Created from Expr::Kind::EnumeratorDecl
            // FIXME: Is it possible to export a single enum decl? I'm pretty sure the
            // only place an enum decl can happen is within an enum type.
            case ModuleDescription::DeclarationHeader::Kind::ENUMERATOR: {
                LCC_TODO("Make an EnumeratorDecl in the global scope");
            }

            case ModuleDescription::DeclarationHeader::Kind::INVALID:
            default:
                LCC_ASSERT(false, "Invalid declaration kind in declaration header: {}", decl_hdr.kind);
        }
    }

    return true;
}
