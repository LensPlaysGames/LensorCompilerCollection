#include <lcc/file.hh>
#include <lcc/utils.hh>

#include <glint/ast.hh>
#include <glint/module_description.hh>

#include <algorithm>
#include <array>
#include <cstring>
#include <iterator>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

lcc::glint::Module::Module(
    File* file,
    std::string module_name,
    Module::ModuleStatus is_logical_module
) : _name{std::move(module_name)},
    _is_module{is_logical_module},
    _file{file} //
{
    FuncType* ty{};

    /// Create the type of the top-level function.
    if (is_logical_module) {
        ty = new (*this) FuncType({}, BuiltinType::Void(*this), {}, {});
    } else {
        auto* cint_ty = FFIType::CInt(*this);

        auto* cchar_ty = FFIType::CChar(*this);
        auto* char_ptr = new (*this) PointerType{new (*this) PointerType{cchar_ty}};

        ty = new (*this) FuncType{
            {
                {"__argc__", cint_ty, {}},
                {"__argv__", char_ptr, {}},
                {"__envp__", char_ptr, {}},
            },

            /// We currently set main() to return `int` since that’s natural for
            /// most programs.
            Type::Int,
            {},
            {},
        };
    }

    // Don't mangle name of top level function.
    ty->set_attr(FuncAttr::NoMangle);

    Location loc{};
    if (file) {
        loc = Location{
            0,
            (decltype(Location::len)) file->size(),
            (decltype(Location::file_id)) file->file_id() //
        };
    }
    _top_level_function
        = new (*this) FuncDecl{
            is_logical_module ? InitFunctionName(name()) : "main",
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
    as<BlockExpr>(top_level_function()->body())->add(node);
}

auto lcc::glint::Module::intern(std::string_view str) -> usz {
    auto it = rgs::find(strings, str);
    if (it != strings.end()) { return usz(std::distance(strings.begin(), it)); }
    strings.emplace_back(str);
    return strings.size() - 1;
}

/// ================
/// Serialisation
/// ================

// FIXME: This should probably be backwards for big endian machines, afaik.
template <typename T>
auto to_bytes(const T object) -> std::array<lcc::u8, sizeof(T) / sizeof(lcc::u8)> {
    std::array<lcc::u8, sizeof(T)> out{};
    const lcc::u8* begin = reinterpret_cast<const lcc::u8*>(&object);
    const lcc::u8* end = begin + (sizeof(T));
    std::copy(begin, end, out.begin());
    return out;
}

// FIXME: This should probably be backwards for big endian machines, afaik.
// requires std::is_trivially_constructible?
template <typename T>
auto from_bytes(std::array<lcc::u8, sizeof(T)> bytes) -> T {
    T out{};
    std::memcpy(&out, bytes.data(), sizeof(T));
    return out;
}

auto lcc::glint::Module::serialise(
    std::vector<u8>& out,
    std::vector<Type*>& cache,
    Type* ty
) -> lcc::u16 {
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
            static_assert(
                +FuncAttr::COUNT == 11,
                "Exhaustive handling of function attributes in module deserialisation"
            );
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
        //     underlying_type_index :TypeIndex
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

        // ArrayViewType: element_type_index :TypeIndex
        case Type::Kind::ArrayView: {
            auto* type = as<ArrayViewType>(ty);

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

        // TODO: Will be a lot like a struct
        case Type::Kind::Sum: {
            LCC_TODO("Serialise sum type (just haven't yet)");
        } break;

        // TODO: Will be a lot like a struct
        case Type::Kind::Union: {
            LCC_TODO("Serialise union type (it's easy, just haven't yet)");
        } break;

        case Type::Kind::Typeof:
            LCC_ASSERT(false, "Sema should have replaced TypeofType with the type of it's contained expression");
    }

    return type_index;
}

std::vector<lcc::u8> lcc::glint::Module::serialise() {
    ModuleDescription::Header hdr{};
    std::vector<u8> declarations{};
    std::vector<u8> types_data{};
    std::vector<u8> serialised_name{};

    // Decls and types collected from exports.
    std::vector<Type*> type_cache{};
    for (auto* decl : exports) {
        // Decl: DeclHeader, length :u8, name :u8[length]

        // Prepare declaration header
        ModuleDescription::TypeIndex type_index = serialise(types_data, type_cache, decl->type());
        ModuleDescription::DeclarationHeader decl_hdr{
            u16(ModuleDescription::DeclarationHeader::get_kind(decl)),
            type_index
        };
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
        not name().empty(),
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
    hdr.size = u32(sizeof(ModuleDescription::Header) + declarations.size() + types_data.size() + serialised_name.size());
    hdr.type_table_offset = u32(sizeof(ModuleDescription::Header) + declarations.size());
    hdr.name_offset = u32(sizeof(ModuleDescription::Header) + declarations.size() + types_data.size());
    hdr.declaration_count = u16(exports.size());
    hdr.type_count = u16(type_cache.size());

    // Convert header to byte representation that is easy to serialise.
    auto hdr_bytes = to_bytes(hdr);

    std::vector<u8> out{};
    out.insert(out.end(), hdr_bytes.begin(), hdr_bytes.end());
    out.insert(out.end(), declarations.begin(), declarations.end());
    out.insert(out.end(), types_data.begin(), types_data.end());
    out.insert(out.end(), serialised_name.begin(), serialised_name.end());
    return out;
}

auto lcc::glint::Module::deserialise(
    lcc::Context* context,
    std::vector<u8> module_metadata_blob
) -> bool {
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
    if (
        hdr.magic[0] != ModuleDescription::magic_byte0
        or hdr.magic[1] != ModuleDescription::magic_byte1
        or hdr.magic[2] != ModuleDescription::magic_byte2
    ) {
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
    struct BasicFixup {
        // Type index of the type that needs fixing up. This index will be
        // overwritten with the type at the replacement type index.
        ModuleDescription::TypeIndex fixup_type_index;
        ModuleDescription::TypeIndex replacement_type_index;
        /// FIXUP is replaced with REPLACEMENT
        BasicFixup(
            u16 zero_index,
            ModuleDescription::TypeIndex fixup_index,
            ModuleDescription::TypeIndex replacement_index
        ) : fixup_type_index(zero_index + fixup_index),
            replacement_type_index(zero_index + replacement_index) {}
    };

    auto validate_fixup = [this, types_zero_index](const BasicFixup& fixup) {
        constexpr auto low_msg
            = "You probably forgot to account for the non-zero zero index into the types container.";
        constexpr auto high_msg
            = "You really messed up creating a fixup that is entirely out of range of the types container.\n  fixup:{}  size:{}\n";
        LCC_ASSERT(
            fixup.fixup_type_index >= types_zero_index,
            low_msg
        );
        LCC_ASSERT(
            fixup.replacement_type_index >= types_zero_index,
            low_msg
        );
        LCC_ASSERT(
            fixup.fixup_type_index < types.size(),
            high_msg,
            fixup.fixup_type_index,
            types.size()
        );
        LCC_ASSERT(
            fixup.replacement_type_index < types.size(),
            high_msg,
            fixup.replacement_type_index,
            types.size()
        );
    };

    struct FunctionFixup {
        // Type index of the type that needs fixing up. This index will be
        // overwritten with the type at the replacement type index.
        u32 function_attributes;
        ModuleDescription::TypeIndex function_type_index;
        ModuleDescription::TypeIndex return_type_index;
        std::vector<ModuleDescription::TypeIndex> param_type_indices;
        std::vector<std::string> param_names;
        /// FIXUP is replaced with REPLACEMENT
        FunctionFixup(
            u16 zero_index,
            ModuleDescription::TypeIndex function_index,
            ModuleDescription::TypeIndex return_index,
            std::vector<ModuleDescription::TypeIndex> param_types,
            std::vector<std::string> param_names_,
            u32 attributes
        ) : function_attributes(attributes), function_type_index(zero_index + function_index),
            return_type_index(zero_index + return_index),
            param_type_indices(std::move(param_types)),
            param_names(std::move(param_names_)) {
            for (auto& index : param_type_indices)
                index += zero_index;
        }
    };

    auto validate_function_fixup = [this, types_zero_index](const FunctionFixup& fixup) {
        constexpr auto low_msg
            = "You probably forgot to account for the non-zero zero index into the types container.";
        constexpr auto high_msg
            = "You really messed up creating a fixup that is entirely out of range of the types container.\n  fixup:{}  size:{}\n";

        // Bounds-check index to be replaced with a function type.
        LCC_ASSERT(
            fixup.function_type_index >= types_zero_index,
            low_msg
        );
        LCC_ASSERT(
            fixup.function_type_index < types.size(),
            high_msg,
            fixup.function_type_index,
            types.size()
        );

        // Bounds-check return type index.
        LCC_ASSERT(
            fixup.return_type_index >= types_zero_index,
            low_msg
        );
        LCC_ASSERT(
            fixup.return_type_index < types.size(),
            high_msg,
            fixup.return_type_index,
            types.size()
        );

        // Bounds-check all parameter indices.
        for (auto param_i : fixup.param_type_indices) {
            LCC_ASSERT(
                param_i >= types_zero_index,
                low_msg
            );
            LCC_ASSERT(
                param_i < types.size(),
                high_msg,
                param_i,
                types.size()
            );
        }

        // Parameter names and type indices have to map 1:1 (same amount)
        LCC_ASSERT(
            fixup.param_type_indices.size() == fixup.param_names.size(),
            "Mismatch in deserialised type and name count"
        );
    };

    std::vector<BasicFixup> ptr_fixups{};
    std::vector<BasicFixup> ref_fixups{};
    std::vector<BasicFixup> dynarray_fixups{};
    std::vector<BasicFixup> view_fixups{};
    std::vector<FunctionFixup> function_fixups{};

    for (decltype(type_count) type_index = 0; type_index < type_count; ++type_index) {
        auto tag = module_metadata_blob.at(type_offset++);
        auto kind = Type::Kind(tag);
        switch (kind) {
            // NamedType: length :u32, name :u8[length]
            case Type::Kind::Named: {
                constexpr auto length_size = sizeof(u32);
                std::array<u8, length_size> length_array{};
                for (unsigned i = 0; i < length_size; ++i)
                    length_array[i] = module_metadata_blob.at(type_offset++);
                u32 length = from_bytes<u32>(length_array);

                LCC_ASSERT(not name().empty(), "Deserialised named type has zero-length name");

                std::string deserialised_name{};
                for (u32 i = 0; i < length; ++i)
                    deserialised_name += char(module_metadata_blob.at(type_offset++));

                LCC_ASSERT(not deserialised_name.empty(), "Deserialised named type has empty name");

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
                // Purely for the side-effect of recording the type in the module.
                (void) BuiltinType::Make(*this, builtin_kind, {});
            } break;

            // FFIType: ffi_kind :u16
            case Type::Kind::FFIType: {
                constexpr auto ffi_kind_size = sizeof(u16);
                std::array<u8, ffi_kind_size> length_array{};
                for (unsigned i = 0; i < ffi_kind_size; ++i)
                    length_array[i] = module_metadata_blob.at(type_offset++);
                u16 ffi_kind_value = from_bytes<u16>(length_array);
                auto ffi_kind = FFIType::FFIKind(ffi_kind_value);
                // Purely for the side-effect of recording the type in the module.
                (void) FFIType::Make(*this, ffi_kind, {});
            } break;

            // PointerType, ReferenceType: type_index :TypeIndex
            case Type::Kind::ArrayView:
            case Type::Kind::DynamicArray:
            case Type::Kind::Pointer:
            case Type::Kind::Reference: {
                constexpr auto ref_type_index_size = sizeof(ModuleDescription::TypeIndex);
                std::array<u8, ref_type_index_size> ref_type_index_array{};
                for (unsigned i = 0; i < ref_type_index_size; ++i)
                    ref_type_index_array.at(i) = module_metadata_blob.at(type_offset++);
                auto ref_type_index = from_bytes<ModuleDescription::TypeIndex>(ref_type_index_array);

                // Normally done in operator new of Type, but we do it manually here since
                // we can't new the pointer type until we have the pointer to the element
                // type, and we (may) have not deserialised that yet.
                types.push_back(nullptr);

                if (kind == Type::Kind::Pointer)
                    ptr_fixups.emplace_back(types_zero_index, type_index, ref_type_index);
                else if (kind == Type::Kind::Reference)
                    ref_fixups.emplace_back(types_zero_index, type_index, ref_type_index);
                else if (kind == Type::Kind::DynamicArray)
                    dynarray_fixups.emplace_back(types_zero_index, type_index, ref_type_index);
                else if (kind == Type::Kind::ArrayView)
                    view_fixups.emplace_back(types_zero_index, type_index, ref_type_index);
                else LCC_TODO("Unhandled type kind in element type deserialisation");
            } break;

            // IntegerType: bitwidth :u16, is_signed :u8
            case Type::Kind::Integer: {
                constexpr auto bitwidth_size = sizeof(u16);
                std::array<u8, bitwidth_size> bitwidth_array{};
                for (unsigned i = 0; i < bitwidth_size; ++i)
                    bitwidth_array[i] = module_metadata_blob.at(type_offset++);
                auto bitwidth = from_bytes<u16>(bitwidth_array);

                u8 is_signed = bitwidth_array[type_offset++];
                new (*this) IntegerType(bitwidth, is_signed, {});
            } break;

            // ArrayType: element_type_index :TypeIndex, element_count :u64
            case Type::Kind::Array: {
                constexpr auto element_type_index_size = sizeof(ModuleDescription::TypeIndex);
                std::array<u8, element_type_index_size> element_type_index_array{};
                for (unsigned i = 0; i < element_type_index_size; ++i)
                    element_type_index_array[i] = module_metadata_blob.at(type_offset++);
                auto element_type_index = from_bytes<ModuleDescription::TypeIndex>(element_type_index_array);

                constexpr auto element_count_size = sizeof(u64);
                LCC_ASSERT(type_offset + element_count_size <= module_metadata_blob.size());
                std::array<u8, element_count_size> element_count_array{};
                for (unsigned i = 0; i < element_count_size; ++i)
                    element_count_array[i] = module_metadata_blob.at(type_offset++);
                auto element_count = from_bytes<u64>(element_count_array);

                (void) element_type_index;
                (void) element_count;
                LCC_TODO("DESERIALISE: Implement ArrayType fixups and make one of those");
            } break;

            // FunctionType:
            //     attributes :u32
            //     param_count :u16
            //     param_types :TypeIndex[param_count]
            //     return_type :TypeIndex
            //     param_names :(param_name_length :u16, param_name :u8[param_name_length])[param_count]
            case Type::Kind::Function: {
                // Attributes
                constexpr auto attributes_size = sizeof(u32);
                std::array<u8, attributes_size> attributes_array{};
                for (unsigned i = 0; i < attributes_size; ++i)
                    attributes_array[i] = module_metadata_blob.at(type_offset++);
                auto attributes = from_bytes<u32>(attributes_array);

                // Parameter Count
                constexpr auto param_count_size = sizeof(u16);
                std::array<u8, param_count_size> param_count_array{};
                for (unsigned i = 0; i < param_count_size; ++i)
                    param_count_array[i] = module_metadata_blob.at(type_offset++);
                auto param_count = from_bytes<u16>(param_count_array);

                // Parameter Type Indices
                std::vector<ModuleDescription::TypeIndex> param_type_indices{};
                for (usz param_index = 0; param_index < param_count; ++param_index) {
                    constexpr auto param_type_index_size = sizeof(ModuleDescription::TypeIndex);
                    std::array<u8, param_type_index_size> param_type_index_array{};
                    for (unsigned i = 0; i < param_type_index_size; ++i)
                        param_type_index_array[i] = module_metadata_blob.at(type_offset++);
                    auto param_type_index = from_bytes<ModuleDescription::TypeIndex>(param_type_index_array);

                    param_type_indices.emplace_back(param_type_index);
                }

                // Return Type Index
                constexpr auto return_type_index_size = sizeof(ModuleDescription::TypeIndex);
                std::array<u8, return_type_index_size> return_type_index_array{};
                for (unsigned i = 0; i < return_type_index_size; ++i)
                    return_type_index_array[i] = module_metadata_blob.at(type_offset++);
                auto return_type_index = from_bytes<ModuleDescription::TypeIndex>(return_type_index_array);

                // Parameter Names
                std::vector<std::string> param_names{};
                for (usz param_index = 0; param_index < param_count; ++param_index) {
                    constexpr auto param_name_length_size = sizeof(u16);
                    std::array<u8, param_name_length_size> param_name_length_array{};
                    for (unsigned i = 0; i < param_name_length_size; ++i)
                        param_name_length_array[i] = module_metadata_blob.at(type_offset++);
                    auto param_name_length = from_bytes<u16>(param_name_length_array);

                    std::string param_name{};
                    param_name.reserve(param_name_length);
                    for (unsigned i = 0; i < param_name_length; ++i)
                        param_name += char(module_metadata_blob.at(type_offset++));

                    param_names.emplace_back(std::move(param_name));
                }

                types.push_back(nullptr);
                function_fixups.emplace_back(
                    types_zero_index,
                    type_index,
                    return_type_index,
                    std::move(param_type_indices),
                    std::move(param_names),
                    attributes
                );
            } break;

            // StructType:
            //     size_in_bytes :u16
            //     align_in_bytes :u16
            //     member_count :u16
            //     member_types :TypeIndex[member_count]
            //     member_data :(byte_offset :u16, name_length :u16, member_name :u8[name_length])[member_count]
            case Type::Kind::Struct:

            // EnumType:
            //     underlying_type_index :TypeIndex
            //     enum_decl_count :u32
            //     enum_decls :u64[enum_decl_count]
            // FIXME: Ideally we should get the size of the value (enum_decls element
            // type) from the underlying type, instead of just assuming it fits in 64
            // bits.
            case Type::Kind::Enum:

            case Type::Kind::Sum:
            case Type::Kind::Union:
                LCC_TODO("Parse type kind {} from binary module metadata", ToString(kind));
                break;

            case Type::Kind::Typeof:
                LCC_ASSERT(false, "Sema should have replaced TypeofType with the type of it's contained expression");
        }
    }

    for (auto fixup : ptr_fixups) {
        validate_fixup(fixup);
        // NOTE: Since new adds the newed type to the types container, we end up
        // with the same type duplicated, so we have to remove the one that gets
        // added automatically... If we just left it, we would try to delete it
        // twice, and that causes it's own whole host of errors.
        int types_size = int(types.size());
        types[fixup.fixup_type_index] = new (*this) PointerType(
            types[fixup.replacement_type_index]
        );
        types.erase(types.begin() + types_size);
    }

    for (auto fixup : ref_fixups) {
        validate_fixup(fixup);
        int types_size = int(types.size());
        types[fixup.fixup_type_index] = new (*this) ReferenceType(
            types[fixup.replacement_type_index],
            {}
        );
        types.erase(types.begin() + types_size);
    }

    for (auto fixup : dynarray_fixups) {
        validate_fixup(fixup);
        int types_size = int(types.size());
        types[fixup.fixup_type_index] = new (*this) DynamicArrayType(
            types[fixup.replacement_type_index],
            nullptr
        );
        types.erase(types.begin() + types_size);
    }

    for (auto fixup : view_fixups) {
        validate_fixup(fixup);
        int types_size = int(types.size());
        types[fixup.fixup_type_index] = new (*this) ArrayViewType(
            types[fixup.replacement_type_index]
        );
        types.erase(types.begin() + types_size);
    }

    for (auto fixup : function_fixups) {
        validate_function_fixup(fixup);
        int types_size = int(types.size());

        std::vector<FuncType::Param> params{};
        usz param_i{0};
        for (auto param_type_index : fixup.param_type_indices) {
            params.emplace_back(
                fixup.param_names.at(param_i),
                types[param_type_index],
                Location{}
            );
            ++param_i;
        }

        auto* function = new (*this) FuncType(
            std::move(params),
            types[fixup.return_type_index],
            {},
            {}
        );
        types[fixup.function_type_index] = function;

        const auto set_attr = [fixup, function](FuncAttr f) {
            if (fixup.function_attributes & (u32(1) << u32(f)))
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

        types.erase(types.begin() + types_size);
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
                auto decl = scope->declare(context, std::string(name), type_decl);
            } break;

            // Created from Expr::Kind::TypeAliasDecl
            case ModuleDescription::DeclarationHeader::Kind::TYPE_ALIAS: {
                auto* type_alias_decl = new (*this) TypeAliasDecl(name, ty, {});
                type_alias_decl->set_sema_done();
                auto decl = scope->declare(context, std::string(name), type_alias_decl);
            } break;

            // Created from Expr::Kind::VarDecl
            case ModuleDescription::DeclarationHeader::Kind::VARIABLE: {
                // FIXME: Should possibly be reexported.
                auto* var_decl = new (*this) VarDecl(name, ty, nullptr, this, Linkage::Imported, {});
                var_decl->set_lvalue();
                var_decl->set_sema_done();
                auto decl = scope->declare(context, std::string(name), var_decl);
            } break;

            // Created from Expr::Kind::FuncDecl
            case ModuleDescription::DeclarationHeader::Kind::FUNCTION: {
                LCC_ASSERT(
                    is<FuncType>(ty),
                    "Cannot create FuncDecl when deserialised type, {}, is not a function",
                    *ty
                );
                auto* func_decl = new (*this) FuncDecl(name, as<FuncType>(ty), nullptr, scope, this, Linkage::Imported, {});
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

            case ModuleDescription::DeclarationHeader::Kind::INVALID:
            default:
                LCC_ASSERT(false, "Invalid declaration kind in declaration header: {}", decl_hdr.kind);
        }
    }

    std::string module_name{};
    for (auto i = module_metadata_blob.begin() + hdr.name_offset; *i; ++i)
        module_name += (char) *i;

    auto init_function_decl = global_scope()->declare(
        context,
        Module::InitFunctionName(module_name),
        new (*this) FuncDecl(
            Module::InitFunctionName(module_name),
            new (*this) FuncType({}, Type::Void, {{FuncAttr::NoMangle, true}}, {}),
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
