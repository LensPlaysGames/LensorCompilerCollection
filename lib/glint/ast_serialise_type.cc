#include <glint/ast.hh>
#include <glint/module_description.hh>

#include <lcc/utils.hh>

#include <algorithm>
#include <limits>
#include <unordered_map>
#include <vector>

auto lcc::glint::Module::serialise(
    std::vector<u8>& out,
    std::vector<Type*>& cache,
    std::vector<Type*>& current,
    std::unordered_map<Type*, ModuleDescription::TypeIndex> indices,
    Type* ty
) -> lcc::u16 {
    using lcc::utils::to_bytes;

    auto found = rgs::find(cache, ty);
    if (found != cache.end())
        return u16(found - cache.begin());

    // Handle self-referencing type (type used in it's own definition).
    auto found_current = rgs::find(current, ty);
    if (found_current != current.end())
        return ModuleDescription::TypeIndex(indices.at(ty));

    LCC_ASSERT(
        cache.size() < 0xffff,
        "Too many types, cannot serialise in binary metadata format version 1"
    );
    current.push_back(ty);

    // For later confidence check
    bool tag_written = false;

    // Helper that should be called before serialising "this" type (i.e. "ty"
    // parameter). We can't do it now due to post-order traversal possibly
    // serialising children first.
    const auto write_tag = [&]() {
        u8 tag = u8(ty->kind());
        out.push_back(tag);
        tag_written = true;
    };

    switch (ty->kind()) {
        // NamedType: length :u16, name :u8[length]
        case Type::Kind::Named: {
            auto* type = as<NamedType>(ty);
            auto type_name = type->name();

            u16 name_length = u16(type_name.length());
            auto name_length_bytes = to_bytes(name_length);

            write_tag();
            // Write `length: u16`
            out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
            // Write `name : u8[length]`
            out.insert(out.end(), type_name.begin(), type_name.end());
        } break;

        // PointerType, ReferenceType: type_index :TypeIndex
        case Type::Kind::Pointer:
        case Type::Kind::Reference: {
            // Serialise the referenced type.
            auto referenced_type_index = serialise(out, cache, current, indices, ty->elem());

            // Convert the type index to it's binary format (an array of bytes).
            auto type_index_bytes = to_bytes(referenced_type_index);

            write_tag();
            // Write `type_index: u16`.
            out.insert(out.end(), type_index_bytes.begin(), type_index_bytes.end());
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

            write_tag();
            // Write `bitwidth :u16`.
            out.insert(out.end(), bitwidth_bytes.begin(), bitwidth_bytes.end());
            // Write `is_signed :u8`.
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

            write_tag();
            // Write `builtin_kind :u8`.
            out.push_back(builtin_kind);
        } break;

        // FFIType: ffi_kind :u16
        case Type::Kind::FFIType: {
            FFIType* type = as<FFIType>(ty);
            u16 ffi_kind = u16(type->ffi_kind());
            auto ffi_kind_bytes = to_bytes(ffi_kind);

            write_tag();
            // Write `ffi_kind :u16`.
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
            auto return_type = serialise(out, cache, current, indices, type->return_type());
            auto return_type_bytes = to_bytes(return_type);

            // Serialise parameter types and fixup parameter type indices previously
            // allocated.
            std::vector<ModuleDescription::TypeIndex> param_types{};
            for (auto param : type->params())
                param_types.push_back(serialise(out, cache, current, indices, param.type));

            write_tag();

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
            for (auto param_type : param_types) {
                auto param_type_bytes = to_bytes(param_type);
                out.insert(out.end(), param_type_bytes.begin(), param_type_bytes.end());
            }

            out.insert(out.end(), return_type_bytes.begin(), return_type_bytes.end());

            // Write parameter names
            for (auto& param : type->params()) {
                auto name_length = u16(param.name.length());
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
                out.insert(out.end(), param.name.begin(), param.name.end());
            }
        } break;

        // EnumType:
        //     underlying_type_index :TypeIndex
        //     enum_decl_count :u32
        //     enum_decls :u64[enum_decl_count]
        //     enum_names :(name_length :u16, name :u8[name_length])[enum_decl_count]
        // FIXME: Ideally we should get the size of the value (enum_decls element
        // type) from the underlying type, instead of just assuming it fits in 64
        // bits.
        case Type::Kind::Enum: {
            auto type = as<EnumType>(ty);

            // TODO: Assert that underlying type can fit in 64 bits. But we don't have
            // context here (yet).

            auto underlying_type = serialise(out, cache, current, indices, type->underlying_type());
            auto underlying_type_bytes = to_bytes(underlying_type);

            write_tag();

            out.insert(out.end(), underlying_type_bytes.begin(), underlying_type_bytes.end());

            // Write enumerator count.
            u32 enum_decl_count = u32(type->enumerators().size());
            auto enum_decl_count_bytes = to_bytes(enum_decl_count);
            out.insert(out.end(), enum_decl_count_bytes.begin(), enum_decl_count_bytes.end());

            // Write enumerator values.
            for (auto* enum_decl : type->enumerators()) {
                u64 value = enum_decl->value().value();
                auto value_bytes = to_bytes(value);
                out.insert(out.end(), value_bytes.begin(), value_bytes.end());
            }

            // Write enumerator names.
            for (auto* enum_decl : type->enumerators()) {
                LCC_ASSERT(
                    enum_decl->name().length() <= std::numeric_limits<u16>::max(),
                    "Enumerator has over-long name, cannot export..."
                );
                u16 name_length = (u16) enum_decl->name().length();
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
                auto enum_decl_name = enum_decl->name();
                out.insert(out.end(), enum_decl_name.begin(), enum_decl_name.end());
            }
        } break;

        // ArrayViewType: element_type_index :TypeIndex
        case Type::Kind::ArrayView: {
            auto* type = as<ArrayViewType>(ty);

            auto element_type = serialise(out, cache, current, indices, type->element_type());
            auto element_type_bytes = to_bytes(element_type);

            write_tag();
            out.insert(out.end(), element_type_bytes.begin(), element_type_bytes.end());
        } break;

        // DynamicArrayType: element_type_index :TypeIndex
        case Type::Kind::DynamicArray: {
            auto* type = as<DynamicArrayType>(ty);

            auto element_type = serialise(out, cache, current, indices, type->elem());
            auto element_type_bytes = to_bytes(element_type);

            write_tag();
            out.insert(out.end(), element_type_bytes.begin(), element_type_bytes.end());
        } break;

        // ArrayType: element_type_index :u16, element_count :u64
        case Type::Kind::Array: {
            auto type = as<ArrayType>(ty);

            // Serialise the array's element type.
            auto element_type = serialise(out, cache, current, indices, type->element_type());

            // Convert the element type to it's binary format.
            auto element_type_bytes = to_bytes(element_type);

            // Get element count
            u64 element_count = u64(type->dimension());

            // Convert the element count value to it's binary format.
            auto element_count_bytes = to_bytes(element_count);

            write_tag();

            // Write `element_type_index :u16`.
            out.insert(out.end(), element_type_bytes.begin(), element_type_bytes.end());

            // Write `element_count :u64`.
            out.insert(out.end(), element_count_bytes.begin(), element_count_bytes.end());
        } break;

        // serialise()
        // StructType:
        //     size_in_bytes :u16
        //     align_in_bytes :u16
        //     member_count :u16
        //     member_types :TypeIndex[member_count]
        //     member_data :(byte_offset :u16, name_length :u16, member_name :u8[name_length])[member_count]
        case Type::Kind::Struct: {
            auto type = as<StructType>(ty);

            auto& members = type->members();

            std::vector<ModuleDescription::TypeIndex> member_types{};
            for (auto& member : members)
                member_types.push_back(serialise(out, cache, current, indices, member.type));

            write_tag();

            // Struct Size
            auto size_in_bytes = u16(type->byte_size());
            auto size_in_bytes_bytes = to_bytes(size_in_bytes);
            out.insert(out.end(), size_in_bytes_bytes.begin(), size_in_bytes_bytes.end());

            // Struct Alignment
            auto align_in_bytes = u16(type->alignment());
            auto align_in_bytes_bytes = to_bytes(align_in_bytes);
            out.insert(out.end(), align_in_bytes_bytes.begin(), align_in_bytes_bytes.end());

            // Member Count
            auto member_count = u16(members.size());
            auto member_count_bytes = to_bytes(member_count);
            out.insert(out.end(), member_count_bytes.begin(), member_count_bytes.end());

            // Member Types
            for (auto member_type : member_types) {
                auto member_type_bytes = to_bytes(member_type);
                out.insert(out.end(), member_type_bytes.begin(), member_type_bytes.end());
            }

            // Member Data
            for (const auto& member : members) {
                auto byte_offset = u16(member.byte_offset);
                auto byte_offset_bytes = to_bytes(byte_offset);
                out.insert(out.end(), byte_offset_bytes.begin(), byte_offset_bytes.end());

                auto name_length = u16(member.name.length());
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());

                out.insert(out.end(), member.name.begin(), member.name.end());
            }
        } break;

        // serialise()
        // SumType:
        //     member_count :u16
        //     member_types :TypeIndex[member_count]
        //     member_data  :(name_length :u16, member_name :u8[name_length])[member_count]
        case Type::Kind::Sum: {
            auto type = as<SumType>(ty);

            auto& members = type->members();

            std::vector<ModuleDescription::TypeIndex> member_types{};
            for (auto& member : members)
                member_types.push_back(serialise(out, cache, current, indices, member.type));

            write_tag();

            auto member_count = u16(members.size());
            auto member_count_bytes = to_bytes(member_count);
            out.insert(out.end(), member_count_bytes.begin(), member_count_bytes.end());

            for (auto member_type : member_types) {
                auto member_type_bytes = to_bytes(member_type);
                out.insert(out.end(), member_type_bytes.begin(), member_type_bytes.end());
            }

            // Member Data
            for (const auto& member : members) {
                auto name_length = u16(member.name.length());
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
                out.insert(out.end(), member.name.begin(), member.name.end());
            }
        } break;

        // UnionType:
        //     member_count :u16
        //     member_types :TypeIndex[member_count]
        //     member_data  :(name_length :u16, member_name :u8[name_length])[member_count]
        case Type::Kind::Union: {
            auto type = as<UnionType>(ty);

            auto& members = type->members();

            std::vector<ModuleDescription::TypeIndex> member_types{};
            for (auto& member : members)
                member_types.push_back(serialise(out, cache, current, indices, member.type));

            write_tag();

            auto member_count = u16(members.size());
            auto member_count_bytes = to_bytes(member_count);
            out.insert(out.end(), member_count_bytes.begin(), member_count_bytes.end());

            for (auto member_type : member_types) {
                auto member_type_bytes = to_bytes(member_type);
                out.insert(out.end(), member_type_bytes.begin(), member_type_bytes.end());
            }

            // Member Data
            for (const auto& member : members) {
                auto name_length = u16(member.name.length());
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
                out.insert(out.end(), member.name.begin(), member.name.end());
            }
        } break;

        case Type::Kind::Typeof:
            LCC_ASSERT(false, "Sema should have replaced TypeofType with the type of it's contained expression");
    }

    LCC_ASSERT(
        tag_written,
        "You forgot to call write_tag() when implementing serialisation of a new type, most likely"
    );

    std::erase(current, ty);

    auto type_index = ModuleDescription::TypeIndex(cache.size());

    LCC_ASSERT(
        indices.at(ty) == type_index,
        "Mismatch in AoT index calculation and actual serialisation"
    );

    cache.emplace_back(ty);

    return type_index;
}
