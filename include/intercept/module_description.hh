#ifndef LCC_INTERCEPT_MODULE_DESCRIPTION_HH
#define LCC_INTERCEPT_MODULE_DESCRIPTION_HH

#include <intercept/ast.hh>

namespace lcc {
namespace intercept {
/// For serialisation purposes.
/// If you know how ELF works, you'll find this familiar and pretty
/// easy-going. If not, hopefully the comments help you out along the way.
///
/// OVERALL STRUCTURE of BINARY METADATA BLOB version 1:
///
/// Beginning of file       type_table_offset  name_offset
/// V                       V                  V
/// Header { Declarations } { Types }          [ Module Name ]
///
/// A declaration is encoded as a DeclarationHeader + N amount of bytes
/// determined by the values in the declaration header.
struct ModuleDescription {
    using TypeIndex = u16;

    // Default/expected values.
    static constexpr u8 default_version = 1;
    static constexpr u8 magic_byte0 = 'I';
    static constexpr u8 magic_byte1 = 'N';
    static constexpr u8 magic_byte2 = 'T';
    struct Header {
        u8 version{default_version};
        u8 magic[3]{
            magic_byte0,
            magic_byte1,
            magic_byte2,
        };

        /// Size, in 8-bit bytes, of this binary metadata blob, including this header.
        u32 size;

        /// The offset within this binary metadata blob at which you will find the
        /// beginning of the type table. Value undefined iff type_count is zero.
        u32 type_table_offset;

        /// The offset within this binary metadata blob at which you will find the
        /// beginning of a NULL-terminated string: the name of the serialised
        /// module.
        u32 name_offset;

        /// The amount of declarations encoded in this binary metadata blob. Once
        /// this many declarations have been deserialised, the reader should stop
        /// reading.
        u16 declaration_count;

        /// The amount of types encoded in this binary metadata blob. Determines
        /// maximum exclusive allowed value of declaration type_index field.
        /// Once the reader deserialised this many types, the reader should stop
        /// reading types.
        u16 type_count;
    };

    struct DeclarationHeader {
        enum struct Kind : u16 {
            INVALID,
            TYPE,
            TYPE_ALIAS,
            ENUMERATOR,
            VARIABLE,
            FUNCTION,
        };

        // One of DeclarationHeader::Kind.
        // Untyped to avoid UB when deserialising.
        u16 kind;
        TypeIndex type_index;

        static constexpr Kind get_kind(Decl* decl) {
            using K = intercept::Expr::Kind;
            switch (decl->kind()) {
                // All kinds of decl must go here
                case K::TypeDecl: return Kind::TYPE;
                case K::TypeAliasDecl: return Kind::TYPE_ALIAS;
                case K::EnumeratorDecl: return Kind::ENUMERATOR;
                case K::VarDecl: return Kind::VARIABLE;
                case K::FuncDecl: return Kind::FUNCTION;

                // Non-decl kinds
                case K::While:
                case K::For:
                case K::Return:
                case K::IntegerLiteral:
                case K::StringLiteral:
                case K::CompoundLiteral:
                case K::OverloadSet:
                case K::EvaluatedConstant:
                case K::If:
                case K::Block:
                case K::Call:
                case K::IntrinsicCall:
                case K::Cast:
                case K::Unary:
                case K::Binary:
                case K::NameRef:
                case K::MemberAccess:
                case K::Module:
                case K::Type:
                    break;
            }
            LCC_UNREACHABLE();
        }
    };
};
} // namespace intercept
} // namespace lcc

#endif /* LCC_INTERCEPT_MODULE_DESCRIPTION_HH */
