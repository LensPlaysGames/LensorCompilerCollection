#ifndef LCC_GLINT_MODULE_DESCRIPTION_HH
#define LCC_GLINT_MODULE_DESCRIPTION_HH

#include <glint/ast.hh>

namespace lcc {
namespace glint {
/// For serialisation purposes.
/// If you know how ELF works, you'll find this familiar and pretty
/// easy-going. If not, hopefully the comments help you out along the way.
///
/// OVERALL STRUCTURE of BINARY METADATA BLOB version 1:
///
/// Beginning of file       type_table_offset expr_table_offset name_offset
/// V                       V                 V                 V
/// Header { Declarations } { Types }         { Expressions }   [ Module Name ]
///
/// While the above /is/ how LCC lays things out, the format allows for any
/// of those things to be in any order in the actual file, simply by
/// specifying the correct offsets for each section.
///
/// A declaration is encoded as a DeclarationHeader + N amount of bytes
/// determined by the values in the declaration header.
///
/// TODO: Include more information that is not needed for Glint, but would
/// probably be very useful for both Glint and surrounding tools.
/// Including but not limited to:
/// |-- Path to Module Source (could be used by non-existent Glint build system
/// |   to automagically build resolved import dependencies)
/// `-- Compiler and Version that generated this metadata/module
struct ModuleDescription {
    // A strong typedef would be better, but C++ is as C++ does.
    using TypeIndex = u16;
    using ExprIndex = u16;
    static constexpr TypeIndex bad_type_index = TypeIndex(-1);
    static constexpr ExprIndex bad_expr_index = ExprIndex(-1);

    // Default/expected values.
    static constexpr u8 default_version = 1;
    static constexpr u8 magic_byte0 = 'G';
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
        /// beginning of the expression table. Value undefined iff expr_count is zero.
        u32 expr_table_offset;

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

        /// The amount of expressions encoded in this binary metadata blob.
        /// Determines maximum exclusive allowed value of declaration expr_index
        /// field. Once the reader deserialised this many expressions, the reader
        /// should stop reading types.
        u16 expr_count;

        /// If a module has no top level expressions, this may be set to false.
        /// When false, consumers of the module this description describes are not
        /// required to call the initialisation function for this module.
        bool requires_initialisation{true};
    };

    struct DeclarationHeader {
        enum struct Kind : u16 {
            INVALID,
            TYPE,
            TYPE_ALIAS,
            ENUMERATOR,
            VARIABLE,
            FUNCTION,
            TEMPLATE, // named templates may be exported
        };

        /// One of DeclarationHeader::Kind.
        // Untyped to avoid UB when deserialising.
        u16 kind{+Kind::INVALID};

        /// The type associated with the declaration. Every declaration must have a
        /// valid type index.
        TypeIndex type_index{u16(-1)};

        /// The expression associated with the declaration.
        /// Used by TEMPLATE declarations to store body expression.
        ExprIndex expr_index{u16(-1)};

        static constexpr Kind get_kind(Decl* decl) {
            using K = glint::Expr::Kind;
            switch (decl->kind()) {
                // All kinds of decl must go here
                case K::TypeDecl: return Kind::TYPE;
                case K::TypeAliasDecl: return Kind::TYPE_ALIAS;
                case K::EnumeratorDecl: return Kind::ENUMERATOR;
                case K::VarDecl: {
                    if (as<VarDecl>(decl)->init() and is<TemplateExpr>(as<VarDecl>(decl)->init()))
                        return Kind::TEMPLATE;

                    return Kind::VARIABLE;
                }
                case K::FuncDecl: return Kind::FUNCTION;

                case K::TemplatedFuncDecl:
                    LCC_TODO("Add templated function declaration kind");

                // Non-decl kinds
                case K::Apply:
                case K::Group:
                case K::Match:
                case K::Switch:
                case K::While:
                case K::For:
                case K::Return:
                case K::IntegerLiteral:
                case K::FractionalLiteral:
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
                case K::Sizeof:
                case K::Alignof:
                case K::Template:
                    break;
            }
            LCC_UNREACHABLE();
        }
    };
};
} // namespace glint
} // namespace lcc

#endif /* LCC_GLINT_MODULE_DESCRIPTION_HH */
