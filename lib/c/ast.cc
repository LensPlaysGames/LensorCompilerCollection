#include <c/ast.hh>

namespace cc = lcc::c;

std::string cc::ToString(TokenKind kind) {
    switch (kind) {
        default: return "<invalid token>";

        // Grouping Delimiters
        case TokenKind::OpenParen: return "(";
        case TokenKind::CloseParen: return ")";
        case TokenKind::OpenBracket: return "[";
        case TokenKind::CloseBracket: return "]";
        case TokenKind::OpenBrace: return "{";
        case TokenKind::CloseBrace: return "}";

        // Other Delimiters
        case TokenKind::Dot: return ".";
        case TokenKind::Comma: return ",";
        case TokenKind::Colon: return ":";
        case TokenKind::SemiColon: return ";";

        // Arithmetic Operators
        case TokenKind::Plus: return "+";
        case TokenKind::Minus: return "-";
        case TokenKind::Star: return "*";
        case TokenKind::Slash: return "/";
        case TokenKind::Percent: return "%";

        // Assignment Operators
        case TokenKind::Equal: return "=";
        case TokenKind::PlusEqual: return "+=";
        case TokenKind::MinusEqual: return "-=";
        case TokenKind::StarEqual: return "*=";
        case TokenKind::SlashEqual: return "/=";
        case TokenKind::PercentEqual: return "%=";
        case TokenKind::AmpersandEqual: return "&=";
        case TokenKind::PipeEqual: return "|=";
        case TokenKind::CaretEqual: return "^=";
        case TokenKind::LessLessEqual: return "<<=";
        case TokenKind::GreaterGreaterEqual: return ">>=";

        // Bitwise Operators
        case TokenKind::Tilde: return "~";
        case TokenKind::Ampersand: return "&";
        case TokenKind::Pipe: return "|";
        case TokenKind::Caret: return "^";
        case TokenKind::LessLess: return "<<";
        case TokenKind::GreaterGreater: return ">>";

        // Boolean Operators
        case TokenKind::Bang: return "!";
        case TokenKind::AmpersandAmpersand: return "&&";
        case TokenKind::PipePipe: return "||";

        // Conditional Evaluation Operators
        case TokenKind::Question: return "?";

        // Equality Operators
        case TokenKind::EqualEqual: return "==";
        case TokenKind::BangEqual: return "!=";

        // Relational Operators
        case TokenKind::Less: return "<";
        case TokenKind::LessEqual: return "<=";
        case TokenKind::Greater: return ">";
        case TokenKind::GreaterEqual: return ">=";

        // Increment/Decrement Operators
        case TokenKind::PlusPlus: return "++";
        case TokenKind::MinusMinus: return "--";

        // Member Selection Operators
        case TokenKind::MinusGreater: return "->";

        // User Tokens
        case TokenKind::Ident: return "<identifier>";
        case TokenKind::LitInt: return "<literal integer>";
        case TokenKind::LitFloat: return "<literal float>";
        case TokenKind::LitChar: return "<literal character>";
        case TokenKind::LitString: return "<literal string>";

        // C89 Keywords
        case TokenKind::Auto: return "auto";
        case TokenKind::Break: return "break";
        case TokenKind::Case: return "case";
        case TokenKind::Char: return "char";
        case TokenKind::Const: return "const";
        case TokenKind::Continue: return "continue";
        case TokenKind::Default: return "default";
        case TokenKind::Do: return "do";
        case TokenKind::Double: return "double";
        case TokenKind::Else: return "else";
        case TokenKind::Enum: return "enum";
        case TokenKind::Extern: return "extern";
        case TokenKind::Float: return "float";
        case TokenKind::For: return "for";
        case TokenKind::Goto: return "goto";
        case TokenKind::If: return "if";
        case TokenKind::Int: return "int";
        case TokenKind::Long: return "long";
        case TokenKind::Register: return "register";
        case TokenKind::Return: return "return";
        case TokenKind::Short: return "short";
        case TokenKind::Signed: return "signed";
        case TokenKind::Sizeof: return "sizeof";
        case TokenKind::Static: return "static";
        case TokenKind::Struct: return "struct";
        case TokenKind::Switch: return "switch";
        case TokenKind::Typedef: return "typedef";
        case TokenKind::Union: return "union";
        case TokenKind::Unsigned: return "unsigned";
        case TokenKind::Void: return "void";
        case TokenKind::Volatile: return "volatile";
        case TokenKind::While: return "while";

        // C99 Keywords
        case TokenKind::Bool_: return "_Bool";
        case TokenKind::Complex_: return "_Complex";
        case TokenKind::Imaginary_: return "_Imaginary";
        case TokenKind::Inline: return "inline";
        case TokenKind::Restrict: return "restrict";

        // C11 Keywords
        case TokenKind::Alignas_: return "_Alignas";
        case TokenKind::Alignof_: return "_Alignof";
        case TokenKind::Atomic_: return "_Atomic";
        case TokenKind::Generic_: return "_Generic";
        case TokenKind::Noreturn_: return "_Noreturn";
        case TokenKind::StaticAssert_: return "_Static_assert";
        case TokenKind::ThreadLocal_: return "_Thread_local";

        // C17 Keywords

        // C23 Keywords
        case TokenKind::Alignas: return "alignas";
        case TokenKind::Alignof: return "alignof";
        case TokenKind::Bool: return "bool";
        case TokenKind::Constexpr: return "constexpr";
        case TokenKind::False: return "false";
        case TokenKind::Nullptr: return "nullptr";
        case TokenKind::StaticAssert: return "static_assert";
        case TokenKind::ThreadLocal: return "thread_local";
        case TokenKind::True: return "true";
        case TokenKind::Typeof: return "typeof";
        case TokenKind::TypeofUnqual: return "typeof_unqual";
        case TokenKind::Decimal128_: return "_Decimal128";
        case TokenKind::Decimal32_: return "_Decimal32";
        case TokenKind::Decimal64_: return "_Decimal64";
    }
}

std::string cc::ToString(OperatorKind kind) {
    switch (kind) {
        default: return "<invalid operator>";
        
        // Unary Operators
        case OperatorKind::PlusIdentity: return "+";
        case OperatorKind::Negate: return "-";
        case OperatorKind::AddressOf: return "&";
        case OperatorKind::Dereference: return "*";
        case OperatorKind::Compl: return "~";

        // Binary Operators

        // Arithmetic Operators
        case OperatorKind::Add: return "+";
        case OperatorKind::Sub: return "-";
        case OperatorKind::Mul: return "*";
        case OperatorKind::Div: return "/";
        case OperatorKind::Mod: return "%";

        // Assignment Operators
        case OperatorKind::Assign: return "=";
        case OperatorKind::AddAssign: return "+=";
        case OperatorKind::SubAssign: return "-=";
        case OperatorKind::MulAssign: return "*=";
        case OperatorKind::DivAssign: return "/=";
        case OperatorKind::ModAssign: return "%=";
        case OperatorKind::AndAssign: return "&=";
        case OperatorKind::OrAssign: return "|=";
        case OperatorKind::XorAssign: return "^=";
        case OperatorKind::LshAssign: return "<<=";
        case OperatorKind::RshAssign: return ">>=";

        // Bitwise Operators
        case OperatorKind::And: return "&";
        case OperatorKind::Or: return "|";
        case OperatorKind::Xor: return "^";
        case OperatorKind::Lsh: return "<<";
        case OperatorKind::Rsh: return ">>";

        // Boolean Operators
        case OperatorKind::LogicalNot: return "!";
        case OperatorKind::LogicalAnd: return "&&";
        case OperatorKind::LogicalOr: return "||";

        // Equality Operators
        case OperatorKind::Equal: return "==";
        case OperatorKind::NotEqual: return "!=";

        // Relational Operators
        case OperatorKind::Less: return "<";
        case OperatorKind::LessEqual: return "<=";
        case OperatorKind::Greater: return ">";
        case OperatorKind::GreaterEqual: return ">=";

        // Increment/Decrement Operators
        case OperatorKind::PlusPlus: return "++";
        case OperatorKind::MinusMinus: return "--";
    }
}

void* cc::BaseNode::operator new(size_t sz, CompilationUnit& cu) {
    auto ptr = ::operator new(sz);
    cu.all_nodes.push_back(static_cast<BaseNode*>(ptr));
    return ptr;
}
