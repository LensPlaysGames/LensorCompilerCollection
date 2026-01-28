#include <lcc/core.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/result.hh>

#include <array>
#include <cstdlib>
#include <deque>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

/// Namespace required for friend declarations in ir.hh.
namespace lcc::parser {

enum struct ErrorId : unsigned {
    INVALID,

    InvalidLiteral,

    Expected,

    Miscellaneous,

    COUNT
};
std::array<std::pair<ErrorId, const char*>, 5> error_id_strings{
    std::pair{ErrorId::INVALID, "ICE: INVALID ERROR ID"},

    {ErrorId::InvalidLiteral, "invalid-literal"},

    {ErrorId::Expected, "expected"},

    {ErrorId::Miscellaneous, "miscellaneous"},

    {ErrorId::COUNT, "ICE: INVALID ERROR ID"}
};
static_assert(
    error_id_strings.size() == +ErrorId::COUNT + 1,
    "Exhaustive handling of ErrorId"
);

enum struct TokenKind {
    Invalid,
    Eof,

    Keyword,
    Temporary,
    IntegerType,
    Global,
    Integer,
    Indent,

    Colon,
    Comma,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Equals,
    Newline,
    Arrow,
};

constexpr auto StringifyEnum(TokenKind t) -> std::string_view {
    switch (t) {
        case TokenKind::Invalid: break;
        case TokenKind::Eof: return "eof";
        case TokenKind::Keyword: return "keyword";
        case TokenKind::Temporary: return "temporary";
        case TokenKind::IntegerType: return "integer type";
        case TokenKind::Global: return "global";
        case TokenKind::Integer: return "integer";
        case TokenKind::Indent: return "indentation";
        case TokenKind::Newline: return "newline";
        case TokenKind::Colon: return ":";
        case TokenKind::Comma: return ",";
        case TokenKind::LParen: return "(";
        case TokenKind::RParen: return ")";
        case TokenKind::LBrack: return "[";
        case TokenKind::RBrack: return "]";
        case TokenKind::LBrace: return "{";
        case TokenKind::RBrace: return "}";
        case TokenKind::Equals: return "=";
        case TokenKind::Arrow: return "->";
    }

    return "<invalid>";
}

std::unordered_map<std::string, IntrinsicKind> intrinsic_kinds{
    {"@memcpy", IntrinsicKind::MemCopy}
    // TODO: memset
};

class Parser : syntax::Lexer<syntax::Token<TokenKind>> {
    using Tk = TokenKind;
    using Token = syntax::Token<Tk>;

    using syntax::Lexer<Token>::Error;
    using syntax::Lexer<Token>::Warning;
    using syntax::Lexer<Token>::Note;

    /// Because of forward references, values in the IR may
    /// not be fully constructed when they are first created.
    ///
    /// Since names of temporaries are generally short, copying
    /// these around should not be a problem because of SSO.
    struct Temporary {
        std::string data;
    };
    struct Global {
        std::string data;
    };
    using IRValue = std::variant<Value*, Temporary, Global>;

    std::deque<Token> lookahead_tokens{};
    bool looking_ahead = false;
    bool last_token_was_newline = false;
    StringMap<Value*> temporaries{};
    StringMap<Value*> globals{};
    StringMap<Type*> named_types{};

    /// Unresolved values.
    StringMap<std::vector<std::pair<Inst*, Block**>>> block_fixups{};
    StringMap<std::vector<std::pair<Inst*, Value**>>> temporary_fixups{};
    StringMap<std::vector<std::pair<Inst*, Value**>>> global_fixups{};

public:
    std::unique_ptr<Module> mod{};

    Parser(Context* ctx, File* file)
        : syntax::Lexer<Token>(ctx, file) {
        mod = std::make_unique<Module>(context);
        NextToken();
    }

    Parser(Context* ctx, std::string_view source)
        : syntax::Lexer<Token>(ctx, source) {
        mod = std::make_unique<Module>(context);
        NextToken();
    }

    auto ParseModule() -> Result<void>;

private:
    /// Check if we’re at one of a set of tokens.
    [[nodiscard]]
    static auto Is(Token* tk, auto... tks) -> bool {
        return ((tk->kind == tks) or ...);
    }

    [[nodiscard]]
    auto At(auto... tks) -> bool {
        return Is(&tok, tks...);
    }

    [[nodiscard]]
    auto Kw(std::string_view text) -> bool {
        return At(Tk::Keyword) and tok.text == text;
    }

    void AddTemporary(std::string name, Value* val) {
        if (temporaries.contains(name))
            Error(ErrorId::Miscellaneous, "Duplicate temporary '{}'", name);
        temporaries[std::move(name)] = val;
    }

    /// Like At(), but consume the token if it matches.
    auto Consume(auto... tks) -> bool {
        if (At(tks...)) {
            NextToken();
            return true;
        }
        return false;
    }

    auto ConsumeOrError(Tk t) -> Result<void> {
        if (not Consume(t)) {
            return Error(
                ErrorId::Expected,
                "Expected '{}'",
                t
            );
        }
        return {};
    }

    auto LookAhead(usz n) -> Token*;
    void NextIdentifier();
    void NextNumber();
    void NextToken();

    template <typename Instruction>
    auto ParseBinary(std::string tmp) -> Result<Inst*>;

    template <typename Instruction>
    auto ParseCast(std::string tmp) -> Result<Inst*>;

    template <typename Instruction>
    auto ParseGEP(std::string tmp) -> Result<Inst*>;

    auto ParseBlock() -> Result<Block*>;
    auto ParseCall(bool tail) -> Result<CallInst*>;
    auto ParseIntrinsic() -> Result<IntrinsicInst*>;
    auto ParseCallConv() -> CallConv;
    auto ParseFunction() -> Result<void>;
    auto ParseLiteral(std::string_view lit) -> Result<void>;
    auto ParseInstruction() -> Result<Inst*>;
    auto ParseType() -> Result<Type*>;
    auto ParseUntypedValue(Type* assumed_type) -> Result<IRValue>;
    auto ParseValue() -> Result<std::pair<Type*, IRValue>>;

    /// Get a `Value*` for a temporary, or mark it to be resolved later.
    void SetBlock(Inst* parent, Block*& val, IRValue v);
    void SetValue(Inst* parent, Value*& val, IRValue v);

    template <typename... Args>
    auto Error(enum ErrorId id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Error(
            context,
            tok.location,
            error_id_strings.at(+id).second,
            fmt,
            std::forward<Args>(args)...
        );
    }

    template <typename... Args>
    auto Warning(enum ErrorId id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Warning(
            context,
            tok.location,
            error_id_strings.at(+id).second,
            fmt,
            std::forward<Args>(args)...
        );
    }

    template <typename... Args>
    auto Note(enum ErrorId id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Note(
            context,
            tok.location,
            error_id_strings.at(+id).second,
            fmt,
            std::forward<Args>(args)...
        );
    }

    static auto IsIdentStart(u32 c) -> bool {
        return IsAlpha(c) or c == '_' or c == '.';
    }
    static auto IsIdentContinue(u32 c) -> bool {
        return IsIdentStart(c) or IsDecimalDigit(c);
    }
};

} // namespace lcc::parser

void lcc::parser::Parser::NextIdentifier() {
    tok.kind = TokenKind::Keyword;
    while (IsIdentContinue(lastc)) {
        if (lastc > 0xff) LCC_TODO("Handle unicode codepoint in identifier");
        tok.text += char(lastc);
        NextChar();
    }
}

void lcc::parser::Parser::NextNumber() {
    /// Helper that actually parses the number.
    const auto ParseNumber =
        [&](std::string_view name, auto&& IsValidDigit, int base) {
            /// Yeet prefix.
            if (base != 10) NextChar();

            /// Lex digits.
            while (IsValidDigit(lastc)) {
                if (lastc > 0xff) LCC_TODO("Handle unicode codepoint in number literal");
                tok.text += char(lastc);
                NextChar();
            }

            /// We need at least one digit.
            tok.location.len = (u16) (CurrentOffset() - tok.location.pos);
            if (tok.text.empty()) {
                Error(
                    ErrorId::InvalidLiteral,
                    "Expected at least one {} digit",
                    name
                );
            }

            /// Actually parse the number.
            const char* cstr = tok.text.c_str();

            /// Convert the number.
            char* end;
            errno = 0;
            tok.integer_value = (u64) std::strtoull(cstr, &end, base);
            if (errno == ERANGE) {
                Error(
                    ErrorId::InvalidLiteral,
                    "Bit width of integer is too large."
                );
            }
            // If there is "stuff" past the end of the number (like "64abc")
            if (end != cstr + tok.text.size()) {
                Error(
                    ErrorId::InvalidLiteral,
                    "Invalid integer literal"
                );
            }
        };

    /// Record the start of the number.
    tok.text.clear();

    tok.integer_value = 0;
    tok.kind = TokenKind::Integer;

    /// At least one leading zero.
    if (lastc == '0') {
        /// Discard the zero.
        NextChar();

        /// Another zero is an error.
        if (IsDecimalDigit(lastc)) {
            Error(
                ErrorId::InvalidLiteral,
                "Leading zeroes are not allowed in literals unless it leads to a base specifier."
            );
        } else if (lastc == 'b' or lastc == 'B')
            ParseNumber("binary", IsBinaryDigit, 2);
        else if (lastc == 'o' or lastc == 'O')
            ParseNumber("octal", IsOctalDigit, 8);
        else if (lastc == 'x' or lastc == 'X')
            ParseNumber("hexadecimal", IsHexDigit, 16);

        /// If the next character is a space or delimiter, then this is a literal 0.
        if (IsSpace(lastc)) return;

        /// Anything else is an error.
        Error(ErrorId::InvalidLiteral, "Invalid integer literal");
    }

    /// Any other digit means we have a decimal number.
    ParseNumber("decimal", IsDecimalDigit, 10);
}

void lcc::parser::Parser::NextToken() {
    defer { last_token_was_newline = At(Tk::Newline); };
    tok.kind = TokenKind::Invalid;
    tok.text.clear();

    if (not looking_ahead and not lookahead_tokens.empty()) {
        tok = std::move(lookahead_tokens.front());
        lookahead_tokens.pop_front();
        return;
    }

    if (lastc == 0) {
        tok.kind = TokenKind::Eof;
        return;
    }

    /// Parse indentation.
    if (last_token_was_newline and IsSpace(lastc) and lastc != '\n') {
        tok.kind = TokenKind::Indent;
        tok.integer_value = 0;
        do {
            tok.integer_value++;
            NextChar();
        } while (IsSpace(lastc) and lastc != '\n');
        return;
    }

    /// Skip whitespace.
    while (IsSpace(lastc) and lastc != '\n') NextChar();
    tok.location.pos = CurrentOffset();

    /// Parse token.
    switch (lastc) {
        case 0: return;
        case '(':
            tok.kind = TokenKind::LParen;
            NextChar();
            break;

        case ')':
            tok.kind = TokenKind::RParen;
            NextChar();
            break;

        case '[':
            tok.kind = TokenKind::LBrack;
            NextChar();
            break;

        case ']':
            tok.kind = TokenKind::RBrack;
            NextChar();
            break;

        case '{':
            tok.kind = TokenKind::LBrace;
            NextChar();
            break;

        case '}':
            tok.kind = TokenKind::RBrace;
            NextChar();
            break;

        case ':':
            tok.kind = TokenKind::Colon;
            NextChar();
            break;

        case ',':
            tok.kind = TokenKind::Comma;
            NextChar();
            break;

        case '=':
            tok.kind = TokenKind::Equals;
            NextChar();
            break;

        case '@':
            NextChar();
            // tok.text = '@';
            NextIdentifier();
            tok.kind = TokenKind::Global;
            break;

        case '%':
            NextChar();
            tok.text = '%';
            NextIdentifier();
            tok.kind = TokenKind::Temporary;
            break;

        case '\n':
            tok.kind = TokenKind::Newline;
            NextChar();
            break;

        // LINE COMMENT
        case ';':
            while (lastc != 0 and lastc != '\n') NextChar();
            if (lastc != 0) NextChar();
            return NextToken();

        case '-':
            NextChar();
            if (lastc == '>') {
                NextChar();
                tok.kind = Tk::Arrow;
                break;
            }

            NextNumber();
            tok.integer_value = -tok.integer_value;
            break;

        default:
            tok.text.clear();
            if (IsIdentStart(lastc)) {
                NextIdentifier();

                /// Try and parse a number after `i`.
                if (
                    tok.text.size() > 1
                    and tok.text[0] == 'i'
                    and IsDecimalDigit(u32(tok.text[1]))
                ) {
                    const char* cstr = tok.text.c_str();

                    /// Convert the number.
                    char* end;
                    errno = 0;
                    tok.integer_value = (u64) std::strtoull(cstr + 1, &end, 10);
                    if (errno == ERANGE) {
                        Error(
                            ErrorId::InvalidLiteral,
                            "Bit width of integer is too large."
                        );
                    }

                    /// If the identifier is something like `s64iam`, it's simply an identifier.
                    if (end != cstr + tok.text.size()) return;
                    tok.kind = TokenKind::IntegerType;
                }
            } else if (IsDecimalDigit(lastc)) {
                NextNumber();
            } else {
                Error(
                    ErrorId::Expected,
                    "Unexpected character '{}'",
                    lastc
                );
            }
            break;
    }

    tok.location.len = (u16) (CurrentOffset() - tok.location.pos);
}

auto lcc::parser::Parser::LookAhead(usz n) -> Token* {
    if (n == 0) return &tok;

    /// If we already have enough tokens, just return the nth token.
    const auto idx = n - 1;
    if (idx < lookahead_tokens.size()) return &lookahead_tokens[idx];

    /// Otherwise, lex enough tokens.
    tempset looking_ahead = true;
    auto current = std::move(tok);
    for (usz i = lookahead_tokens.size(); i < n; i++) {
        tok = {};
        NextToken();
        lookahead_tokens.push_back(std::move(tok));
    }
    tok = std::move(current);

    /// Return the nth token.
    return &lookahead_tokens[idx];
}

template <typename Instruction>
auto lcc::parser::Parser::ParseBinary(std::string tmp) -> Result<Inst*> {
    auto loc = tok.location;
    NextToken();
    auto lhs = ParseValue();
    auto comma = ConsumeOrError(Tk::Comma);
    if (IsError(lhs, comma))
        return Diag();
    auto rhs = ParseUntypedValue(lhs->first);

    Instruction* inst;
    if constexpr (requires { Instruction(lhs->first); }) {
        inst = new (*mod) Instruction(lhs->first, loc);
    } else {
        inst = new (*mod) Instruction(loc);
    }

    SetValue(inst, inst->left, lhs->second);
    SetValue(inst, inst->right, *rhs);
    AddTemporary(std::move(tmp), inst);
    return inst;
}

auto lcc::parser::Parser::ParseBlock() -> Result<Block*> {
    /// Block name and colon.
    if (not At(Tk::Keyword))
        return Error(ErrorId::Expected, "Expected block name");
    auto* b = new (*mod) Block(tok.text);
    AddTemporary("%" + b->name(), b);
    NextToken();
    if (not Consume(Tk::Colon))
        return Error(ErrorId::Expected, "Expected ':'");
    if (not Consume(Tk::Newline))
        return Error(ErrorId::Expected, "Expected line break");

    /// Parse instructions.
    while (At(Tk::Indent) and LookAhead(2)->kind != Tk::Colon) {
        NextToken();
        auto i = ParseInstruction();
        if (i.is_diag()) continue;
        b->insert(*i, true);
        if (not At(Tk::Eof)) (void) ConsumeOrError(Tk::Newline);
    }

    return b;
}

auto lcc::parser::Parser::ParseCall(bool tail) -> Result<CallInst*> {
    auto loc = tok.location;
    NextToken();
    auto cc = ParseCallConv();

    Type* ret = Type::VoidTy;
    std::vector<Type*> arg_types;
    std::vector<IRValue> args;
    auto callee = ParseUntypedValue(ret);
    if (not callee) return callee.diag();
    if (not Consume(Tk::LParen))
        return Error(ErrorId::Expected, "Expected '('");
    while (not At(Tk::RParen)) {
        auto arg = ParseValue();
        if (not arg) return arg.diag();
        arg_types.push_back(arg->first);
        args.push_back(arg->second);
        if (not Consume(Tk::Comma)) break;
    }

    if (not Consume(Tk::RParen))
        return Error(ErrorId::Expected, "Expected ')'");

    bool is_variadic = false;
    if (At(Tk::Keyword) and tok.text == "variadic") {
        NextToken();
        is_variadic = true;
    }

    if (Consume(Tk::Arrow)) {
        auto ty = ParseType();
        if (not ty) return ty.diag();
        ret = *ty;
    }

    auto* call = new (*mod) CallInst(
        FunctionType::Get(mod->context(), ret, std::move(arg_types), is_variadic),
        loc
    );

    SetValue(call, call->callee_value, *callee);
    call->arguments.resize(args.size());
    for (const auto& [i, arg] : vws::enumerate(args))
        SetValue(call, call->arguments[usz(i)], arg);

    if (tail) call->set_tail_call();
    call->cc = cc;
    return call;
}

auto lcc::parser::Parser::ParseIntrinsic() -> Result<IntrinsicInst*> {
    std::vector<Type*> arg_types;
    std::vector<IRValue> args;

    auto intrinsic_name = tok.text;
    if (not At(Tk::Global))
        return Error(ErrorId::Expected, "Expected intrinsic name");

    auto name = tok.text;
    auto loc = tok.location;

    auto intrinsic_it = intrinsic_kinds.find(name);
    if (intrinsic_it == intrinsic_kinds.end())
        return Error(ErrorId::Miscellaneous, "Unrecognized intrinsic name");

    NextToken();

    if (not Consume(Tk::LParen))
        return Error(ErrorId::Expected, "Expected '('");
    while (not At(Tk::RParen)) {
        auto arg = ParseValue();
        if (not arg) return arg.diag();
        arg_types.push_back(arg->first);
        args.push_back(arg->second);
        if (not Consume(Tk::Comma)) break;
    }

    if (not Consume(Tk::RParen))
        return Error(ErrorId::Expected, "Expected ')'");

    auto* intrinsic = new (*mod) IntrinsicInst(intrinsic_it->second, {}, loc);
    intrinsic->operand_list.resize(args.size());
    for (const auto& [i, arg] : vws::enumerate(args))
        SetValue(intrinsic, intrinsic->operand_list[usz(i)], arg);

    return intrinsic;
}

auto lcc::parser::Parser::ParseCallConv() -> CallConv {
    CallConv cc = CallConv::C;
    if (At(Tk::Keyword)) {
        auto SetCC = [&](CallConv c) {
            cc = c;
            NextToken();
        };

        if (tok.text == "ccc") SetCC(CallConv::C);
        else if (tok.text == "glintcc") SetCC(CallConv::Glint);
    }
    return cc;
}

template <typename Instruction>
auto lcc::parser::Parser::ParseCast(std::string tmp) -> Result<Inst*> {
    NextToken();
    auto val = ParseValue();
    auto to = ParseLiteral("to");
    auto ty = ParseType();
    if (IsError(val, to, ty))
        return Diag();
    auto inst = new (*mod) Instruction(*ty, tok.location);
    SetValue(inst, inst->op, val->second);
    AddTemporary(std::move(tmp), inst);
    return inst;
}

auto lcc::parser::Parser::ParseFunction() -> Result<void> {
    /// Parse name and colon.
    if (not At(Tk::Keyword))
        return Error(ErrorId::Expected, "Expected function name");
    auto name = tok.text;
    auto loc = tok.location;
    // Eat name
    NextToken();

    // Eat lparen to open linkage string
    if (not Consume(Tk::LParen))
        return Error(ErrorId::Expected, "Expected '('");

    if (not At(Tk::Keyword)) {
        return Error(
            ErrorId::Expected,
            "Expected linkage (local, imported, exported, etc.)"
        );
    }

    auto linkage = Linkage::Exported;
    if (tok.text == "local") linkage = Linkage::LocalVar;
    else if (tok.text == "internal") linkage = Linkage::Internal;
    else if (tok.text == "used") linkage = Linkage::Used;
    else if (tok.text == "exported") linkage = Linkage::Exported;
    else if (tok.text == "imported") linkage = Linkage::Imported;
    else if (tok.text == "reexported") linkage = Linkage::Reexported;
    else {
        return Error(
            ErrorId::Expected,
            "Expected valid linkage (imported, exported, etc.), got '{}'",
            tok.text
        );
    }

    // Eat linkage
    NextToken();

    if (not Consume(Tk::RParen))
        return Error(ErrorId::Expected, "Expected ')'");

    if (not Consume(Tk::Colon))
        return Error(ErrorId::Expected, "Expected ':'");

    CallConv cc = ParseCallConv();

    /// Parse signature.
    auto ret = ParseType();
    if (not ret) return ret.diag();

    if (not Consume(Tk::LParen))
        return Error(ErrorId::Expected, "Expected '('");

    std::vector<Type*> args{};
    std::vector<std::string> names{};
    while (not At(Tk::RParen, Tk::Eof, Tk::Newline, Tk::Indent)) {
        auto ty = ParseType();
        if (not ty) return ty.diag();
        args.push_back(*ty);
        if (not At(Tk::Temporary)) {
            names.emplace_back();
        } else {
            names.push_back(tok.text);
            NextToken();
        }
        if (not Consume(Tk::Comma)) break;
    }
    if (not Consume(Tk::RParen))
        return Error(ErrorId::Expected, "Expected ')'");

    bool is_variadic = false;
    if (At(Tk::Keyword) and tok.text == "variadic") {
        NextToken();
        is_variadic = true;
    }

    /// Create the function.
    auto* f = new (*mod) Function(
        mod.get(),
        std::move(name),
        FunctionType::Get(mod->context(), *ret, std::move(args), is_variadic),
        linkage,
        cc,
        loc
    );

    /// Check for duplicates.
    for (auto n : f->names()) {
        if (globals.contains(n.name))
            Error(ErrorId::Miscellaneous, "Duplicate global symbol '{}'", n.name);
        globals[n.name] = f;
    }

    /// Colon means we have a body.
    if (not Consume(Tk::Colon)) return {};
    if (not Consume(Tk::Newline))
        return Error(ErrorId::Expected, "Expected line break");

    /// Register mappings for function arguments.
    temporaries.clear();
    for (const auto& [i, arg] : vws::enumerate(names)) {
        if (arg.empty()) continue;
        AddTemporary(std::move(arg), f->param(usz(i)));
    }

    /// Clear the last function’s fixup lists.
    temporary_fixups.clear();
    block_fixups.clear();

    /// Parse blocks.
    while (Consume(Tk::Indent)) {
        auto b = ParseBlock();
        if (not b) return b.diag();
        f->append_block(*b);
    }

    /// Fix up temporaries.
    for (auto& [tmp, fixups] : temporary_fixups) {
        auto it = temporaries.find(tmp);
        if (it == temporaries.end())
            return Error(ErrorId::Miscellaneous, "Unknown value '{}'", tmp);
        for (auto elem : fixups) {
            *elem.second = it->second;
            Inst::AddUse(it->second, elem.first);
        }
    }

    /// Fix up blocks.
    for (auto [tmp, fixups] : block_fixups) {
        auto it = temporaries.find(tmp);
        if (it == temporaries.end())
            return Error(ErrorId::Miscellaneous, "Unknown block '{}'", tmp);
        auto* b = cast<Block>(it->second);
        if (not b)
            return Error(ErrorId::Miscellaneous, "'{}' is not a block", tmp);
        for (auto elem : fixups) {
            *elem.second = b;
            Inst::AddUse(b, elem.first);
        }
    }

    return {};
}

template <typename Instruction>
auto lcc::parser::Parser::ParseGEP(std::string tmp) -> Result<Inst*> {
    auto loc = tok.location;
    NextToken();
    auto ty = ParseType();
    auto lit = ParseLiteral("from");
    auto ptr = ParseUntypedValue(Type::PtrTy);
    auto at = ParseLiteral("at");
    auto idx = ParseValue();
    if (IsError(ty, lit, ptr, at, idx))
        return Diag();
    auto gep = new (*mod) Instruction(*ty, loc);
    SetValue(gep, gep->pointer, *ptr);
    SetValue(gep, gep->index, idx->second);
    AddTemporary(std::move(tmp), gep);
    return gep;
}

auto lcc::parser::Parser::ParseLiteral(std::string_view lit) -> Result<void> {
    if (not At(Tk::Keyword) or tok.text != lit)
        return Error(ErrorId::Expected, "Expected '{}'", lit);
    NextToken();
    return {};
}

auto lcc::parser::Parser::ParseInstruction() -> Result<Inst*> {
    /// Instructions that don’t yield a value.
    if (At(Tk::Keyword)) {
        auto loc = tok.location;
        if (tok.text == "tail" or tok.text == "call") {
            const bool tail = tok.text == "tail";
            if (tail) NextToken();
            return ParseCall(tail);
        }

        if (tok.text == "intrinsic") {
            NextToken();
            return ParseIntrinsic();
        }

        if (tok.text == "store") {
            NextToken();
            auto val = ParseValue();
            auto into = ParseLiteral("into");
            auto ptr = ParseUntypedValue(val->first);

            if (IsError(val, into, ptr))
                return Diag();
            auto store = new (*mod) StoreInst(loc);
            SetValue(store, store->value, val->second);
            SetValue(store, store->pointer, *ptr);
            return store;
        }

        if (tok.text == "branch") {
            NextToken();

            /// `branch to` is an unconditional branch.
            if (Kw("to")) {
                NextToken();
                auto res = ParseUntypedValue(nullptr);
                if (res.is_diag()) return res.diag();
                auto br = new (*mod) BranchInst(loc);
                SetBlock(br, br->target_block, *res);
                return br;
            }

            /// `branch on` is a conditional branch.
            if (Kw("on")) {
                NextToken();
                auto cond = ParseUntypedValue(Type::I1Ty);
                auto lit_to = ParseLiteral("to");
                auto to = ParseUntypedValue(nullptr);
                auto lit_else = ParseLiteral("else");
                auto els = ParseUntypedValue(nullptr);
                if (IsError(cond, lit_to, to, lit_else, els))
                    return Diag();
                auto br = new (*mod) CondBranchInst(loc);
                SetValue(br, br->condition, *cond);
                SetBlock(br, br->then, *to);
                SetBlock(br, br->otherwise, *els);
                return br;
            }

            return Error(
                ErrorId::Expected,
                "Expected 'to' or 'on' after 'branch'"
            );
        }

        if (tok.text == "return") {
            NextToken();
            auto ret = new (*mod) ReturnInst(nullptr, loc);
            if (not At(Tk::Newline, Tk::Eof)) {
                auto res = ParseValue();
                if (res.is_diag()) return res.diag();
                SetValue(ret, ret->value, res->second);
            }
            return ret;
        }

        if (tok.text == "unreachable") {
            NextToken();
            return new (*mod) UnreachableInst(loc);
        }

        return Error(ErrorId::Miscellaneous, "Unknown instruction '{}'", tok.text);
    }

    /// Instructions that yield a value.
    if (not At(Tk::Temporary))
        return Error(ErrorId::Expected, "Expected instruction");
    auto loc = tok.location;
    auto tmp = tok.text;
    NextToken();
    (void) ConsumeOrError(Tk::Equals);
    if (not At(Tk::Keyword))
        return Error(ErrorId::Expected, "Expected instruction");

    if (tok.text == "alloca") {
        NextToken();
        auto res = ParseType();
        if (res.is_diag()) return res.diag();
        auto alloca = new (*mod) AllocaInst(*res, loc);
        AddTemporary(std::move(tmp), alloca);
        return alloca;
    }

    if (tok.text == "copy") {
        NextToken();
        auto arg = ParseValue();
        if (arg.is_diag()) return arg.diag();
        auto copy = new (*mod) CopyInst(arg->first, loc);
        SetValue(copy, copy->op, arg->second);
        AddTemporary(std::move(tmp), copy);
        return copy;
    }

    if (tok.text == "call") {
        auto call = ParseCall(false);
        if (call.is_diag()) return call.diag();
        AddTemporary(std::move(tmp), *call);
        return call;
    }

    if (tok.text == "gep") return ParseGEP<GEPInst>(std::move(tmp));
    if (tok.text == "gmp") return ParseGEP<GetMemberPtrInst>(std::move(tmp));

    if (tok.text == "load") {
        NextToken();
        auto ty = ParseType();
        auto from = ParseLiteral("from");
        auto ptr = ParseUntypedValue(Type::PtrTy);
        if (IsError(ty, from, ptr))
            return Diag();
        auto load = new (*mod) LoadInst(*ty, loc);
        SetValue(load, load->pointer, *ptr);
        AddTemporary(std::move(tmp), load);
        return load;
    }

    if (tok.text == "phi") {
        NextToken();
        auto type = ParseType();
        auto com = ConsumeOrError(Tk::Comma);
        if (IsError(type, com))
            return Diag();

        std::vector<IRValue> blocks;
        std::vector<IRValue> values;
        do {
            auto l = ConsumeOrError(Tk::LBrack);
            auto block = ParseUntypedValue(nullptr);
            auto co = ConsumeOrError(Tk::Colon);
            auto value = ParseUntypedValue(*type);
            auto r = ConsumeOrError(Tk::RBrack);
            if (IsError(l, block, co, value, r))
                return Diag();
            blocks.push_back(*block);
            values.push_back(*value);
        } while (Consume(Tk::Comma));

        auto phi = new (*mod) PhiInst(*type, loc);
        phi->incoming.resize(blocks.size());
        for (auto&& [i, inc] : vws::enumerate(phi->incoming)) {
            SetBlock(phi, inc.block, blocks[usz(i)]);
            SetValue(phi, inc.value, values[usz(i)]);
        }

        AddTemporary(std::move(tmp), phi);
        return phi;
    }

    if (tok.text == "neg") {
        NextToken();
        auto val = ParseValue();
        if (val.is_diag()) return val.diag();
        auto neg = new (*mod) NegInst(val->first, loc);
        SetValue(neg, neg->op, val->second);
        AddTemporary(std::move(tmp), neg);
        return neg;
    }

    if (tok.text == "compl") {
        NextToken();
        auto val = ParseValue();
        if (val.is_diag()) return val.diag();
        auto c = new (*mod) ComplInst(val->first, loc);
        SetValue(c, c->op, val->second);
        AddTemporary(std::move(tmp), c);
        return c;
    }

    if (tok.text == "add") return ParseBinary<AddInst>(std::move(tmp));
    if (tok.text == "sub") return ParseBinary<SubInst>(std::move(tmp));
    if (tok.text == "mul") return ParseBinary<MulInst>(std::move(tmp));
    if (tok.text == "sdiv") return ParseBinary<SDivInst>(std::move(tmp));
    if (tok.text == "udiv") return ParseBinary<UDivInst>(std::move(tmp));
    if (tok.text == "srem") return ParseBinary<SRemInst>(std::move(tmp));
    if (tok.text == "urem") return ParseBinary<URemInst>(std::move(tmp));
    if (tok.text == "and") return ParseBinary<AndInst>(std::move(tmp));
    if (tok.text == "or") return ParseBinary<OrInst>(std::move(tmp));
    if (tok.text == "xor") return ParseBinary<XorInst>(std::move(tmp));
    if (tok.text == "shl") return ParseBinary<ShlInst>(std::move(tmp));
    if (tok.text == "shr") return ParseBinary<ShrInst>(std::move(tmp));
    if (tok.text == "sar") return ParseBinary<SarInst>(std::move(tmp));
    if (tok.text == "eq") return ParseBinary<EqInst>(std::move(tmp));
    if (tok.text == "ne") return ParseBinary<NeInst>(std::move(tmp));
    if (tok.text == "slt") return ParseBinary<SLtInst>(std::move(tmp));
    if (tok.text == "sle") return ParseBinary<SLeInst>(std::move(tmp));
    if (tok.text == "sgt") return ParseBinary<SGtInst>(std::move(tmp));
    if (tok.text == "sge") return ParseBinary<SGeInst>(std::move(tmp));
    if (tok.text == "ult") return ParseBinary<ULtInst>(std::move(tmp));
    if (tok.text == "ule") return ParseBinary<ULeInst>(std::move(tmp));
    if (tok.text == "ugt") return ParseBinary<UGtInst>(std::move(tmp));
    if (tok.text == "uge") return ParseBinary<UGeInst>(std::move(tmp));
    if (tok.text == "bitcast") return ParseCast<BitcastInst>(std::move(tmp));
    if (tok.text == "zext") return ParseCast<ZExtInst>(std::move(tmp));
    if (tok.text == "sext") return ParseCast<SExtInst>(std::move(tmp));
    if (tok.text == "trunc") return ParseCast<TruncInst>(std::move(tmp));

    return Error(
        ErrorId::Miscellaneous,
        "Unknown instruction '{}'",
        tok.text
    );
}

auto lcc::parser::Parser::ParseModule() -> Result<void> {
    while (not At(Tk::Eof)) {
        if (Consume(Tk::Newline)) continue;

        if (Kw("struct")) {
            auto t = ParseType();
            if (not t) return t.diag();
            // Ensure named type exists
            auto found = rgs::find_if(named_types, [&](const auto& n) {
                return n.second == *t;
            });
            LCC_ASSERT(
                found != named_types.end(),
                "Parsed lonely type, but it is not a named type."
            );
            continue;
        }

        if (auto err = ParseFunction(); err.is_diag())
            return err.diag();
    }

    /// Fix up references to globals.
    for (auto&& [name, fixups] : global_fixups) {
        auto it = globals.find(name);
        if (it == globals.end())
            return Error(ErrorId::Miscellaneous, "Unknown global '{}'", name);
        for (auto elem : fixups) {
            *elem.second = it->second;
            Inst::AddUse(it->second, elem.first);
        }
    }

    return {};
}

auto lcc::parser::Parser::ParseType() -> Result<Type*> {
    Type* base{};
    std::string name{};
    if (Kw("ptr"))
        base = Type::PtrTy;
    else if (Kw("void"))
        base = Type::VoidTy;
    else if (At(Tk::IntegerType))
        base = IntegerType::Get(mod->context(), (usz) tok.integer_value);
    else if (Kw("struct")) {
        // Eat struct keyword
        NextToken();

        if (not At(Tk::Keyword)) {
            return Error(
                ErrorId::Expected,
                "Expected name of struct following 'struct' keyword"
            );
        }
        name = tok.text;
        // Eat struct name
        NextToken();

        // Register struct name as named type, but it refers to nullptr.
        // This means we will be able to check if a type recursively mentions
        // itself (by seeing if the name is registered but still attributed to
        // nullptr, we know we are still in the middle of parsing that type).
        named_types[name] = nullptr;

        if (auto r = ConsumeOrError(Tk::LBrace); not r)
            return r.diag();

        std::vector<Type*> members{};
        while (not At(Tk::RBrace, Tk::Eof, Tk::Newline, Tk::Indent)) {
            auto member_t = ParseType();
            if (not member_t) return member_t;
            members.emplace_back(*member_t);
            // Optionally eat comma following each struct member.
            Consume(Tk::Comma);
        }

        if (auto r = ConsumeOrError(Tk::RBrace); not r)
            return r.diag();

        base = StructType::Get(
            mod->context(),
            members,
            StructType::AlignNotSet,
            name
        );
    } else if (At(Tk::Global, Tk::Keyword)) {
        auto n = tok.text;
        // Eat name of named type.
        NextToken();

        // Find name in list of registered named types.
        auto found = named_types.find(n);
        if (found == named_types.end()) {
            return Error(
                ErrorId::Miscellaneous,
                "Got name when parsing type, but '{}' doesn't refer to an existing named type!",
                n
            );
        }

        // If a named type is registered to nullptr, we are still parsing the type
        // and it is incomplete, and therefore cannot show up in it's own
        // definition (as it is an incomplete type).
        LCC_ASSERT(found->second, "A named type must not reference itself in it's own definition.");

        return found->second;
    } else return Error(ErrorId::Expected, "Expected type");
    NextToken();

    /// Parse qualifiers.
    ///
    /// Note that function types should never appear on their own in
    /// the IR, but rather only in call instructions and definitions
    /// of functions, where they’re handled separately. Do not attempt
    /// to parse function types here.
    while (At(Tk::LBrack)) {
        NextToken();

        if (not At(Tk::Integer))
            return Error(ErrorId::Expected, "Expected integer");
        auto size = tok.integer_value;
        NextToken();

        if (not Consume(Tk::RBrack))
            return Error(ErrorId::Expected, "Expected ']'");

        base = ArrayType::Get(mod->context(), (usz) size, base);
    }

    if (not name.empty())
        named_types[name] = base;

    return base;
}

auto lcc::parser::Parser::ParseUntypedValue(Type* assumed_type) -> Result<IRValue> {
    if (Kw("poison")) {
        NextToken();
        return IRValue{new (*mod) PoisonValue(assumed_type)};
    }

    if (At(Tk::Global)) {
        auto text = tok.text;
        NextToken();
        auto it = globals.find(text);
        if (it == globals.end()) return IRValue{Global{std::move(text)}};
        return IRValue{it->second};
    }

    if (At(Tk::Temporary)) {
        auto text = tok.text;
        NextToken();
        auto it = temporaries.find(text);
        if (it == temporaries.end()) return IRValue{Temporary{std::move(text)}};
        return IRValue{it->second};
    }

    if (At(Tk::Integer)) {
        auto i = new (*mod) IntegerConstant(
            assumed_type,
            tok.integer_value
        );
        NextToken();
        return IRValue{i};
    }

    /// TODO: strings and array constants.
    return Error(ErrorId::Expected, "Expected value");
}

auto lcc::parser::Parser::ParseValue() -> Result<std::pair<Type*, IRValue>> {
    auto ty = ParseType();
    if (ty.is_diag()) return ty.diag();
    auto val = ParseUntypedValue(*ty);
    if (val.is_diag()) return val.diag();
    return std::pair{*ty, *val};
}

void lcc::parser::Parser::SetValue(Inst* parent, Value*& val, IRValue v) {
    if (auto value = std::get_if<Value*>(&v)) {
        val = *value;
        Inst::AddUse(*value, parent);
    } else if (auto g = std::get_if<Global>(&v)) {
        global_fixups[g->data].emplace_back(parent, &val);
    } else {
        temporary_fixups[std::get<Temporary>(v).data].emplace_back(parent, &val);
    }
}

void lcc::parser::Parser::SetBlock(Inst* parent, lcc::Block*& val, lcc::parser::Parser::IRValue v) {
    if (auto* value = std::get_if<Value*>(&v)) {
        auto* b = cast<Block>(*value);
        if (not b) {
            Error(ErrorId::Miscellaneous, "Value does not name a block");
            return;
        }
        val = b;
        Inst::AddUse(b, parent);
    } else if ([[maybe_unused]] auto* g = std::get_if<Global>(&v)) {
        Error(ErrorId::Miscellaneous, "Value does not name a block");
    } else {
        block_fixups[std::get<Temporary>(v).data]
            .emplace_back(parent, &val);
    }
}

auto lcc::Module::Parse(Context* ctx, std::string_view source) -> std::unique_ptr<Module> {
    parser::Parser p{ctx, source};
    if (not p.ParseModule()) return nullptr;
    return std::move(p.mod);
}

auto lcc::Module::Parse(Context* ctx, File& file) -> std::unique_ptr<Module> {
    parser::Parser p{ctx, &file};
    if (not p.ParseModule()) return nullptr;
    return std::move(p.mod);
}
