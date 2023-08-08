#ifndef LAYE_PARSER_HH
#define LAYE_PARSER_HH

#include <laye/ast.hh>
#include <laye/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Parser {
    Lexer lexer;
    LayeToken tok{};
    std::unique_ptr<Module> mod{};

    Parser(Context* context, File* file)
        : lexer(Lexer{context, file}) {}

    /// Check if we’re at one of a set of tokens.
    [[nodiscard]] auto At(auto... tks) { return ((tok.kind == tks) or ...); }

    /// Like At(), but consume the token if it matches.
    bool Consume(TokenKind tk) {
        if (At(tk)) {
            NextToken();
            return true;
        }
        return false;
    }

    /// Read the next token into tok.
    void NextToken() {
        lexer.ReadToken(tok);
    }

    /// Synchronise on semicolons and braces.
    void Synchronise();

public:
    static std::unique_ptr<Module> Parse(Context* context, File* file);
};
} // namespace lcc::laye

#endif // LAYE_PARSER_HH
