#ifndef C_PARSER_HH
#define C_PARSER_HH

#include <c/ast.hh>
#include <c/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::c {
class Parser {
    TranslationUnit* _tu;
    File* _file;

    Lexer _lexer;
    
    CToken token{};
    std::vector<CToken> look_ahead{};

public:
    static auto Parse(CContext* context, File& file) -> TranslationUnit*;

private:
    Parser(TranslationUnit* tu, File& file)
        : _tu(tu), _file(&file), _lexer(Lexer{tu, &file}) {}

    auto translation_unit() const { return _tu; }
    auto c_context() const { return _tu->c_context(); }
    auto lcc_context() const { return _tu->lcc_context(); }

    auto CurrLocation() { return token.location; }

    auto PeekToken(usz ahead = 1) -> CToken {
        LCC_ASSERT(ahead >= 1, "Peek look-ahead indexing starts at 1.");
        while (look_ahead.size() < ahead) {
            CToken aheadToken{};
            _lexer.ReadToken(aheadToken);
            look_ahead.push_back(aheadToken);
        }

        return look_ahead[ahead - 1];
    }

    /// Read the next token into tok.
    auto NextToken() {
        if (not look_ahead.empty()) {
            token = look_ahead[0];
            // pop from the front of the vector
            look_ahead.erase(look_ahead.begin());
        } else _lexer.ReadToken(token);
        return token;
    }

    /// Check if we’re at one of a set of tokens.
    [[nodiscard]] auto At(auto... tks) { return ((token.kind == tks) or ...); }
    [[nodiscard]] auto PeekAt(usz ahead, auto... tks) { return ((PeekToken(ahead).kind == tks) or ...); }

    /// Like At(), but consume the token if it matches.
    bool Consume(auto... tks) {
        if (At(tks...)) {
            NextToken();
            return true;
        }
        return false;
    }

    bool IsAtTypeStart();

    auto ParseTopLevel() -> Decl*;
    auto ParseType() -> Type*;
};
}

#endif // C_PARSER_HH
