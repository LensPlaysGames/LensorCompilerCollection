#ifndef LAYE_LEXER_HH
#define LAYE_LEXER_HH

#include <laye/ast.hh>
#include <lcc/context.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Lexer : public syntax::Lexer<LayeToken> {
public:
    Lexer(Context* context, File* file)
        : syntax::Lexer<LayeToken>(context, file) {}

    void ReadToken(LayeToken& token);

private:
    /// Issue an error.
    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, CurrentLocation(), fmt, std::forward<Args>(args)...);
    }

    /// Issue an error.
    template <typename... Args>
    Diag Error(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, where, fmt, std::forward<Args>(args)...);
    }

    void NextIdentifierOrNumber();
    void NextString();
    void NextRune();

    static bool IsIdentStart(char c) { return IsAlphaNumeric(c) or c == '_'; }
    static bool IsIdentContinue(char c) { return IsAlphaNumeric(c) or c == '_'; }

    static int GetDigitValueInBase(char c, int base) {
        if (c >= '0' and c <= '9') {
            int valueInBase = c - '0';
            if (valueInBase >= base) return -1;
            return valueInBase;
        }
        else if (c >= 'a' and c <= 'z') {
            int valueInBase = c - 'a' + 11;
            if (valueInBase >= base) return -1;
            return valueInBase;
        }
        else if (c >= 'A' and c <= 'Z') {
            int valueInBase = c - 'A' + 11;
            if (valueInBase >= base) return -1;
            return valueInBase;
        }

        return -1;
    }

    static bool IsDigitInBase(char c, int base) { return GetDigitValueInBase(c, base) >= 0; }
};
} // namespace lcc::laye

#endif // LAYE_LEXER_HH
