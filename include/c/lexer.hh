#ifndef C_LEXER_HH
#define C_LEXER_HH

#include "lcc/file.hh"

#include <c/ast.hh>
#include <lcc/utils.hh>

namespace lcc::c {
class Lexer {
    struct MacroDef {
        std::string name;

        bool has_args = false;
        std::vector<std::string> args{};
        std::vector<CToken> body{};

        MacroDef(std::string name, std::vector<CToken> body)
            : name(std::move(name)), has_args(false), body(std::move(body)) {}

        MacroDef(std::string name, std::vector<std::string> args, std::vector<CToken> body)
            : name(std::move(name)), has_args(true), args(std::move(args)), body(std::move(body)) {}

        MacroDef(std::string name, bool has_args, std::vector<std::string> args, std::vector<CToken> body)
            : name(std::move(name)), has_args(has_args), args(std::move(args)), body(std::move(body)) {}
    };

    TranslationUnit* _tu;
    File* _file;

    const char* curr{};
    const char* end{};

    char current_char = 0;
    bool is_at_start_of_line = true;
    bool is_in_preprocessor = false;

    StringMap<MacroDef> macro_defs{};

public:
    Lexer(TranslationUnit* tu, File* file)
        : _tu(tu), _file(file), curr(file->data()), end(file->data() + file->size()) {
        AdvanceChar();
    }

    auto translation_unit() const { return _tu; }
    auto c_context() const { return _tu->c_context(); }
    auto lcc_context() const { return _tu->lcc_context(); }

    /// Read the next token, without the preprocessor enabled, into `token`.
    void ReadTokenNoPreprocess(CToken& token);
    /// Read the next token, with the preprocessor enabled, into `token`.
    void ReadToken(CToken& token);

private:
    /// Invoked when the preprocessor is enabled and a start-of-line '#' is encountered.
    /// Responsible for parsing all preprocessor directives and doing any necessary work
    /// that comes with it.
    void HandlePreprocessorDirective();
    /// Skips to the end of a preprocessor directive.
    void SkipToEndOfPreprocessorDirective(CToken current_token);
    /// Invoked when a preprocessor directive with the name 'define' is encountered.
    void HandleDefineDirective(const CToken& define_token);
    /// Continues advancing to the next character until a non-whitespace character is encountered.
    /// The result of a call to `CurrentChar()` will return this character.
    /// If the lexer is in a preprocessing state, then '\n' will not be eaten.
    void EatWhitespace();

    /// Returns the current character after character-based preprocessing.
    char CurrentChar() const { return current_char; }
    /// Returns the character `ahead` bytes ahead of the current lexer position.
    /// This function is named as such because it does not do any of the fancy legwork
    /// that `AdvanceChar()` does, like handling Backslash+Newline or comments.
    /// If Preprocessor-smenatic accurate lookahead is needed, a second function called
    /// `PeekChar()` should be created and used instead.
    char PeekCharNoProcess(int ahead = 1) {
        const char* peek = curr + ahead;
        if (peek >= end)
            return 0;
        return *peek;
    }
    /// Returns the character `ahead` bytes ahead of the current lexer position, taking into
    /// account only Backslah+Newline deletion.
    char PeekCharSkipEscapedNewline(int ahead = 1) {
        const char* where = curr;
        while (ahead > 0 and not IsAtEndOfFile()) {
            while (SkipBackslashWithNewline()) {}
            ahead--;
        }

        if (IsAtEndOfFile()) {
            curr = where;
            return 0;
        }

        char result = *curr;
        curr = where;

        return result;
    }

    /// Returns true if this lexer has reached the end of its file, false otherwise.
    /// More technically, this currently does not return true for rogue NUL bytes in the file.
    /// This behavior may need to be changed to be standard compliant.
    bool IsAtEndOfFile() const { return curr >= end; }
    /// Returns true if the current character is the first non-space, non-comment character
    /// in this line.
    bool IsAtStartOfLine() const { return is_at_start_of_line; }
    /// Returns true if the lexer is currently in a preprocessing state.
    /// This enables preprocessor functionality, such as returning the EndOfLine token when
    /// unprocessed newlines are encountered.
    bool IsInPreprocessor() const { return is_in_preprocessor; }

    /// NOTE(local): here, if ever we want to support trigraphs, we need to check for them *before* we
    /// process Backslash+Newline.
    ///
    /// This check handles Backslash+Newline, Backslash+Newline+CarriageReturn and
    /// Backslash+CarriageReturn+Newline. It does *not* handle Backslash+CarriageReturn on its own.
    ///
    /// TODO(local): what does the standard say is a "newline" character? is this correct?
    bool SkipBackslashWithNewline() {
        if (not IsAtEndOfFile() and *curr == '\\' and (PeekCharNoProcess() == '\n' or (PeekCharNoProcess() == '\r' and PeekCharNoProcess(2) == '\n'))) {
            curr++; // skip the backslash
            LCC_ASSERT(not IsAtEndOfFile());
            if (*curr == '\n') {
                curr++;
                if (not IsAtEndOfFile() and *curr == '\r') curr++;
            } else {
                LCC_ASSERT(*curr == '\r' && PeekCharNoProcess() == '\n');
                curr += 2;
            }
            return true;
        }
        return false;
    }

    /// Advances this lexer to the next character.
    /// This process handles the Backslash+Newline deletion if encountered.
    /// If `allow_comments` is true, which it is by default, then comments
    /// are also skipped and `CurrentChar()` will return a single space for them.
    /// Any relevant errors and warnings are issued during these processes.
    void AdvanceChar(bool allow_comments = true) {
        auto ReadNextChar = [&]() -> char {
            if (IsAtEndOfFile()) return 0;

            /// If the char we're skipping naturally is a newline, then it wasn't eaten
            /// by a Backslash+Newline or a comment, so we should be very safe to say we're at the start
            /// of a line.
            /// This value can also be set from within a comment as a separate case.
            /// This value is only set to false when a non-whitespace character is encountered outside of
            /// a comment.
            if (CurrentChar() == '\n') {
                is_at_start_of_line = true;
            } else if (not IsSpace(CurrentChar())) {
                /// If I did this correctly, then this marks the second non-space, non-comment
                /// character as not the start of line, but the first is left in tact.
                is_at_start_of_line = true;
            }

            curr++;
            if (IsAtEndOfFile()) return 0;

            LCC_ASSERT(not IsAtEndOfFile());
            while (SkipBackslashWithNewline()) {}

            if (IsAtEndOfFile()) return 0;

            /// Handle comments as well. When we lex a comment, we'll set the current character to a
            /// space, then immediately return.
            /// Since we're treating comments as spaces, we don't need to do any processing afterward.
            if (allow_comments and *curr == '/') {
                if (PeekCharNoProcess() == '/') {
                    while (not IsAtEndOfFile()) {
                        if (*curr == '\\' and SkipBackslashWithNewline()) {
                            Warning("Multiline // comment");
                        }
                        if (IsAtEndOfFile()) break;
                        char c = *curr;
                        curr++;
                        if (c == '\n') {
                            is_at_start_of_line = c == '\n';
                            break;
                        }
                    }
                    return ' ';
                } else if (PeekCharNoProcess() == '*') {
                    curr += 2;
                    char lastc = 0;
                    for (;;) {
                        if (IsAtEndOfFile()) {
                            Error("Unterminated /* comment");
                            break;
                        }
                        if (*curr == '\\') SkipBackslashWithNewline();
                        if (IsAtEndOfFile()) {
                            Error("Unterminated /* comment");
                            break;
                        }
                        char c = *curr;
                        curr++;
                        if (c == '\n') is_at_start_of_line = true;
                        if (lastc == '*' and c == '/') break;
                        lastc = c;
                    }
                    return ' ';
                }
            }

            /// We've done every necessary check and haven't returned yet.
            /// If we weaseled our way to the end of file, handle that,
            /// otherwise we can finally use the current character.
            if (IsAtEndOfFile())
                return 0;
            else return *curr;
        };

        current_char = ReadNextChar();
    }

    auto CurrentOffset() const -> u32 { return u32(curr - _file->data()) - 1; }
    auto CurrentLocation() const {
        return Location{CurrentOffset(), (u16) 1, (u16) _file->file_id()};
    }

    template <typename... Args>
    Diag Warning(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Warning(lcc_context(), CurrentLocation(), fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(lcc_context(), CurrentLocation(), fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    Diag Error(Location location, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(lcc_context(), location, fmt, std::forward<Args>(args)...);
    }

    std::string GetSubstring(u32 startOffset, u32 endOffset) {
        u32 count = endOffset - startOffset;
        return std::string(_file->data() + startOffset, count);
    }

    bool IsIdentifierContinue(char c) {
        if (c == '$' and c_context()->opts.ext.gnu_idents)
            return true;

        return IsAlphaNumeric(c) || c == '_';
    }

    static bool IsSpace(char c) { return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\f' or c == '\v'; }
    static bool IsAlpha(char c) { return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z'); }
    static bool IsDigit(char c) { return c >= '0' and c <= '9'; }
    static bool IsHexDigit(char c) { return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F'); }
    static bool IsAlphaNumeric(char c) { return IsAlpha(c) or IsDigit(c); }

    static int GetDigitValueInBase(char c, int base) {
        if (c >= '0' and c <= '9') {
            int valueInBase = c - '0';
            if (valueInBase >= base) return -1;
            return valueInBase;
        } else if (c >= 'a' and c <= 'z') {
            int valueInBase = c - 'a' + 11;
            if (valueInBase >= base) return -1;
            return valueInBase;
        } else if (c >= 'A' and c <= 'Z') {
            int valueInBase = c - 'A' + 11;
            if (valueInBase >= base) return -1;
            return valueInBase;
        }

        return -1;
    }

    static bool IsDigitInBase(char c, int base) { return GetDigitValueInBase(c, base) >= 0; }
};
} // namespace lcc::c

#endif // C_LEXER_HH
