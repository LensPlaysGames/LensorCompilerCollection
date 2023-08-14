#ifndef LCC_SYNTAX_LEXER_HH
#define LCC_SYNTAX_LEXER_HH

#include <lcc/diags.hh>
#include <lcc/file.hh>
#include <lcc/utils.hh>

namespace lcc::syntax {
template <typename TToken>
class Lexer {
    File* file;

    const char* curr{};
    const char* end{};

protected:
    TToken tok{};
    Context* context{};
    char lastc = ' ';

    Lexer(Context* context, File* file)
        : context(context), file(file), curr(file->data()), end(file->data() + file->size()) { NextChar(); }

    auto FileId() const { return file->file_id(); }

    auto CurrentOffset() const -> u32 { return u32(curr - file->data()) - 1; }

    auto CurrentLocation() const {
        return Location{CurrentOffset(), (u16) 1, (u16) file->file_id()};
    }

    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    std::string GetSubstring(u32 startOffset, u32 endOffset) {
        u32 count = endOffset - startOffset;
        return std::string(file->data() + startOffset, count);
    }

    void NextChar() {
        if (curr >= end) {
            lastc = 0;
            return;
        }

        lastc = *curr++;
        if (lastc == 0) {
            Diag::Error(
                context,
                Location{(u32) CurrentOffset(), (u16) 1, (u16) file->file_id()},
                "Lexer encountered NUL byte within source file."
            );
        }

        if (lastc == '\r' || lastc == '\n') {
            if (curr != end && (*curr == '\r' || *curr == '\n')) {
                bool same = lastc == *curr;
                lastc = '\n';

                /// CRCR or LFLF
                if (same) return;

                /// CRLF or LFCR
                curr++;
            }

            /// Either CR or LF followed by something else.
            lastc = '\n';
        }
    }

    static bool IsSpace(char c) { return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\f' or c == '\v'; }
    static bool IsAlpha(char c) { return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z'); }
    static bool IsDigit(char c) { return c >= '0' and c <= '9'; }
    static bool IsHexDigit(char c) { return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F'); }
    static bool IsAlphaNumeric(char c) { return IsAlpha(c) or IsDigit(c); }
};
} // namespace lcc::syntax

#endif // LCC_SYNTAX_LEXER_HH
