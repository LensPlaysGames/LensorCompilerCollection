#ifndef LCC_SYNTAX_LEXER_HH
#define LCC_SYNTAX_LEXER_HH

#include <lcc/diags.hh>
#include <lcc/file.hh>
#include <lcc/utils.hh>

namespace lcc::syntax {
namespace detail {
/// API for the lexer to make sure the lexer code doesn’t
/// try to do funny stuff with `curr` since extracting the
/// next character is NOT trivial and should ONLY ever be
/// done by calling `next()`!!!
///
/// To reiterate, this MUST NOT expose a way of retrieving
/// a source span!
class CharacterRange {
    /// Note: Slight data duplication here since the
    /// lexer already stores the file and the context,
    /// but 16 extra bytes per lexer instance is fine,
    /// I’d say.
    Context* const ctx;
    File* const f;

    const char* curr;
    const char* const end;

public:
    CharacterRange(Context* ctx, File* f)
        : ctx(ctx),
          f(f),
          curr(f->data()),
          end(f->data() + f->size()) {}

    /// Get the current offset in the file.
    auto current_offset() const -> u32 {
        return u32(curr - f->data()) - 1;
    }

    /// Get the next character.
    char next() {
        if (curr >= end) return 0;
        const auto c = *curr++;

        /// Stray null.
        if (c == 0) {
            Diag::Error(
                ctx,
                Location{(u32) current_offset(), (u16) 1, (u16) f->file_id()},
                "Lexer encountered NUL byte within source file."
            );
        }

        /// Handle line endings nonsene.
        if (c == '\r' || c == '\n') {
            if (curr != end && (*curr == '\r' || *curr == '\n')) {
                bool same = c == *curr;

                /// CRCR or LFLF
                if (same) return '\n';

                /// CRLF or LFCR
                curr++;
            }

            /// Either CR or LF followed by something else.
            return '\n';
        }

        /// Regular character.
        return c;
    }
};
}

template <typename TToken>
class Lexer {
    File* file;
    detail::CharacterRange chars;

protected:
    TToken tok{};
    Context* context{};
    char lastc = ' ';

    Lexer(Context* context, File* file)
        : file(file),
          chars(context, file),
          context(context) { NextChar(); }

    auto FileId() const { return file->file_id(); }

    auto CurrentOffset() const -> u32 { return chars.current_offset(); }

    auto CurrentLocation() const {
        return Location{CurrentOffset(), (u16) 1, (u16) file->file_id()};
    }

    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    void NextChar() {
        lastc = chars.next();
    }

    static bool IsSpace(char c) { return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\f' or c == '\v'; }
    static bool IsAlpha(char c) { return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z'); }
    static bool IsDigit(char c) { return c >= '0' and c <= '9'; }
    static bool IsHexDigit(char c) { return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F'); }
    static bool IsAlphaNumeric(char c) { return IsAlpha(c) or IsDigit(c); }
};
} // namespace lcc::syntax

#endif // LCC_SYNTAX_LEXER_HH
