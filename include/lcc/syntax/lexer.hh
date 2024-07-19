#ifndef LCC_SYNTAX_LEXER_HH
#define LCC_SYNTAX_LEXER_HH

#include <lcc/diags.hh>
#include <lcc/file.hh>
#include <lcc/utils.hh>

#include <string_view>
#include <utility>

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

    const char* curr;
    const char* const end;
    const char* const begin;

public:
    CharacterRange(File* f)
        : curr(f->data()),
          end(f->data() + f->size()),
          begin(f->data()) {}

    CharacterRange(std::string_view s)
        : curr(s.data()),
          end(s.data() + s.size()),
          begin(s.data()) {}

    /// Get the current offset in the file.
    auto current_offset() const -> u32 {
        return u32(curr - begin) - 1;
    }

    /// Get the next character.
    char next() {
        if (curr >= end) return 0;
        const auto c = *curr++;

        /// Stray null.
        if (c == 0) {
            fmt::print("ERROR: Lexer encountered NUL byte within source file.");
            return 0;
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
} // namespace detail

template <typename TToken>
struct Lexer {
    detail::CharacterRange chars;
    TToken tok{};
    Context* context{};
    char lastc = ' ';

    Lexer(detail::CharacterRange chs) : chars(chs) {}
    Lexer(Context* ctx, detail::CharacterRange chs)
        : chars(chs), context(ctx) {}

    auto CurrentOffset() const -> u32 { return chars.current_offset(); }

    void NextChar() {
        lastc = chars.next();
    }

    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    Diag Warning(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Warning(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    static bool IsSpace(char c) { return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\f' or c == '\v'; }
    static bool IsAlpha(char c) { return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z'); }
    static bool IsDigit(char c) { return c >= '0' and c <= '9'; }
    static bool IsHexDigit(char c) { return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F'); }
    static bool IsAlphaNumeric(char c) { return IsAlpha(c) or IsDigit(c); }
};

template <typename TToken>
class SpanLexer : public Lexer<TToken> {
    std::string_view source;

protected:
    SpanLexer(Context* context, std::string_view source)
        : Lexer<TToken>(context, source),
          source(source) {
        this->NextChar();
    }

    // FIXME: These necessary?
    using Lexer<TToken>::Error;
    using Lexer<TToken>::Warning;
};

template <typename TToken>
class FileLexer : public Lexer<TToken> {
    File* file;

protected:
    FileLexer(Context* context, File* file)
        : Lexer<TToken>(context, file),
          file(file) {
        this->NextChar();
    }

    using Lexer<TToken>::Error;
    using Lexer<TToken>::Warning;

    auto FileId() const { return file->file_id(); }

    auto CurrentLocation() const {
        return Location{Lexer<TToken>::CurrentOffset(), (u16) 1, (u16) file->file_id()};
    }
};
} // namespace lcc::syntax

#endif // LCC_SYNTAX_LEXER_HH
