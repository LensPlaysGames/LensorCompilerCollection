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
    const char* end;
    const char* begin;

    enum class Encoding {
        UTF8,
        ASCII,

        COUNT
    } encoding{Encoding::UTF8};

public:
    explicit CharacterRange(File* f)
        : curr(f->data()),
          end(f->data() + f->size()),
          begin(f->data()) {}

    explicit CharacterRange(File* f, usz pos, usz len)
        : curr(f->data() + std::min(pos, f->size() - 1)),
          end(f->data() + std::min(pos + len, f->size())),
          begin(f->data()) {}

    explicit CharacterRange(std::string_view s)
        : curr(s.data()),
          end(s.data() + s.size()),
          begin(s.data()) {}

    /// Get the current offset in the file.
    [[nodiscard]]
    auto current_offset() const -> u32 {
        return u32(curr - begin) - 1;
    }

    [[nodiscard]]
    static auto utf8_pack_bytes(u32 b0, u32 b1 = 0, u32 b2 = 0, u32 b3 = 0) -> u32 {
        if ((b0 & 0b1110'0000U) == 0b1100'0000U) {
            // 2 byte encoding; first byte holds top five bits, second byte holds
            // bottom six.
            u32 byte0 = (b0 & 0b0001'1111U) << 6U;
            u32 byte1 = b1 & 0b0011'1111U;
            return byte0 | byte1;
        }

        if ((b0 & 0b1111'0000U) == 0b1110'0000U) {
            u32 byte0 = (b0 & 0b0000'1111U) << 12U;
            u32 byte1 = (b1 & 0b0011'1111U) << 6U;
            u32 byte2 = b2 & 0b0011'1111U;
            return byte0 | byte1 | byte2;
        }

        if ((b0 & 0b1111'1000U) == 0b1111'0000U) {
            u32 byte0 = (b0 & 0b0000'0111U) << 18U;
            u32 byte1 = (b1 & 0b0011'1111U) << 12U;
            u32 byte2 = (b2 & 0b0011'1111U) << 6U;
            u32 byte3 = b3 & 0b0011'1111U;
            return byte0 | byte1 | byte2 | byte3;
        }

        return b0;
    }

    // Given the first byte of a utf8 encoded unicode character, return the
    // encoded codepoint. May advance.
    [[nodiscard]]
    auto rest_utf8(u8 b0) -> u32 {
        constexpr auto is_continuation = [](u8 b) -> bool {
            return (b & 0b1100'0000U) == 0b1000'0000U;
        };

        // Skip continuation bytes. This will skip leftover from overlong
        // encodings as well as handle the case of a random continuation byte in
        // the file.
        while (is_continuation(b0)) b0 = u8(*curr++);

        auto remaining_bytes = end - curr;
        if ((b0 & 0b1110'0000U) == 0b1100'0000U) {
            if (remaining_bytes < 1) {
                fmt::print("ERROR: Lexer expected 2-byte UTF8 encoding but there is not enough input");
                return 0;
            }
            auto b1 = (u8(*curr++));
            if (not is_continuation(b1)) {
                fmt::print("ERROR: Lexer expected 2-byte UTF8 encoding but one of the continuation bytes is not a continuation byte (invalid utf8 encoding).");
                return 0;
            }
            return utf8_pack_bytes(b0, b1);
        }

        if ((b0 & 0b1111'0000U) == 0b1110'0000U) {
            if (remaining_bytes < 2) {
                fmt::print("ERROR: Lexer expected 3-byte UTF8 encoding but there is not enough input");
                return 0;
            }
            auto b1 = (u8(*curr++));
            auto b2 = (u8(*curr++));
            if (not rgs::all_of(std::array<u8, 2>{b1, b2}, is_continuation)) {
                fmt::print("ERROR: Lexer expected 3-byte UTF8 encoding but one of the continuation bytes is not a continuation byte (invalid utf8 encoding).");
                return 0;
            }
            return utf8_pack_bytes(b0, b1, b2);
        }

        if ((b0 & 0b1111'1000U) == 0b1111'0000U) {
            if (remaining_bytes < 3) {
                fmt::print("ERROR: Lexer expected 4-byte UTF8 encoding but there is not enough input");
                return 0;
            }
            auto b1 = (u8(*curr++));
            auto b2 = (u8(*curr++));
            auto b3 = (u8(*curr++));
            if (not rgs::all_of(std::array<u8, 3>{b1, b2, b3}, is_continuation)) {
                fmt::print("ERROR: Lexer expected 3-byte UTF8 encoding but one of the continuation bytes is not a continuation byte (invalid utf8 encoding).");
                return 0;
            }
            return utf8_pack_bytes(b0, b1, b2, b3);
        }

        // Single-byte encoded character
        return b0;
    }

    /// Get the next character.
    [[nodiscard]]
    auto next() -> u32 {
        if (curr >= end) return 0;
        const auto c = *curr++;

        // Handle NUL byte.
        if (c == 0) {
            fmt::print("ERROR: Lexer encountered NUL byte within source file.");
            return 0;
        }

        // Handle line endings nonsense.
        // Convert CRLF -> LF, LFCR -> LF, CR -> LF, and LF -> LF.
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

        // Parse the encoded character code-unit into a codepoint.
        switch (encoding) {
            case Encoding::ASCII: return u32(c);
            case Encoding::UTF8: return rest_utf8(u8(c));

            case Encoding::COUNT:
                LCC_UNREACHABLE();
        }
        LCC_UNREACHABLE();
    }
};
} // namespace detail

template <typename TToken>
struct Lexer {
    detail::CharacterRange chars;
    TToken tok{};
    Context* context{};
    u32 lastc = ' ';

    explicit Lexer(detail::CharacterRange chs) : chars(chs) {}
    Lexer(Context* ctx, detail::CharacterRange chs)
        : chars(chs), context(ctx) {}
    Lexer(Context* ctx, std::string_view source)
        : chars(detail::CharacterRange(source)), context(ctx) {}
    Lexer(Context* ctx, File* file)
        : chars(detail::CharacterRange(file)), context(ctx) {
        tok.location.file_id = (decltype(tok.location.file_id)) file->file_id();
    }

    [[nodiscard]]
    auto CurrentOffset() const -> u32 { return chars.current_offset(); }

    void NextChar() {
        lastc = chars.next();
    }

    template <typename... Args>
    auto Error(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Error(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto Warning(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Warning(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto Note(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Note(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    static auto IsSpace(u32 c) -> bool {
        return c == ' ' or c == '\t' or c == '\n'
            or c == '\r' or c == '\f' or c == '\v';
    }
    static auto IsAlpha(u32 c) -> bool {
        return (c >= 'a' and c <= 'z')
            or (c >= 'A' and c <= 'Z');
    }
    static auto IsDigit(u32 c) -> bool {
        return c >= '0' and c <= '9';
    }
    static auto IsHexDigit(u32 c) -> bool {
        return (c >= '0' and c <= '9')
            or (c >= 'a' and c <= 'f')
            or (c >= 'A' and c <= 'F');
    }
    static auto IsAlphaNumeric(u32 c) -> bool {
        return IsAlpha(c) or IsDigit(c);
    }
};

template <typename TToken>
class SpanLexer : public Lexer<TToken> {
    std::string_view _source;

protected:
    SpanLexer(Context* context, std::string_view source)
        : Lexer<TToken>(context, source),
          _source(source) {
        this->NextChar();
    }

    // FIXME: These necessary?
    using Lexer<TToken>::Error;
    using Lexer<TToken>::Warning;
    using Lexer<TToken>::Note;
};

template <typename TToken>
class SpanWithinFileLexer : public Lexer<TToken> {

protected:
    SpanWithinFileLexer(Context* context, File* file, usz offset, usz length)
        : Lexer<TToken>(context, file, offset, length) {
        this->NextChar();
    }

    // FIXME: These necessary?
    using Lexer<TToken>::Error;
    using Lexer<TToken>::Warning;
    using Lexer<TToken>::Note;
};

template <typename TToken>
class FileLexer : public Lexer<TToken> {
    File* _file;

protected:
    FileLexer(Context* context, File* file)
        : Lexer<TToken>(context, file),
          _file(file) {
        this->NextChar();
    }

    using Lexer<TToken>::Error;
    using Lexer<TToken>::Warning;
    using Lexer<TToken>::Note;
};
} // namespace lcc::syntax

#endif // LCC_SYNTAX_LEXER_HH
