#include <lcc/diags.hh>
#include <lcc/syntax/lexer.hh>

namespace lcc::syntax {
template <typename TToken>
void Lexer<TToken>::NextChar() {
    if (curr >= end) {
        lastc = 0;
        return;
    }

    lastc = *curr++;
    if (lastc == 0) {
        Diag::Error(
            context,
            Location{CurrentOffset(), 1},
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
} // namespace lcc::syntax
