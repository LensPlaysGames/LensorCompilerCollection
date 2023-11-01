#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/platform.hh>

/// ===========================================================================
///  Diagnostics.
/// ===========================================================================
using Kind = lcc::Diag::Kind;

namespace {
/// Get the colour of a diagnostic.
static constexpr auto Colour(lcc::Diag::Kind kind) -> lcc::utils::Colour {
    switch (kind) {
        using enum lcc::utils::Colour;
        case Kind::ICError: return Magenta;
        case Kind::Warning: return Yellow;
        case Kind::Note: return Green;

        case Kind::FError:
        case Kind::Error:
            return Red;

        default:
            return Reset;
    }
}

/// Get the name of a diagnostic.
static constexpr std::string_view Name(lcc::Diag::Kind kind) {
    switch (kind) {
        case Kind::ICError: return "Internal Compiler Error";
        case Kind::FError: return "Fatal Error";
        case Kind::Error: return "Error";
        case Kind::Warning: return "Warning";
        case Kind::Note: return "Note";
        default: return "Diagnostic";
    }
}
} // namespace

/// Abort due to assertion failure.
[[noreturn]] void lcc::detail::AssertFail(std::string&& msg) {
    Diag::ICE("{}", std::move(msg));
}

void lcc::Diag::HandleFatalErrors() {
    /// Abort on ICE.
    if (kind == Kind::ICError) {
        lcc::platform::PrintBacktrace();
        std::exit(ICE_EXIT_CODE);
    }

    /// Exit on a fatal error.
    if (kind == Kind::FError) std::exit(FATAL_EXIT_CODE);
}

/// Print a diagnostic with no (valid) location info.
void lcc::Diag::PrintDiagWithoutLocation() {
    using enum utils::Colour;
    utils::Colours C(ShouldUseColour());

    /// Print the message.
    fmt::print(stderr, "{}{}{}: {}", C(Bold), C(Colour(kind)), Name(kind), C(Reset));
    fmt::print(stderr, "{}\n", msg);
    HandleFatalErrors();
}

bool lcc::Diag::ShouldUseColour() const {
    if (ctx) return ctx->use_colour_diagnostics();
    return lcc::platform::StderrIsTerminal();
}

lcc::Diag::~Diag() { print(); }

void lcc::Diag::print() {
    using enum utils::Colour;

    /// If this diagnostic is suppressed, do nothing.
    if (kind == Kind::None) return;

    /// Don’t print the same diagnostic twice.
    defer { kind = Kind::None; };

    /// Print attached diagnostics to be printed before this one.
    for (auto& [diag, print_before] : attached)
        if (print_before)
            diag.print();

    /// Diagnostics to be printed after this one will be printed later.
    defer {
        for (auto& [diag, print_before] : attached)
            if (not print_before)
                diag.print();

        /// Not necessary but it may help conserve storage.
        attached.clear();
    };

    /// If the diagnostic is an error, set the error flag.
    if (kind == Kind::Error and ctx) ctx->set_error();

    /// If there is no context, then there is also no location info.
    if (not ctx) {
        PrintDiagWithoutLocation();
        return;
    }

    /// If the location is invalid, either because the specified file does not
    /// exists, its position is out of bounds or 0, or its length is 0, then we
    /// skip printing the location.
    utils::Colours C(ShouldUseColour());
    const auto& fs = ctx->files();
    if (not where.seekable(ctx)) {
        /// Even if the location is invalid, print the file name if we can.
        if (where.file_id < fs.size()) {
            const auto& file = *fs[where.file_id].get();
            fmt::print(stderr, "{}{}: ", C(Bold), file.path().string());
        }

        /// Print the message.
        PrintDiagWithoutLocation();
        return;
    }

    /// If the location is valid, get the line, line number, and column number.
    const auto [line, col, line_start, line_end] = where.seek(ctx);

    /// Split the line into everything before the range, the range itself,
    /// and everything after.
    std::string before(line_start, col);
    std::string range(line_start + col, where.len);
    std::string after(line_start + col + where.len, line_end);

    /// Replace tabs with spaces. We need to do this *after* splitting
    /// because this invalidates the offsets.
    utils::ReplaceAll(before, "\t", "    ");
    utils::ReplaceAll(range, "\t", "    ");
    utils::ReplaceAll(after, "\t", "    ");

    /// Print the file name, line number, and column number.
    const auto& file = *fs[where.file_id].get();
    fmt::print(stderr, "{}{}:{}:{}: ", C(Bold), file.path().string(), line, col);

    /// Print the diagnostic name and message.
    fmt::print(stderr, "{}{}: {}{}\n", C(Colour(kind)), Name(kind), C(Reset), msg);

    /// Print the line up to the start of the location, the range in the right
    /// colour, and the rest of the line.
    fmt::print(stderr, " {} | {}", line, before);
    fmt::print(stderr, "{}{}{}{}", C(Bold), C(Colour(kind)), range, C(Reset));
    fmt::print(stderr, "{}\n", after);

    /// Determine the number of digits in the line number.
    const auto digits = utils::NumberWidth(line);

    /// Underline the range. For that, we first pad the line based on the number
    /// of digits in the line number and append more spaces to line us up with
    /// the range.
    for (usz i = 0; i < digits + before.size() + sizeof("  | ") - 1; i++)
        fmt::print(stderr, " ");

    /// Finally, underline the range.
    fmt::print(stderr, "{}{}", C(Bold), C(Colour(kind)));
    for (usz i = 0; i < range.size(); i++) fmt::print(stderr, "~");
    fmt::print(stderr, "{}\n", C(Reset));

    /// Handle fatal errors.
    HandleFatalErrors();
}

void lcc::Diag::print_attached() {
    for (auto& [diag, print_before] : attached)
        if (print_before)
            diag.print();

    for (auto& [diag, print_before] : attached)
        if (not print_before)
            diag.print();

    attached.clear();
}
