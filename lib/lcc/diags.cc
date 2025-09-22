#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/platform.hh>

#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

/// ===========================================================================
///  Diagnostics.
/// ===========================================================================
using Kind = lcc::Diag::Kind;

namespace {
/// Get the colour of a diagnostic.
constexpr auto Colour(lcc::Diag::Kind kind) -> lcc::utils::Colour {
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
constexpr auto Name(lcc::Diag::Kind kind) -> std::string_view {
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

// Exit due to assertion failure.
[[noreturn]]
void lcc::detail::AssertFail(std::string&& msg) {
    Diag::ICE("{}", std::move(msg));
}

void lcc::Diag::HandleFatalErrors() {
    // Exit on internal compiler error (ICE).
    if (kind == Kind::ICError) {
        lcc::platform::PrintBacktrace();
        std::exit(ICE_EXIT_CODE);
    }

    // Also exit on any fatal error.
    if (kind == Kind::FError)
        std::exit(FATAL_EXIT_CODE);
}

/// Print a diagnostic with no (valid) location info.
void lcc::Diag::PrintDiagWithoutLocation() {
    using enum utils::Colour;
    utils::Colours C(ShouldUseColour());

    /// Print the message.
    fmt::print(
        stderr,
        "{}{}{}: {}",
        C(Bold),
        C(Colour(kind)),
        Name(kind),
        C(Reset)
    );
    fmt::print(stderr, "{}\n", message);
    HandleFatalErrors();
}

auto lcc::Diag::ShouldUseColour() const -> bool {
    if (context) return context->option_use_colour();
    return lcc::platform::StderrIsTerminal();
}

lcc::Diag::~Diag() { print(); }

constexpr auto named_link(std::string_view uri, std::string_view name) -> std::string {
    return fmt::format("\033]8;;{}\033\\{}\033]8;;\033\\", uri, name);
}

auto file_link(const lcc::fs::path& path) -> std::string {
    return named_link(
        fmt::format("file://{}", lcc::fs::absolute(path).string()),
        lcc::fs::relative(path).string()
    );
}

void lcc::Diag::print() {
    using enum utils::Colour;

    // If this diagnostic is suppressed, do nothing.
    if (kind == Kind::None) return;

    // Don’t print the same diagnostic twice.
    defer { kind = Kind::None; };

    // Print attached diagnostics to be printed before this one.
    for (auto& [diag, print_before] : attached)
        if (print_before)
            diag.print();

    // Diagnostics to be printed after this one will be printed later.
    defer {
        for (auto& [diag, print_before] : attached)
            if (not print_before)
                diag.print();

        // Not necessary but it may help conserve storage.
        attached.clear();
    };

    // If the diagnostic is an error, set the error flag.
    if (kind == Kind::Error and context) context->set_error();

    // If there is no context, then there is also no location info.
    if (not context) {
        PrintDiagWithoutLocation();
        return;
    }

    // If the location is invalid, either because the specified file does not
    // exists, its position is out of bounds or 0, or its length is 0, then we
    // skip printing the location.
    utils::Colours C(ShouldUseColour());
    const auto& fs = context->files();
    if (not where.seekable(context)) {
        // Even if the location is invalid, print the file name if we can.
        if (where.file_id < fs.size()) {
            const auto& file = *fs[where.file_id].get();
            fmt::print(stderr, "{}{}: ", C(Bold), file.path().string());
        }

        // Print the message.
        PrintDiagWithoutLocation();
        return;
    }

    // If the location is valid, get the line, line number, and column number.
    const auto [line, col, line_start, line_end] = where.seek(context);

    bool location_is_multiline{false};
    for (auto* it = line_start; it < line_end; ++it) {
        if (*it == '\n') {
            location_is_multiline = true;
            break;
        }
    }

    // Split the line into everything before the range, the range itself, and
    // everything after.
    std::string before(line_start, col);
    std::string range(line_start + col, where.len);
    std::string after(std::min(line_start + col + where.len, line_end), line_end);

    // Replace tabs with spaces. We need to do this *after* splitting because
    // this invalidates the offsets.
    utils::ReplaceAll(before, "\t", "    ");
    utils::ReplaceAll(range, "\t", "    ");
    utils::ReplaceAll(after, "\t", "    ");

    // TODO: If diagnostic points to the end of a line, insert a space to highlight.

    // Print the file name, line number, and column number.
    const auto& file = *fs[where.file_id].get();
    fmt::print(stderr, "{}{}:{}:{}: ", C(Bold), fs::relative(file.path()).string(), line, col);
    // fmt::print(stderr, "{}{}:{}:{}: ", C(Bold), file_link(file.path()), line, col);

    // Print the diagnostic name and message.
    // TODO: If message is multiple lines, format it a little differently to be a little more understandable.
    std::vector<usz> message_newline_offsets{};
    for (usz i = 0; i < message.size(); ++i)
        if (message.at(i) == '\n') message_newline_offsets.push_back(i);

    if (not message_newline_offsets.empty()) {
        // Print message prefix
        fmt::print(stderr, "{}{}:{} ", C(Colour(kind)), Name(kind), C(Reset));

        usz printed_offset = 0;
        for (auto newline_offset : message_newline_offsets) {
            // Do indentation for continuing lines, but only if the lines don't begin
            // with their own indentation already.
            if (printed_offset != 0 and message.at(printed_offset) != ' ')
                fmt::print(stderr, "    ");
            fmt::print(
                stderr,
                "{}",
                std::string_view(
                    message.begin() + isz(printed_offset),
                    message.begin() + isz(newline_offset) + 1
                )
            );
            printed_offset = newline_offset + 1;
        }
        // Last part of format without a trailing newline.
        if (not message.ends_with('\n')) {
            if (message.at(printed_offset) != ' ') fmt::print(stderr, "    ");
            fmt::print(
                stderr,
                "{}\n",
                std::string_view(
                    message.begin() + isz(printed_offset),
                    message.end()
                )
            );
        }
    } else fmt::print(stderr, "{}{}: {}{}\n", C(Colour(kind)), Name(kind), C(Reset), message);

    // Print the line up to the start of the location, the range in the right
    // colour, and the rest of the line.
    fmt::print(stderr, " {} | {}", line, before);
    fmt::print(stderr, "{}{}{}{}", C(Bold), C(Colour(kind)), range, C(Reset));
    fmt::print(stderr, "{}\n", after);

    // Determine the number of digits in the line number.
    const auto digits = utils::NumberWidth(line);

    // Underline the range.
    // TODO: Multiline location underline calculation
    if (not location_is_multiline) {
        // We first pad the line based on the number of digits in the line number
        // and append more spaces to line us up with the range.
        for (usz i = 0; i < digits + before.size() + sizeof("  | ") - 1; ++i)
            fmt::print(stderr, " ");

        // Finally, print the underline itself.
        fmt::print(stderr, "{}{}", C(Bold), C(Colour(kind)));
        for (usz i = 0; i < range.size(); ++i) fmt::print(stderr, "~");
        fmt::print(stderr, "{}\n", C(Reset));
    }

    // Handle fatal errors.
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
