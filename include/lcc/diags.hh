#ifndef LCC_DIAGS_HH
#define LCC_DIAGS_HH

#include <lcc/forward.hh>
#include <lcc/location.hh>
#include <lcc/utils.hh>

#include <exception>
#include <string>
#include <utility>
#include <vector>

namespace lcc {

// TODO: "fix" diagnostic which inserts suggested text into a given source
// line, and may optionally apply the fix.

/// A diagnostic. The diagnostic is issued when the destructor is called.
struct Diag {
    // TODO: "Fix" diagnostic type for suggested overwrites/fixes to the
    // source, and eventually the ability to actually commit the fix to the
    // file.

    /// Diagnostic severity.
    enum struct Kind {
        None,    ///< Not an error. Do not emit this diagnostic.
        Note,    ///< Informational note.
        Warning, ///< Warning, but no hard error.
        Error,   ///< Hard error. Program being compiled is ill-formed.
        FError,  ///< Fatal error, but NOT a compiler bug.
        ICError, ///< Internal Compiler error (bug in compiler).
    };

private:
    Kind kind;
    const Context* context{};
    Location where{};
    std::string message{};

    /// Attached diagnostics.
    std::vector<std::pair<Diag, bool>> attached;

    /// Handle fatal error codes.
    void HandleFatalErrors();

    /// Print a diagnostic with no (valid) location info.
    void PrintDiagWithoutLocation();

    /// Determine whether we should use colours at all.
    [[nodiscard]]
    auto ShouldUseColour() const -> bool;

public:
    static constexpr u8 ICE_EXIT_CODE = 17;
    static constexpr u8 FATAL_EXIT_CODE = 18;

    // Move constructor
    Diag(Diag&& other) noexcept
        : kind(other.kind),
          context(other.context),
          where(other.where),
          message(std::move(other.message)),
          attached(std::move(other.attached)) {
        other.kind = Kind::None;
    }

    auto operator=(Diag&& other) noexcept -> Diag& {
        if (this == &other) return *this;
        context = other.context;
        kind = other.kind;
        where = other.where;
        message = std::move(other.message);
        attached = std::move(other.attached);
        other.kind = Kind::None;
        return *this;
    }

    /// Create an empty diagnostic.
    explicit Diag() : kind(Kind::None){};

    /// Disallow copying.
    Diag(const Diag&) = delete;
    auto operator=(const Diag&) -> Diag& = delete;

    /// The destructor prints the diagnostic, if it hasn’t been moved from.
    ~Diag();

    /// Issue a diagnostic.
    Diag(const Context* context_, Kind kind_, Location where_, std::string message_)
        : kind(kind_), context(context_), where(where_), message(std::move(message_)) {}

    /// Issue a diagnostic with no location.
    Diag(Kind kind_, std::string&& message_)
        : kind(kind_), message(std::move(message_)) {}

    /// Issue a diagnostic with a format string and arguments.
    template <typename... Args>
    Diag(
        const Context* context_,
        Kind kind_,
        Location where_,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) : Diag{context_, kind_, where_, fmt::format(fmt, std::forward<Args>(args)...)} {}

    /// Issue a diagnostic with a format string and arguments, but no location.
    template <typename... Args>
    Diag(Kind kind_, fmt::format_string<Args...> fmt, Args&&... args)
        : Diag{kind_, fmt::format(fmt, std::forward<Args>(args)...)} {}

    /// Attach another diagnostic to this one.
    ///
    /// \param print_before If true, the diagnostic will be printed
    ///     before this one. Otherwise, it will be printed after this
    ///     one.
    /// \param diag The diagnostic to attach.
    void attach(Diag&& diag, bool print_before = false) {
        attached.emplace_back(std::move(diag), print_before);
    }

    /// Print this diagnostic now. This resets the diagnostic.
    void print();

    /// Print all attached diagnostics now.
    ///
    /// All diagnostics for which \c print_before was \c true will be
    /// printed first, all other diagnostics will be printed afterward.
    ///
    /// This removes any attached diagnostics.
    void print_attached();

    /// Suppress this and all attached diagnostics (it will not be printed).
    void suppress(bool issue_attached_diagnostics = false) {
        if (issue_attached_diagnostics) print_attached();
        kind = Kind::None;
    }

    /// Emit a note.
    template <typename... Args>
    static auto Note(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Note, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a note.
    template <typename... Args>
    static auto Note(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Note, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static auto Warning(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Warning, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static auto Warning(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Warning, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static auto Error(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Error, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static auto Error(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Error, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Raise an internal compiler error and exit.
    template <typename... Args>
    [[noreturn]]
    static void ICE(fmt::format_string<Args...> fmt, Args&&... args) {
        // Yes, this nested scope is important; the destructor prints the
        // diagnostic, and an ICE exits after printing.
        { Diag _{Kind::ICError, fmt::format(fmt, std::forward<Args>(args)...)}; }
        fmt::print(stderr, "\n¡¡BIG PROBLEM!! ICE didn't exit...\n");
        std::terminate(); /// Should never be reached.
    }

    /// Raise an internal compiler error at a location and exit.
    template <typename... Args>
    [[noreturn]]
    static void ICE(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        { Diag _{ctx, Kind::ICError, where, fmt::format(fmt, std::forward<Args>(args)...)}; }
        fmt::print(stderr, "\n¡¡BIG PROBLEM!! ICE didn't exit...\n");
        std::terminate(); /// Should never be reached.
    }

    /// Raise a fatal error and exit.
    ///
    /// This is NOT an ICE; instead it is an error that is probably caused by
    /// the underlying system, such as attempting to output to a directory that
    /// isn’t accessible to the user.
    template <typename... Args>
    [[noreturn]]
    static void Fatal(fmt::format_string<Args...> fmt, Args&&... args) {
        { Diag _{Kind::FError, fmt::format(fmt, std::forward<Args>(args)...)}; }
        std::terminate(); /// Should never be reached.
    }
};

} // namespace lcc

#endif // LCC_DIAGS_HH
