#ifndef LCC_DIAGS_HH
#define LCC_DIAGS_HH

#include <lcc/forward.hh>
#include <lcc/location.hh>
#include <lcc/utils.hh>

namespace lcc {
/// A diagnostic. The diagnostic is issued when the destructor is called.
struct Diag {
    /// Diagnostic severity.
    enum struct Kind {
        None,    ///< Not an error. Do not emit this diagnostic.
        Note,    ///< Informational note.
        Warning, ///< Warning, but no hard error.
        Error,   ///< Hard error. Program is ill-formed.
        FError,  ///< Fatal (system) error. NOT a compiler bug.
        ICError, ///< Compiler bug.
    };

private:
    const Context* ctx;
    Kind kind;
    Location where;
    std::string msg;

    /// Attached diagnostics.
    std::vector<std::pair<Diag, bool>> attached;

    /// Handle fatal error codes.
    void HandleFatalErrors();

    /// Print a diagnostic with no (valid) location info.
    void PrintDiagWithoutLocation();

    /// Determine whether we should use colours at all.
    bool ShouldUseColour() const;

public:
    static constexpr u8 ICE_EXIT_CODE = 17;
    static constexpr u8 FATAL_EXIT_CODE = 18;

    Diag(Diag&& other)
        : ctx(other.ctx),
          kind(other.kind),
          where(other.where),
          msg(std::move(other.msg)),
          attached(std::move(other.attached)) {
        other.kind = Kind::None;
    }

    Diag& operator=(Diag&& other) {
        if (this == &other) return *this;
        ctx = other.ctx;
        kind = other.kind;
        where = other.where;
        msg = std::move(other.msg);
        attached = std::move(other.attached);
        other.kind = Kind::None;
        return *this;
    }

    /// Create an empty diagnostic.
    explicit Diag() : ctx(nullptr), kind(Kind::None), where(), msg() {}

    /// Disallow copying.
    Diag(const Diag&) = delete;
    Diag& operator=(const Diag&) = delete;

    /// The destructor prints the diagnostic, if it hasn’t been moved from.
    ~Diag();

    /// Issue a diagnostic.
    Diag(const Context* ctx, Kind kind, Location where, std::string msg)
        : ctx(ctx), kind(kind), where(where), msg(std::move(msg)) {}

    /// Issue a diagnostic with no location.
    Diag(Kind _kind, std::string&& msg)
        : ctx(nullptr), kind(_kind), where(), msg(std::move(msg)) {}

    /// Issue a diagnostic with a format string and arguments.
    template <typename... Args>
    Diag(
        const Context* ctx,
        Kind kind,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) : Diag{ctx, kind, where, fmt::format(fmt, std::forward<Args>(args)...)} {}

    /// Issue a diagnostic with a format string and arguments, but no location.
    template <typename... Args>
    Diag(Kind kind, fmt::format_string<Args...> fmt, Args&&... args)
        : Diag{kind, fmt::format(fmt, std::forward<Args>(args)...)} {}

    /// Attach another diagnostic to this one.
    ///
    /// \param print_before If true, the diagnostic will be printed
    ///     before this one. Otherwise, it will be printed after this
    ///     one.
    /// \param diag The diagnostic to attach.
    void attach(bool print_before, Diag&& diag) {
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
    static Diag Note(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag{Kind::Note, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a note.
    template <typename... Args>
    static Diag Note(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        return Diag{ctx, Kind::Note, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static Diag Warning(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag{Kind::Warning, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static Diag Warning(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        return Diag{ctx, Kind::Warning, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag{Kind::Error, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static Diag Error(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        return Diag{ctx, Kind::Error, where, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Raise an internal compiler error and exit.
    template <typename... Args>
    [[noreturn]] static void ICE(fmt::format_string<Args...> fmt, Args&&... args) {
        Diag{Kind::ICError, fmt::format(fmt, std::forward<Args>(args)...)};
        std::terminate(); /// Should never be reached.
    }

    /// Raise an internal compiler error at a location and exit.
    template <typename... Args>
    [[noreturn]] static void ICE(
        const Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        Diag{ctx, Kind::ICError, where, fmt::format(fmt, std::forward<Args>(args)...)};
        std::terminate(); /// Should never be reached.
    }

    /// Raise a fatal error and exit.
    ///
    /// This is NOT an ICE; instead it is an error that is probably caused by
    /// the underlying system, such as attempting to output to a directory that
    /// isn’t accessible to the user.
    template <typename... Args>
    [[noreturn]] static void Fatal(fmt::format_string<Args...> fmt, Args&&... args) {
        Diag{Kind::FError, fmt::format(fmt, std::forward<Args>(args)...)};
        std::terminate(); /// Should never be reached.
    }
};

} // namespace lcc

#endif // LCC_DIAGS_HH
