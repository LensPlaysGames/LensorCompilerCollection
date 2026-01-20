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

/// A diagnostic. The diagnostic is issued when the destructor is called.
struct Diag {
    friend Context;

    /// Diagnostic severity.
    enum struct Kind {
        None,    ///< Not an error. Do not emit this diagnostic.
        Note,    ///< Informational note.
        Warning, ///< Warning, but no hard error.
        Error,   ///< Hard error. Program being compiled is ill-formed.
        FError,  ///< Fatal error, but NOT a compiler bug.
        ICError, ///< Internal Compiler error (bug in compiler).
    };

    struct Fix {
        enum struct Kind {
            // Do not perform any operation on the source on behalf of this fix.
            INVALID,

            // Replace text at fix.location with fix.text.
            REPLACE,

            // Insert fix.text at start of fix.location.
            INSERT,

            // Do not perform any operation on the source on behalf of this fix.
            COUNT
        } kind{Diag::Fix::Kind::INVALID};

        std::string text{};
        Location location{};
    };

private:
    Kind kind;
    const Context* context{};
    Location where{};
    std::string id{};
    std::string message{};

    /// Attached diagnostics.
    struct ShouldPrintBefore {
        bool v;

        operator bool() { return v; }
    };
    std::vector<std::pair<Diag, ShouldPrintBefore>> attached{};

    // A list of objects that dictate how to change the source in such a way
    // that this diagnostic would no longer be emitted (and ideally no others
    // would, either). Optional.
    std::vector<Fix> fixes{};

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
          id(std::move(other.id)),
          message(std::move(other.message)),
          attached(std::move(other.attached)) {
        other.kind = Kind::None;
    }

    auto operator=(Diag&& other) noexcept -> Diag& {
        if (this == &other) return *this;
        context = other.context;
        kind = other.kind;
        where = other.where;
        id = std::move(other.id);
        message = std::move(other.message);
        attached = std::move(other.attached);
        other.kind = Kind::None;
        return *this;
    }

    /// Create an empty diagnostic.
    explicit Diag() : kind(Kind::None) {};

    /// Disallow copying.
    Diag(const Diag&) = delete;
    auto operator=(const Diag&) -> Diag& = delete;

    /// The destructor prints the diagnostic, if it hasn’t been moved from.
    ~Diag();

    /// Issue a diagnostic.
    Diag(
        Context* context_,
        Kind kind_,
        Location where_,
        std::string id_,
        std::string message_
    );

    /// Issue a diagnostic with no location.
    Diag(
        Kind kind_,
        std::string&& id_,
        std::string&& message_
    ) : kind(kind_),
        id(std::move(id_)),
        message(std::move(message_)) {}

    /// Issue a diagnostic with a format string and arguments.
    template <typename... Args>
    Diag(
        Context* context_,
        Kind kind_,
        Location where_,
        std::string id_,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) : Diag{context_, kind_, where_, id_, fmt::format(fmt, std::forward<Args>(args)...)} {}

    /// Issue a diagnostic with a format string and arguments, but no location.
    template <typename... Args>
    Diag(Kind kind_, std::string id_, fmt::format_string<Args...> fmt, Args&&... args)
        : Diag{kind_, id_, fmt::format(fmt, std::forward<Args>(args)...)} {}

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

    void fix_by(Fix fix) { fixes.emplace_back(fix); }

    void fix_by_replacing_with(
        Location where_to_replace,
        std::string text_to_replace_with
    ) {
        fix_by(
            {Diag::Fix::Kind::REPLACE,
             text_to_replace_with,
             where_to_replace}
        );
    }

    void fix_by_inserting_at(
        Location where_to_insert,
        std::string text_to_insert
    ) {
        fix_by(
            {Diag::Fix::Kind::INSERT,
             text_to_insert,
             where_to_insert}
        );
    }

    /// Emit a note.
    template <typename... Args>
    static auto Note(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Note, "note", fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a note.
    template <typename... Args>
    static auto Note(
        Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Note, where, "note", fmt::format(fmt, std::forward<Args>(args)...)};
    }
    /// Emit a note.
    template <typename... Args>
    static auto Note(std::string id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Note, id, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a note.
    template <typename... Args>
    static auto Note(
        Context* ctx,
        Location where,
        std::string id,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Note, where, id, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static auto Warning(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Warning, "warning", fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static auto Warning(
        Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Warning, where, "warning", fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static auto Warning(std::string id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Warning, id, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit a warning.
    template <typename... Args>
    static auto Warning(
        Context* ctx,
        Location where,
        std::string id,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Warning, where, id, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static auto Error(
        Context* ctx,
        Location where,
        std::string id,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Error, where, id, fmt::format(fmt, std::forward<Args>(args)...)};
    }
    /// Emit an error.
    template <typename... Args>
    static auto Error(std::string id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Error, id, fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Emit an error.
    template <typename... Args>
    static auto Error(
        Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> Diag {
        return Diag{ctx, Kind::Error, where, "error", fmt::format(fmt, std::forward<Args>(args)...)};
    }
    /// Emit an error.
    template <typename... Args>
    static auto Error(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag{Kind::Error, "error", fmt::format(fmt, std::forward<Args>(args)...)};
    }

    /// Raise an internal compiler error and exit.
    template <typename... Args>
    [[noreturn]]
    static void ICE(fmt::format_string<Args...> fmt, Args&&... args) {
        // Yes, this nested scope is important; the destructor prints the
        // diagnostic, and an ICE exits after printing.
        { Diag _{Kind::ICError, "internal-compiler-error", fmt::format(fmt, std::forward<Args>(args)...)}; }
        fmt::print(stderr, "\n¡¡BIG PROBLEM!! ICE didn't exit...\n");
        std::terminate(); /// Should never be reached.
    }

    /// Raise an internal compiler error at a location and exit.
    template <typename... Args>
    [[noreturn]]
    static void ICE(
        Context* ctx,
        Location where,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) {
        { Diag _{ctx, Kind::ICError, where, "internal-compiler-error", fmt::format(fmt, std::forward<Args>(args)...)}; }
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
    static void Fatal(std::string id, fmt::format_string<Args...> fmt, Args&&... args) {
        { Diag _{Kind::FError, id, fmt::format(fmt, std::forward<Args>(args)...)}; }
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
        { Diag _{Kind::FError, "fatal-error", fmt::format(fmt, std::forward<Args>(args)...)}; }
        std::terminate(); /// Should never be reached.
    }
};

} // namespace lcc

#endif // LCC_DIAGS_HH
