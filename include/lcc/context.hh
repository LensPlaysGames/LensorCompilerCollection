#ifndef LCC_CONTEXT_HH
#define LCC_CONTEXT_HH

#include <lcc/file.hh>
#include <lcc/forward.hh>
#include <lcc/location.hh>
#include <lcc/utils.hh>

namespace lcc {
class Context {
    /// The files owned by the context.
    std::vector<std::unique_ptr<File>> owned_files;

    /// Error flag. This is set-only.
    mutable bool error_flag = false;

public:
    /// Create a new context.
    explicit Context() = default;

    /// Do not allow copying or moving the context.
    Context(const Context&) = delete;
    Context(Context&&) = delete;
    Context& operator=(const Context&) = delete;
    Context& operator=(Context&&) = delete;

    /// Currently this doesn’t do anything special.
    ~Context() = default;

    /// Create a new file from a name and contents.
    template <typename Buffer>
    File& create_file(fs::path name, Buffer&& contents) {
        return make_file(
            std::move(name),
            std::vector<char>{std::forward<Buffer>(contents)}
        );
    }

    /// Get a list of all files owned by the context.
    [[nodiscard]] auto files() const -> const decltype(owned_files)& {
        return owned_files;
    }

    /// Get a file from disk.
    ///
    /// This loads a file from disk or returns a reference to it if
    /// is has already been loaded.
    ///
    /// \param path The path to the file.
    /// \return A reference to the file.
    File& get_or_load_file(fs::path path);

    /// Check if the error flag is set.
    [[nodiscard]] bool has_error() const { return error_flag; }

    /// Set the error flag.
    ///
    /// \return The previous value of the error flag.
    bool set_error() const {
        auto old = error_flag;
        error_flag = true;
        return old;
    }

private:
    /// Register a file in the context.
    File& make_file(fs::path name, std::vector<char>&& contents);
};
} // namespace lcc

#endif // LCC_CONTEXT_HH
