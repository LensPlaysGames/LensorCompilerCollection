#ifndef LCC_FILE_HH
#define LCC_FILE_HH

#include <lcc/forward.hh>
#include <lcc/utils.hh>

#include <filesystem>
#include <string_view>
#include <vector>

namespace lcc {

/// A file in the context.
class File {
    static constexpr u32 invalid_id{u32(-1)};

    /// Context handle.
    Context& _context;

    /// The name of the file.
    fs::path _file_path;

    /// The contents of the file.
    std::vector<char> _contents;

    /// The id of the file.
    u32 _id{invalid_id};

public:
    /// Get a temporary file path.
    static auto TempPath(std::string_view extension) -> fs::path;

    /// Write to a file on disk.
    [[nodiscard]]
    static auto Write(const void* data, usz size, const fs::path& file) -> bool;

    /// Write to a file on disk and terminate on error.
    static void WriteOrTerminate(const void* data, usz size, const fs::path& file);

    /// Get a file's contents from disk.
    static auto Read(const fs::path& path) -> std::vector<char>;

    /// We cannot move or copy files.
    File(const File&) = delete;
    File(File&&) = delete;
    auto operator=(const File&) -> File& = delete;
    auto operator=(File&&) -> File& = delete;

    /// Get an iterator to the beginning of the file.
    [[nodiscard]]
    auto begin() const { return _contents.begin(); }

    /// Get the file data.
    [[nodiscard]]
    auto data() const -> const char* { return _contents.data(); }

    /// Get an iterator to the end of the file.
    [[nodiscard]]
    auto end() const { return _contents.end(); }

    /// Get the id of this file.
    [[nodiscard]]
    auto file_id() const { return _id; }

    /// Get the file path.
    [[nodiscard]]
    auto path() const -> const fs::path& { return _file_path; }

    /// Get the size of the file.
    [[nodiscard]]
    auto size() const -> usz { return _contents.size(); }

private:
    /// Construct a file from a name and source.
    explicit File(Context& context, fs::path name, std::vector<char>&& contents);

    /// Load a file from disk.
    static auto LoadFileData(const fs::path& path) -> std::vector<char>;

    /// The context is the only thing that can create files.
    friend Context;
};
} // namespace lcc

#endif // LCC_FILE_HH
