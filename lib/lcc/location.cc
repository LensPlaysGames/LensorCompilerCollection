#include <lcc/context.hh>
#include <lcc/location.hh>

bool lcc::Location::seekable(const lcc::Context* ctx) const {
    auto& files = ctx->files();
    if (file_id >= files.size()) return false;
    const auto* f = files[file_id].get();
    return pos + len <= f->size() and is_valid();
}

/// Seek to a source location. The location must be valid.
auto lcc::Location::seek(const lcc::Context* ctx) const -> LocInfo {
    LocInfo info{};

    /// Get the file that the location is in.
    auto& files = ctx->files();
    const auto* f = files.at(file_id).get();

    // Seek back to the start of the line.
    const char* const data = f->data();
    info.line_start = data + pos;
    while (info.line_start > data and *info.line_start != '\n') info.line_start--;
    if (*info.line_start == '\n') info.line_start++;

    // Seek forward to the end of the line.
    const char* const end = data + f->size();
    info.line_end = data + pos + len;
    while (info.line_end < end and *info.line_end != '\n') info.line_end++;

    // Determine the line and column number.
    info.line = 1;
    for (const char* d = data; d < data + pos; d++) {
        if (*d == '\n') {
            info.line++;
            info.col = 0;
        } else {
            info.col++;
        }
    }

    /// Done!
    return info;
}

/// TODO: Lexer should create map that counts where in a file the lines start so
/// we can do binary search on that instead of iterating over the entire file.
auto lcc::Location::seek_line_column(const lcc::Context* ctx) const -> LocInfoShort {
    LocInfoShort info{};

    /// Get the file that the location is in.
    auto& files = ctx->files();
    const auto* f = files.at(file_id).get();

    /// Seek back to the start of the line.
    const char* const data = f->data();

    /// Determine the line and column number.
    info.line = 1;
    for (const char* d = data; d < data + pos; d++) {
        if (*d == '\n') {
            info.line++;
            info.col = 0;
        } else {
            info.col++;
        }
    }

    /// Done!
    return info;
}
