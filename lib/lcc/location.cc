#include <lcc/context.hh>
#include <lcc/location.hh>
#include <lcc/utils.hh>

bool lcc::Location::seekable(const lcc::Context* ctx) const {
    auto& files = ctx->files();
    if (file_id >= files.size()) return false;
    const auto* f = files[file_id].get();
    return is_valid() and ((pos + len <= f->size()) or pos == f->size());
}

/// Seek to a source location. The location must be valid.
auto lcc::Location::seek(const lcc::Context* ctx) const -> LocInfo {
    LCC_ASSERT(ctx);
    LCC_ASSERT(seekable(ctx), "Cannot seek Location that is not seekable");

    LocInfo info{};

    /// Get the file that the location is in.
    auto& files = ctx->files();
    const auto* f = files.at(file_id).get();

    // If the location starts on a newline, which line would we prefer
    // to gather? I believe the previous line would make more sense.
    //
    // we don't do this
    // \nfoo
    // \nbar
    // we do this
    // foo\n
    // bar\n
    // so, a newline is associated not with the line it starts, but with the
    // one it ends.
    //
    // "foo\nbar\nEOF"
    //     ^   location points here
    // I believe we should collect "foo\n" as the "containing line",
    // not "bar\n".
    //
    // "foo\nEOF"
    //     ^   location points here
    // I believe we should collect "foo\n" as the "containing line",
    // not "EOF".
    //
    // "foo\n\n\nEOF"
    //       ^   location points here
    // I believe we should collect "\n" as the "containing line",
    // not "foo\n".
    //
    // "foo\nbar\nEOF"
    //        ^   location points here
    // We should collect "bar\n" as the "containing line".

    // Seek back to the start of the line.
    const char* const data = f->data();
    info.line_start = data + pos;
    // Basically, if we are at a newline, move behind it.
    if (info.line_start > data and *info.line_start == '\n')
        --info.line_start;
    // Collect the line we are currently at, up until the start of the file or
    // the end of another line.
    while (info.line_start > data and *info.line_start != '\n')
        --info.line_start;
    // If we made it to the end of a different line, do not include the end of
    // that line as the start of this line.
    if (*info.line_start == '\n') ++info.line_start;

    // Seek forward to the end of the line.
    const char* const end = data + f->size();
    LCC_ASSERT(len, "Location with zero length is not seekable");
    info.line_end = data + pos + len;
    while (info.line_end < end and *info.line_end != '\n')
        ++info.line_end;

    // Determine the line and column number.
    // TODO: Use preprocessor define to allow 0 or 1 based line numbers?
    info.line = 1;
    for (const char* d = data; d < data + pos; ++d) {
        if (*d == '\n') {
            ++info.line;
            info.col = 0;
        } else ++info.col;
    }

    LCC_ASSERT(
        info.line_start != info.line_end,
        "A valid, seekable location is never empty"
    );

    /// Done!
    return info;
}

/// TODO: Lexer should create map that counts where in a file the lines start so
/// we can do binary search on that instead of iterating over the entire file.
auto lcc::Location::seek_line_column(const lcc::Context* ctx) const -> LocInfoShort {
    LCC_ASSERT(ctx);
    LCC_ASSERT(seekable(ctx), "Cannot seek Location that is not seekable");

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
