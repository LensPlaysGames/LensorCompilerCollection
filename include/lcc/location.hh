#ifndef LCC_LOCATION_HH
#define LCC_LOCATION_HH

#include <lcc/utils.hh>

namespace lcc {
class Context;

/// A decoded source location.
struct LocInfo {
    usz line;
    usz col;
    const char* line_start;
    const char* line_end;
};

/// A short decoded source location.
struct LocInfoShort {
    usz line;
    usz col;
};

/// A source range in a file.
struct Location {
    u32 pos{};
    u16 len{};
    u16 file_id{};

    Location() = default;
    Location(u32 pos, u16 len, u16 file_id)
        : pos(pos), len(len), file_id(file_id) {}

    /// Create a new location that spans two locations.
    Location(Location a, Location b) {
        if (a.file_id != b.file_id) return;
        if (not a.is_valid() or not b.is_valid()) return;
        pos = std::min<u32>(a.pos, b.pos);
        len = u16(std::max<u32>(a.pos + a.len, b.pos + b.len) - pos);
    }

    /// Seek to a source location.
    [[nodiscard]] auto seek(const Context* ctx) const -> LocInfo;

    /// Seek to a source location, but only return the line and column.
    [[nodiscard]] auto seek_line_column(const Context* ctx) const -> LocInfoShort;

    /// Check if the source location is seekable.
    [[nodiscard]] bool seekable(const Context* ctx) const;

    bool is_valid() const { return len != 0; }
};
} // namespace lcc

#endif // LCC_LOCATION_HH
