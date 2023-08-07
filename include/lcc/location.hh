#ifndef LCC_LOCATION_HH
#define LCC_LOCATION_HH

#include <lcc/utils.hh>

namespace lcc {
/// A source range in a file.
struct Location {
    u32 pos;
    u16 len;
    u16 file_id;

    bool is_valid() const { return len != 0; }
};

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
} // namespace lcc

#endif // LCC_LOCATION_HH
