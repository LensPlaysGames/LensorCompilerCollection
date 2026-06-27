#include <clink/link_uarchive.hh>

#include <object/generic.hh>

#include <algorithm>
#include <array>
#include <bit>
#include <cstdlib>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace uarchive {
constexpr std::array<char, 8> magic_bytes{'!', '<', 'a', 'r', 'c', 'h', '>', '\n'};

// NOTE: Numbers stored as ASCII decimal characters, not binary value.
struct FileHeader {
    char _identifier[16]{};
    char _modified_seconds[12]{};
    char _owner_id[6]{};
    char _group_id[6]{};
    // Type and permissions
    // NOTE: octal ASCII characters
    char _mode[8]{};
    char _size[10];
    char _reserved[2]{0x60, 0x0a};

    auto identifier() const -> std::string_view {
        return {_identifier, sizeof(_identifier)};
    }
    auto size() const -> uint32_t {
        return (uint32_t) strtoul(_size, nullptr, 10);
    }
};

// ensure struct matches binary format
static_assert(
    offsetof(FileHeader, _identifier) == 0
    and offsetof(FileHeader, _modified_seconds) == 16
    and offsetof(FileHeader, _owner_id) == 28
    and offsetof(FileHeader, _group_id) == 34
    and offsetof(FileHeader, _mode) == 40
    and offsetof(FileHeader, _size) == 48
    and offsetof(FileHeader, _reserved) == 58
);

} // namespace uarchive

namespace clink {

auto collect_uarchive(
    std::span<char> blob,
    std::vector<std::string_view> undefined,
    std::vector<std::string>& visited
) -> std::vector<std::vector<char>> {
    // Ensure blob is big enough for magic bytes + at least one file header...
    if (blob.size() <= uarchive::magic_bytes.size() + sizeof(uarchive::FileHeader))
        return {};

    uint32_t offset{uarchive::magic_bytes.size()};
    uarchive::FileHeader hdr{};
    memcpy(
        &hdr,
        blob.data() + offset,
        sizeof(uarchive::FileHeader)
    );

    std::vector<std::vector<char>> blobs{};

    // GNU Symbol Table
    // Layout:
    //   4 byte big endian integer -- amount of symbols
    //   SYMBOL COUNT * 4 byte big endian integers
    //   SYMBOL_COUNT * null-terminated strings
    if (hdr.identifier().starts_with("/ ")) {
        auto symbol_table_data_offset = offset + sizeof(uarchive::FileHeader);
        auto symbol_table_blob = blob.subspan(
            symbol_table_data_offset,
            symbol_table_data_offset + hdr.size()
        );

        // If we encounter a "/<digits>" name, we can assume (given GNU symbol
        // table format) the next entry is "//" and will contain the strings at
        // byte offsets specified by said digits.
        auto string_table_data_offset = symbol_table_data_offset
                                      + hdr.size()
                                      + sizeof(uarchive::FileHeader);

        uint32_t symbol_count{};
        memcpy(&symbol_count, symbol_table_blob.data(), sizeof(symbol_count));
        if constexpr (std::endian::native == std::endian::little)
            symbol_count = std::byteswap(symbol_count);

        auto symbol_table_header_offsets_bytes = symbol_table_blob.subspan(
            sizeof(symbol_count),
            symbol_count * sizeof(symbol_count)
        );
        auto symbol_table_header_offsets = std::span<uint32_t>{
            reinterpret_cast<uint32_t*>(symbol_table_header_offsets_bytes.data()),
            symbol_count
        };

        uint32_t strings_offset{};
        auto symbol_table_strings = symbol_table_blob.subspan(
            sizeof(symbol_count) + symbol_table_header_offsets.size_bytes()
        );

        for (auto symbol_offset : symbol_table_header_offsets) {
            // symbol offset is big endian, swap for little endian machines before
            // using.
            if constexpr (std::endian::native == std::endian::little)
                symbol_offset = std::byteswap(symbol_offset);

            auto symbol_name = symbol_table_strings.data() + strings_offset;
            // Advance strings_offset
            while (
                strings_offset < symbol_table_strings.size()
                and symbol_table_strings[strings_offset]
            ) ++strings_offset;
            // null terminator
            ++strings_offset;

            // fmt::print("`{}` header at offset 0x{:x}\n", symbol_name, symbol_offset);
            if (not std::ranges::contains(undefined, symbol_name))
                continue;

            // fmt::print("Found needed symbol `{}`\n", symbol_name);
            uarchive::FileHeader symbol_header{};
            memcpy(
                &symbol_header,
                blob.data() + symbol_offset,
                sizeof(symbol_header)
            );
            // fmt::print(
            //     "  In object at offset 0x{:x} `{}`\n",
            //     symbol_offset + sizeof(symbol_header),
            //     symbol_header.identifier()
            // );

            auto file_identifier = symbol_header.identifier();
            if (
                file_identifier.length() > 1
                and file_identifier.starts_with("/")
                and file_identifier.at(1) >= '0'
                and file_identifier.at(1) <= '9'
            ) {
                auto longname_offset = (uint32_t) strtoul(
                    file_identifier.substr(1).data(),
                    nullptr,
                    10
                );
                auto* const identifier_begin = blob.data()
                                             + string_table_data_offset
                                             + longname_offset;
                auto* identifier_it = identifier_begin;
                bool encountered_slash{false};
                // go until NULL character or "/\n".
                while (*identifier_it) {
                    if (encountered_slash and *identifier_it == '\n')
                        break;

                    encountered_slash = *identifier_it == '/';
                    ++identifier_it;
                }
                file_identifier = std::string_view{
                    identifier_begin,
                    (lcc::usz) (identifier_it - identifier_begin)
                };
                if (file_identifier.ends_with('/'))
                    file_identifier.remove_suffix(1);
            } else {
                while (file_identifier.ends_with(' '))
                    file_identifier.remove_suffix(1);
                if (file_identifier.ends_with('/'))
                    file_identifier.remove_suffix(1);
            }

            auto canonical_file_id = fmt::format("{}/{}", file_identifier, symbol_offset);

            // Only go on to extract this blob *if we haven't already*.
            if (std::ranges::contains(visited, canonical_file_id))
                continue;

            // TODO: verbose context
            // fmt::print(
            //     "clink: Extracted object `{}` from archive, satisfies `{}`\n",
            //     file_identifier,
            //     symbol_name
            // );

            // NOTE: May want to "canonicalize" this or whatever.
            visited.emplace_back(std::move(canonical_file_id));

            std::vector<char> object_blob{};
            object_blob.resize(symbol_header.size());
            memcpy(
                object_blob.data(),
                blob.data() + symbol_offset + sizeof(uarchive::FileHeader),
                symbol_header.size()
            );
            blobs.emplace_back(std::move(object_blob));
        }
    }

    return blobs;
}

} // namespace clink
