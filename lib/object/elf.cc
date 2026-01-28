#include <object/elf.h>
#include <object/elf.hh>
#include <object/generic.hh>

#include <fmt/format.h>
#include <lcc/file.hh>
#include <lcc/utils.hh>

#include <filesystem>
#include <string>
#include <string_view>
#include <utility>

namespace lcc::elf {

auto validate_header(const elf64_header& hdr) -> std::pair<bool, std::string> {
    if (hdr.e_ident[EI_MAG0] != 0x7f
        or hdr.e_ident[EI_MAG1] != 'E'
        or hdr.e_ident[EI_MAG2] != 'L'
        or hdr.e_ident[EI_MAG3] != 'F') {
        return {
            false,
            fmt::format(
                "Invalid ELF magic bytes. Expected 0x7f, 'E', 'L', 'F', but got 0x{:x}, '{}', '{}', '{}'",
                hdr.e_ident[EI_MAG0],
                char(hdr.e_ident[EI_MAG1]),
                char(hdr.e_ident[EI_MAG2]),
                char(hdr.e_ident[EI_MAG3])
            )
        };
    }

    if (hdr.e_ident[EI_VERSION] != 1) {
        return {
            false,
            fmt::format(
                "Invalid ELF version. Expected 1, but got {}",
                hdr.e_ident[EI_VERSION]
            )
        };
    }

    if (hdr.e_ehsize != sizeof(elf64_header)) {
        return {
            false,
            fmt::format(
                "Invalid ELF header size. Expected {}, but got {}",
                sizeof(elf64_header),
                hdr.e_ehsize
            )
        };
    }

    if (hdr.e_shentsize != sizeof(elf64_shdr)) {
        return {
            false,
            fmt::format(
                "Invalid ELF section header table entry size. Expected {}, but got {}",
                sizeof(elf64_shdr),
                hdr.e_shentsize
            )
        };
    }

    return {true, "ok"};
}

void verify_header(const elf64_header& hdr) {
    auto pair = validate_header(hdr);
    LCC_ASSERT(
        pair.first,
        "{}",
        pair.second
    );

    LCC_ASSERT(
        hdr.e_ident[EI_MAG0] == 0x7f
            and hdr.e_ident[EI_MAG1] == 'E'
            and hdr.e_ident[EI_MAG2] == 'L'
            and hdr.e_ident[EI_MAG3] == 'F',
        "Invalid ELF magic bytes. Expected 0x7f, 'E', 'L', 'F', but got 0x{:x}, '{}', '{}', '{}'",
        hdr.e_ident[EI_MAG0],
        char(hdr.e_ident[EI_MAG1]),
        char(hdr.e_ident[EI_MAG2]),
        char(hdr.e_ident[EI_MAG3])
    );

    LCC_ASSERT(
        hdr.e_ident[EI_VERSION] == 1,
        "Invalid ELF version. Expected 1, but got {}",
        hdr.e_ident[EI_VERSION]
    );

    LCC_ASSERT(
        hdr.e_ehsize == sizeof(elf64_header),
        "Invalid ELF header size. Expected {}, but got {}",
        sizeof(elf64_header),
        hdr.e_ehsize
    );

    LCC_ASSERT(
        hdr.e_shentsize == sizeof(elf64_shdr),
        "Invalid ELF section header table entry size. Expected {}, but got {}",
        sizeof(elf64_shdr),
        hdr.e_shentsize
    );
}

auto get_section_from_blob(std::vector<char> blob, std::string_view name) -> Section {
    LCC_ASSERT(
        blob.size() >= sizeof(elf64_header),
        "Cannot get section {} from ELF binary blob as it is too small to even contain the header",
        name
    );

    elf64_header hdr{};
    std::memcpy(&hdr, blob.data(), sizeof(elf64_header));

    verify_header(hdr);

    Section out{};
    out.name = name;

    auto section_headers_offset = hdr.e_shoff;
    const auto* section_headers_base = reinterpret_cast<const elf64_shdr*>(
        blob.data() + section_headers_offset
    );
    auto section_header_count = hdr.e_shnum;

    // First grab reference to section header name section.
    // Usually `.strtab` or `.shstrtab`.
    const auto* shstrtab_hdr = section_headers_base + hdr.e_shstrndx;
    auto* shstrtab = blob.data() + shstrtab_hdr->sh_offset;
    // Now try to find section with the given name.
    for (decltype(section_header_count) i = 0; i < section_header_count; ++i) {
        const auto* section_header = section_headers_base + i;
        std::string_view section_name{shstrtab + section_header->sh_name};
        if (section_name == name) {
            auto begin = blob.begin() + isz(section_header->sh_offset);
            auto end = begin + isz(section_header->sh_size);
            out.contents().insert(out.contents().end(), begin, end);
            break;
        }
    }
    return out;
}

// NOTE: Asserts false if anything goes wrong (file can't open, section not there, etc)
auto get_section_from_file(const fs::path& filepath, std::string_view name) -> Section {
    LCC_ASSERT(
        std::filesystem::exists(filepath),
        "Cannot get section from ELF file that does not exist: {}",
        filepath.string()
    );

    const auto contents = File::Read(filepath);
    return get_section_from_blob(contents, name);
}

} // namespace lcc::elf
