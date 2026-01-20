#ifndef LOFF_ELF_HH
#define LOFF_ELF_HH

#include <lcc/utils.hh>
#include <object/elf.h>
#include <object/generic.hh>

#include <filesystem>
#include <string_view>
#include <utility>
#include <vector>

namespace lcc::elf {

// @return a pair with the first element being whether or not the header
// is valid, and the second a string being either one of "ok" if the
// header is valid, or a descriptive error message if it is not.
auto validate_header(const elf64_header&) -> std::pair<bool, std::string>;

// Asserts that header is valid. Crashes with reason if it isn't.
// If you'd simply like to tell whether or not a header is valid and not
// assert that it is, try `validate_header()`
void verify_header(const elf64_header&);

auto get_section_from_blob(std::vector<char> blob, std::string_view name) -> Section;
auto get_section_from_file(const fs::path& filepath, std::string_view name) -> Section;

} // namespace lcc::elf

#endif /* LOFF_ELF_HH */
