#ifndef LOFF_ELF_HH
#define LOFF_ELF_HH

#include <filesystem>
#include <object/elf.h>
#include <object/generic.hh>

namespace lcc {
namespace elf {

// @return a pair with the first element being whether or not the header
// is valid, and the second a string being either one of "ok" if the
// header is valid, or a descriptive error message if it is not.
std::pair<bool, std::string> validate_header(const elf64_header&);

// Asserts that header is valid. Crashes with reason if it isn't.
// If you'd simply like to tell whether or not a header is valid and not
// assert that it is, try `validate_header()`
void verify_header(const elf64_header&);

Section get_section_from_blob(std::vector<char> blob, std::string_view name);
Section get_section_from_file(std::filesystem::path filepath, std::string_view name);

} // namespace elf
} // namespace lcc

#endif /* LOFF_ELF_HH */
