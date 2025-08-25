#ifndef LCC_UTF8_HH
#define LCC_UTF8_HH

#include <lcc/utils.hh>

#include <string>

namespace lcc::utf8 {
auto ToString(u32 codepoint) -> std::string {
    std::string out;
    if (codepoint <= 0x7f) {
        out += (char) codepoint;
    } else if (codepoint <= 0x7ff) {
        u8 b0 = (codepoint >> 6U) & 0b0001'1111;
        u8 b1 = codepoint & 0b0011'1111;
        out += char(b0 | 0b1100'0000);
        out += char(b1 | 0b1000'0000);
    } else if (codepoint <= 0xffff) {
        u8 b0 = (codepoint >> 12U) & 0b1111;
        u8 b1 = (codepoint >> 6U) & 0b0011'1111;
        u8 b2 = codepoint & 0b0011'1111;
        out += char(b0 | 0b1110'0000);
        out += char(b1 | 0b1000'0000);
        out += char(b2 | 0b1000'0000);
    } else if (codepoint <= 0x10ffff) {
        u8 b0 = (codepoint >> 18U) & 0b111;
        u8 b1 = (codepoint >> 12U) & 0b0011'1111;
        u8 b2 = (codepoint >> 6U) & 0b0011'1111;
        u8 b3 = codepoint & 0b0011'1111;
        out += char(b0 | 0b1111'0000);
        out += char(b1 | 0b1000'0000);
        out += char(b2 | 0b1000'0000);
        out += char(b3 | 0b1000'0000);
    } else LCC_ASSERT(false, "Over-large utf codepoint (>0x10ffff)");

    return out;
}
} // namespace lcc::utf8

#endif /* LCC_UTF8_HH */
