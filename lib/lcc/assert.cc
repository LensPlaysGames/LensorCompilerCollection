#include <lcc/assert.hh>
#include <lcc/diags.hh>

#include <string>
#include <utility>

// Exit due to assertion failure.
[[noreturn]]
void lcc::detail::AssertFail(std::string&& msg) {
    Diag::ICE("{}", std::move(msg));
}
