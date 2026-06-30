#include <lccbase/assert.hh>

#include <lccbase/diags.hh>

#include <string>
#include <utility>

// Exit due to assertion failure.
[[noreturn]]
void lcc::detail::AssertFail(std::string&& msg) {
    Diag::ICE("{}", std::move(msg));
}
