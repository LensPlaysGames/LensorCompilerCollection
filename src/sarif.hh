#ifndef LCC_DRIVER_SARIF_HH
#define LCC_DRIVER_SARIF_HH

#include <lccbase/context.hh>

#include <lccjson/lccjson.hh>

#include <string>
#include <string_view>

std::string as_sarif(lcc::Context& ctx, std::string_view command_line);

#endif /* LCC_DRIVER_SARIF_HH */
