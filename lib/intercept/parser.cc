#include <intercept/parser.hh>

namespace lcc::intercept {
Module* Parser::Parse(Context* context, File& file) {
    auto result = new Module;
    result->file = &file;

    Parser parser(context, &file);

    return result;
}
} // namespace lcc::intercept
