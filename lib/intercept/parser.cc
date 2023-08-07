#include <intercept/parser.hh>

namespace lcc::intercept {
Module* Parser::Parse(Context* context, File& file) {
    auto result = new Module;
    result->file = &file;

    Parser parser(&file);

    return result;
}

void Lexer::NextToken() {
}
} // namespace lcc::intercept
