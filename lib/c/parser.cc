#include <c/parser.hh>

namespace cc = lcc::c;

auto cc::Parser::Parse(Context* context, File& file) -> TranslationUnit* {
    auto translation_unit = new TranslationUnit{&file};
    
    Parser parser{context, file};
    
    return translation_unit;
}
