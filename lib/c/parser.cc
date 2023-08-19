#include <c/parser.hh>

namespace cc = lcc::c;

auto cc::Parser::Parse(CContext* context, File& file) -> TranslationUnit* {
    auto translation_unit = new TranslationUnit{context, &file};
    
    Parser parser{context, file};
    
    return translation_unit;
}
