#include <c/parser.hh>

namespace cc = lcc::c;

using Tk = cc::TokenKind;

auto cc::Parser::Parse(CContext* context, File& file) -> TranslationUnit* {
    auto translation_unit = new TranslationUnit{context, &file};
    
    Parser parser{translation_unit, file};
    
    return translation_unit;
}

auto cc::Parser::ParseTopLevel() -> Decl* {
    LCC_ASSERT(false, "TODO c top level");
}

auto cc::Parser::ParseType() -> Type* {
    auto location = token.location;
    if (Consume(Tk::Int)) {
        return new (*translation_unit()) IntType{location, IntegerKind::Int};
    }

    LCC_ASSERT(false, "TODO c type");
}
