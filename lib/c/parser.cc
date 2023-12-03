#include <c/ast.hh>
#include <c/parser.hh>

namespace cc = lcc::c;

using Tk = cc::TokenKind;

auto cc::Parser::Parse(CContext* context, File& file) -> TranslationUnit* {
    auto translation_unit = new TranslationUnit{context, &file};

    Parser parser{translation_unit, file};
    parser.ParseTopLevel();

    return translation_unit;
}

void cc::Parser::ParseTopLevel() {
    /// Parse the file.
    for (;;) {
        if (Consume(Tk::SemiColon)) continue;

        /// Stop if we’re at end of file.
        if (At(Tk::EndOfFile)) break;

        /// Parse a top-level declaration statement.
        auto statement = ParseStatement();
        if (not Consume(Tk::SemiColon)) {
            if (At(Tk::EndOfFile)) {
                Error("Expected ';' but got end of file");
            } else if (statement) {
                Location location{};
                if (statement.value()->location().is_valid())
                    location = statement.value()->location();

                // TODO: Attempt to get the location that is as close to where the semi-
                // colon should be (init of decl, etc).

                // Limit location to length of one, discarding the beginning (fold right).
                if (location.len > 1) {
                    location.pos += location.len - 1;
                    location.len = 1;
                }

                Error(location, "Expected ';'")
                    .attach(false, Diag::Note(lcc_context(), token.location, "Before this"));
            }
        }
        if (statement) translation_unit()->add_top_level_decl(statement.value());
        /// TODO: Synchronise on semicolons and braces in case of an error.
        // else Synchronise();
    }
}

auto cc::Parser::ParseStatement() -> Result<Decl*> {
    // auto* t = ParseType();
    // std::string id = token.text;

    LCC_ASSERT(false, "TODO c statement");
}

auto cc::Parser::ParseType() -> Type* {
    auto location = token.location;
    if (Consume(Tk::Int)) {
        return new (*translation_unit()) IntType{location, IntegerKind::Int};
    }
    if (Consume(Tk::Void)) {
        return new (*translation_unit()) VoidType{location};
    }

    LCC_ASSERT(false, "TODO c type");
}
