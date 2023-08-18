#include <c/lexer.hh>

namespace cc = lcc::c;

void cc::Lexer::ReadTokenNoPreprocess(CToken& token) {
    token.kind = TokenKind::Invalid;
    token.location.len = 0;
    token.location.file_id = (u16) _file->file_id();
    token.text.clear();
    token.integer_value = 0;
    token.float_value = 0;
    token.artificial = false;
    token.integer_kind = LitIntegerKind::Int;
    token.float_kind = LitFloatKind::Double;
    token.macro_arg_index = -1;

    /*

    Things to be kept in mind when reading tokens:

    1.  Remove all comments as they are encountered:
        This is more formally "Replace all C comments with a single space".

        Comments are allowed to contain newlines that split a preprocessor directive
        accross multiple lines. If a preprocessor directive is being parsed and
        a comment takes the lexer to another line, it should not stop parsing the
        preprocessor directive. I don't think this applies to line comments.

        An exception to this rule is the #include directive with '<' and '>' delimited file names.
        This means specifically that a comment is not recognized while within the "quoted" portion.

    2.  Delete Backslash+Newline absolutely everywhere it is seen.
        This can likely be done in the AdvanceChar() routine to be handled silently and automatically.
        The one exception is if we ever allow trigraphs. If trigraphs are enabled, the do not
        parse if broken up by Backslash+Newline and will have to be handled separately.

        Note that the behavior of Backslash+Newline is unclean in string literals, but is standard.

    3.  Expand pre-defined macro names with their expansions (when not within a string or character literal).
        This one is self explanatory and will probably just *happen* when we decide what an identifier is.

    4.  Preprocessor directives need to be checked correctly.
        If a '#' is the first non-comment, non-whitespace character of a line, it must always start a
        preprocessor directive. The preprocessor directive name is never expanded and must be an identifier.

    5.  Unterminated comments in #include'd files report errors and do not carry over.
        This behavior is true of most of #include behavior, but I noted it here as a reminder that
        lexing should be very #include aware and stop parsing tokens at the end of an #include'd file.

        Include guards can be optimized by remembering which files are entirely contained within
        #ifndef/#endif and skipping them if the symbol is defined.

    6.  Parsing #define bodies does not invoke the macro expansion handler.
        Macro expansion does not happen in preprocessor defines. When a macro is expanded,
        only then is its body checked for more expansions. Notes will need to be taken on when and
        how exactly a macro is marked as not expandable during this expansion process.

    7.  Macro arguments are separated by commas.
        If ever you need a comma in your argument, it can only exist when enclosed in parenthesis.
        Other delimiters are ignored for this purpose, so square and curly braces do not affect
        this behavior.

        Macro argument lists must not have whitespace between the macro name and the opening parenthesis
        of the list of arguments, but using that macro does *not* require those spaces. I assume the
        easiest way to solve this is to look up an identifier, and only if it's a macro with arguments
        do we scan forward until a non-whitespace character is encountered to see if it's being expanded
        or not. If it is not, it does not expand at all and is used as a regular identifier.

    */

    token.location.pos = CurrentOffset();
}

void cc::Lexer::ReadToken(CToken& token) {
    EatWhitespace();
    if (IsAtStartOfLine() and CurrentChar() == '#') {
        HandlePreprocessorDirective();
        LCC_ASSERT(not IsInPreprocessor());
    }

    ReadTokenNoPreprocess(token);
}

void cc::Lexer::HandlePreprocessorDirective() {
    LCC_ASSERT(not IsInPreprocessor());
    LCC_ASSERT(IsAtStartOfLine() and CurrentChar() == '#');

    is_in_preprocessor = true;
    AdvanceChar(); // skip '#'

    CToken token;
    ReadTokenNoPreprocess(token);

    switch (token.kind) {
        default: {
        invalid_preprocessing_directive:;
            Error(token.location, "Invalid preprocessing directive");
            SkipToEndOfPreprocessorDirective(token);
        } break;

            // case TokenKind::LitInt: // line directive???

        case TokenKind::Ident: {
            goto invalid_preprocessing_directive;
            std::string ident_value = token.text;

            if (ident_value == "define")
                HandleDefineDirective(token);
            else goto invalid_preprocessing_directive;
        } break;
    }

    LCC_ASSERT(IsInPreprocessor());
    is_in_preprocessor = false;
}

void cc::Lexer::SkipToEndOfPreprocessorDirective(CToken current_token) {
    LCC_ASSERT(IsInPreprocessor());
    while (not IsAtEndOfFile() and current_token.kind != TokenKind::EndOfLine)
        ReadTokenNoPreprocess(current_token);
}

void cc::Lexer::HandleDefineDirective(const CToken& define_token) {
    LCC_ASSERT(IsInPreprocessor());
    LCC_ASSERT(define_token.kind == TokenKind::Ident and define_token.text == "define");

    CToken token;
    ReadTokenNoPreprocess(token);

    if (token.kind == TokenKind::EndOfLine) {
        Error(token.location, "Macro name missing");
        return; // we already hit the TokenKind::EndOfLine, nothing more to do
    }

    if (token.kind != TokenKind::Ident) {
        Error(token.location, "Macro name must be an identifier");
        SkipToEndOfPreprocessorDirective(token);
        return;
    }

    LCC_ASSERT(token.kind == TokenKind::Ident);
    std::string macro_name = token.text;

    bool macro_has_arguments = false;
    std::vector<std::string> macro_args{};
    std::vector<CToken> macro_body{};

    if (CurrentChar() == '(') {
        /// This is a macro with arguments.
        macro_has_arguments = true;
        AdvanceChar(); // skip the '('

        for (;;) {
            ReadTokenNoPreprocess(token);
            if (token.kind == TokenKind::EndOfLine or token.kind == TokenKind::EndOfFile) {
                Error(token.location, "Expected ')' in macro parameter list");
                SkipToEndOfPreprocessorDirective(token);
                return;
            }

            if (token.kind != TokenKind::Ident) {
                Error(token.location, "Invalid token in macro parameter list");
                SkipToEndOfPreprocessorDirective(token);
                return;
            }

            LCC_ASSERT(token.kind == TokenKind::Ident);
            macro_args.push_back(token.text);

            ReadTokenNoPreprocess(token);
            if (token.kind == TokenKind::CloseParen)
                break;
            else if (token.kind != TokenKind::Comma) {
                Error(token.location, "Expected comma in macro parameter list");
                SkipToEndOfPreprocessorDirective(token);
                return;
            }
        }
    }

    while (not IsAtEndOfFile()) {
        ReadTokenNoPreprocess(token);
        if (token.kind == TokenKind::EndOfLine)
            break;
        
        if (token.kind == TokenKind::Ident and macro_has_arguments) {
            LCC_ASSERT(token.macro_arg_index == -1);
            for (usz i = 0; i < macro_args.size(); i++) {
                if (macro_args[i] == token.text) {
                    token.macro_arg_index = (isz)i;
                    break;
                }
            }
        }

        macro_body.push_back(token);
    }

    MacroDef macro_def{
        macro_name,
        macro_has_arguments,
        std::move(macro_args),
        std::move(macro_body)};
    macro_defs.emplace(macro_name, macro_def);
}

void cc::Lexer::EatWhitespace() {
    while (not IsAtEndOfFile() and not IsSpace(CurrentChar()))
        AdvanceChar();
}
