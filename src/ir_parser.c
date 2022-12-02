#include <codegen/intermediate_representation.h>
#include <ir_parser.h>
#include <error.h>
#include <setjmp.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t usz;

/// ===========================================================================
///  Diagnostics
/// ===========================================================================
enum diagnostic_level {
    DIAG_NOTE,
    DIAG_WARN,
    DIAG_ERR,
    DIAG_ICE,
    DIAG_SORRY,
};


/// Source location.
typedef struct {
    u32 start;
    u32 end;
} loc;

/// Don’t call this function directly. Use the ISSUE_DIAGNOSTIC macro instead.
FORMAT(printf, 7, 8)
void issue_diagnostic_internal(
    const char *file,
    const char *function,
    int line,
    enum diagnostic_level level,
    const char *source,
    loc location,
    const char *fmt,
    ...) {
}

/// Issue a diagnostic.
#define ISSUE_DIAGNOSTIC(severity, source, location, ...) \
    issue_diagnostic_internal(                            \
        __FILE__,                                         \
        __PRETTY_FUNCTION__,                              \
        __LINE__,                                         \
        (severity),                                       \
        (source),                                         \
        (location),                                       \
        "\x01" __VA_ARGS__)

/// ===========================================================================
///  Lexer
/// ===========================================================================

/// Token types.
enum tk {
    tk_invalid,
    tk_eof,
    tk_newline,

    tk_ident,
    tk_temp,
    tk_register,
    tk_number,

    tk_comma,
    tk_colon,
    tk_lbrace,
    tk_lbrack,
    tk_lparen,
    tk_rbrace,
    tk_rbrack,
    tk_rparen,
    tk_assign,
};

/// String span.
typedef struct {
    const char *data;
    usz size;
} span;

/// Block in the symbol table.
typedef struct {
    span name;
    loc location;
    IRBlock *block;
} block_sym;

/// Temporary in the symbol table.
typedef struct {
    span name;
    loc location;
    IRInstruction *block;
} temp_sym;

/// IR parser context.
typedef struct {
    /// The type of the current token.
    enum tk tok_type;

    /// Whether the lexer should keep newlines.
    bool keep_newlines;

    /// The context for which to generate code.
    CodegenContext *context;

    /// The source code that we’re parsing.
    const char *source_start;
    const char *source_curr;

    /// The text that makes up the current token.
    span tok;

    /// Numeric value of the current token.
    i64 integer;

    /// The start/end positions of the current token.
    loc location;

    /// Symbol tables.
    VECTOR (block_sym) block_syms;
    VECTOR (temp_sym) temp_syms;

    /// Jump buffer. I don't feel like checking for errors
    /// all the time, and it’s not like we’re going to try
    /// to recover from errors anyway.
    jmp_buf jmp;
} IRParser;

#define IDENT(x) (p->tok.size == sizeof(x) - 1 && memcmp(p->tok.data, x, p->tok.size) == 0)
#define DO_ERR_AT(sev, location, ...)                                  \
    do {                                                               \
        ISSUE_DIAGNOSTIC(sev, p->source_start, location, __VA_ARGS__); \
        longjmp(p->jmp, 1);                                            \
    } while (0)

#define ERR_AT(location, ...) DO_ERR_AT(DIAG_ERR, location, __VA_ARGS__)
#define ERR(...) ERR_AT(p->location, __VA_ARGS__)
#define SORRY(...) DO_ERR_AT(DIAG_SORRY, p->location, __VA_ARGS__)

#define WARN(...) do { \
    ISSUE_DIAGNOSTIC(DIAG_WARN, p->source_start, p->start_pos, p->end_pos, __VA_ARGS__); \
} while (0)

/// Check if a span is the name of an instruction.
/// Returns IR_COUNT if the span is not the name of an instruction.
enum IRType irtype_from_span(span s) {
    TODO();
}

/// Get the next token.
void next_token(IRParser *p) {
    TODO();
}

/// ===========================================================================
///  Parser
/// ===========================================================================

/// Add a temporary to the current symbol table.
bool make_temporary(IRParser *p, loc location, span name, IRInstruction *temp) {
    TODO();
}

/// Add a block to the current symbol table.
bool make_block(IRParser *p, loc location, span name, IRBlock *block) {
    TODO();
}

/// This function handles the bulk of the parsing.
///
/// It returns true if the parsed instruction was a branch, and false otherwise.
///
/// <instruction>       ::= [ <temp> [ ":" <register> ] "=" ] <value-instruction> "\n"
///                       | <void-instruction> "\n"
/// <value-instruction> ::= NUMBER
///                       | CALL ( <name> | <temp> ) "(" <parameters> ")"
///                       | PHI { "[" <name> ":" <temp> "]" }
///                       | <unary> <temp>
///                       | <binary> <temp> "," <temp>
///                       | LOAD ( <temp> | <name> )
///                       | REGISTER <register>
///                       | ALLOCA
/// <void-instruction>  ::= STORE <temp> "," ( <temp> | <name> )
///
///
/// <unary>  ::= COPY
/// <binary> ::= ADD | SUB | MUL | DIV | MOD | EQ | NE | LT | LE | GT | GE | SHL | SHR | SAR
///
/// <branch>    ::= UNREACHABLE "\n"
///               | RETURN "\n"
///               | RETURN <temp> "\n"
///               | BR <name> "\n"
///               | BR-COND <temp> "," <name> "," <name> "\n"
static bool parse_instruction_or_branch() {
}


/// <block-body>  ::= <instruction>* <branch>
static void parse_block_body(IRParser *p) {
    for (;;) {
        /// An rbrace is not allowed here since we haven’t seen the branch yet.
        if (p->tok_type == tk_rbrace) {
            loc current_block_location;
            if (!p->block_syms.size) current_block_location = p->location;
            else current_block_location = p->block_syms.data[p->block_syms.size - 1].location;
            ERR_AT(current_block_location, "Missing branch in block");
        }

        /// Parse the next instruction or branch.
        if (parse_instruction_or_branch()) break;
    }
}

/// <block>       ::= <name> ":" <block-body>
static void parse_block(IRParser *p) {
    /// Parse the block name.
    if (p->tok_type != tk_ident) ERR("Expected block");
    make_block(p, p->tok, VECTOR_BACK(*p->context->functions)->blocks.first);
    next_token(p);
    if (p->tok_type != tk_colon) ERR("expected ':' after block name");

    /// From now on, we care about newlines.
    next_token(p);

    /// Parse the body.
    p->keep_newlines = true;
    parse_block_body(p);
    p->keep_newlines = false;
}

/// <body> ::= <first-block> <block>*
/// <first-block> ::= ( <name> ":" ) <block-body>
static void parse_body(IRParser *p) {
    /// The first block is special, because it can be unnamed.
    if (p->tok_type == tk_ident && lookahead(p, 1)->tok_type == tk_colon) {
        make_block(p, p->tok, VECTOR_BACK(*p->context->functions)->blocks.first);
        next_token(p);
        next_token(p);

        /// Parse the body of the first block.
        parse_block_body(p);

        /// Parse the remaining blocks.
        while (p->tok_type != tk_rbrace) parse_block(p);
        return;
    }

    /// If there is no unnamed first block, there must still be at least one block.
    do parse_block(p);
    while (p->tok_type != tk_rbrace);
}

/// <attributes> ::= <attribute>*
/// <attribute>  ::= CONSTEVAL | FORCEINLINE | GLOBAL | NORETURN | PURE | LEAF
static void parse_attributes(IRParser *p) {
    IRFunction *f = VECTOR_BACK(*p->context->functions);

#define ATTR(a)                                                        \
    if (IDENT(#a)) {                                                   \
        if (f->CAT(attr_, a)) WARN("Duplicate 'consteval' attribute"); \
        f->CAT(attr_, a) = true;                                       \
        next_token(p);                                                 \
        continue;                                                      \
    }

    while (p->tok_type == tk_ident) {
        ATTR(consteval)
        ATTR(forceinline)
        ATTR(global)
        ATTR(noreturn)
        ATTR(pure)
        ATTR(leaf)
        ERR("Unknown attribute '%.*s'", (int) p->tok.size, p->tok.data);
    }

#undef ATTR
}

/// <parameters> ::= "(" [ <temp> { "," <temp> } ] ")"
static void parse_parameters(IRParser *p) {
    int64_t param_count = 0;

    /// Parameter list.
    if (p->tok_type != tk_lparen) ERR("Expected '(' after function name");
    next_token(p);

    /// Parse the parameters.
    while (p->tok_type != tk_rparen) {
        /// Create a parameter reference.
        if (p->tok_type != tk_temp) ERR("Expected temporary after '(' or ','");
        make_temporary(p, p->tok, ir_parameter_reference(p->context, param_count++));
        next_token(p);

        /// Yeet the comma if there is one.
        if (p->tok_type != tk_comma) break;
        next_token(p);
    }

    /// Yeet ")".
    if (p->tok_type != tk_rparen) ERR("Expected ')' after function parameters");
    next_token(p);
}

/// <extern> ::= DECLARE <name> <parameters> <attributes> "\n"
static void parse_extern(IRParser *p) {
    SORRY("Parsing extern functions is currently not supported");
}

/// <function> ::= DEFUN <name> <parameters> <attributes> "{" <body> "}"
static bool parse_function(IRParser *p) {
    next_token(p); /// Yeet 'defun'.

    /// Function name.
    if (p->tok_type != tk_ident) ERR("Expected function name after 'defun'");
    IRFunction *f = ir_function(p->context, strndup(p->tok.data, p->tok.size));
    VECTOR_PUSH(*p->context->functions, f);
    next_token(p);

    /// Function parameters.
    parse_parameters(p);

    /// Function attributes.
    parse_attributes(p);

    /// Yeet "{".
    if (p->tok_type != tk_lbrace) {
        if (p->tok_type == tk_newline) {
            ERR("Expected '{' after function attributes. Hint: use 'declare' to declare an extern function.");
        } else {
            ERR("Expected '{' in function definition");
        }
    }
    next_token(p);

    /// Parse the function body.
    parse_body(p);

    /// Yeet "}".
    if (p->tok_type != tk_rbrace) ERR("Expected '}' after function body");
    next_token(p);
}

/// <ir> ::= { <function> | <extern> }
/// TODO: All parse functions may leak memory if there is a parse error.
///       Do we care about that? (If this ever ends up in a library, we should).
static bool parse_ir(IRParser *p) {
    if (setjmp(p->jmp)) return false;
    for (;;) {
        /// Parse a top-level declaration.
        switch (p->tok_type) {
            case tk_ident:
                if (IDENT("defun")) parse_function(p);
                else if (IDENT("declare")) parse_extern(p);
                else ERR("Expected 'defun' or 'declare'");
            case tk_eof: return false;
            default: ERR("Expected 'defun' or 'declare'");
        }
    }
    return true;
}

bool ir_parse(CodegenContext *context, const char *ir, size_t sz) {
}