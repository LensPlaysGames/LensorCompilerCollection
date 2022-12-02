#include <codegen/intermediate_representation.h>
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <ir_parser.h>
#include <math.h>
#include <setjmp.h>

/// For isatty().
#ifdef _WIN32
#    include <io.h>
#    define isatty _isatty
#else
#    include <stdarg.h>
#    include <unistd.h>
#endif

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

    DIAG_COUNT,
};

/// Source location.
typedef struct {
    u32 start;
    u32 end;
} loc;

static const char *diagnostic_level_names[DIAG_COUNT] = {
    "Note",
    "Warning",
    "Error",
    "Internal Compiler Error",
    "Sorry, unimplemented",
};

static const char *diagnostic_level_colours[DIAG_COUNT] = {
    "\033[1;34m",
    "\033[1;36m",
    "\033[1;31m",
    "\033[1;31m",
    "\033[1;31m",
};

/// Issue a compiler diagnostic.
///
/// WARNING: ALTER THIS FUNCTION AT YOUR OWN PERIL.
///
/// This function is the culmination of many, *many* hours of battling
/// off-by-one errors and banging my head against the nearest wall. It
/// was copied from a previous project because I tried reproducing it
/// at first, but failed horribly. – Sirraide.
FORMAT(printf, 5, 6)
void issue_diagnostic(
    /// Error level and source location.
    enum diagnostic_level level,
    const char *filename,
    span source,
    loc location,

    /// The actual error message.
    const char *fmt,
    ...) {
    ASSERT(level >= 0 && level < DIAG_COUNT);

    /// Check if stderr is a terminal.
    bool is_terminal = isatty(fileno(stderr));

    if (location.start > source.size) location.start = (u32) (source.size);
    if (location.end > source.size) location.end = (u32) (source.size);

    /// Seek to the start of the line. Keep track of the line number.
    u32 line = 1;
    u32 line_start = 0;
    for (u32 i = location.start; i > 0; --i) {
        if (source.data[i] == '\n') {
            if (!line_start) line_start = i + 1;
            ++line;
        }
    }

    /// Don’t include the newline in the line.
    if (source.data[line_start] == '\n') ++line_start;

    /// Seek to the end of the line.
    u32 line_end = location.end;
    while (line_end < source.size && source.data[line_end] != '\n') line_end++;

    /// Print the filename, line and column, severity and message.
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
    fprintf(stderr, "%s:%u:%u: ", filename, line, location.start - line_start);
    if (is_terminal) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    fprintf(stderr, "%s: ", diagnostic_level_names[level]);
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    if (is_terminal) fprintf(stderr, "\033[m");

    /// Print the line.
    fprintf(stderr, "\n %d | ", line);
    for (u32 i = line_start; i < location.start; ++i) {
        if (source.data[i] == '\t') fprintf(stderr, "    ");
        else fputc(source.data[i], stderr);
    }
    if (is_terminal) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    for (u32 i = location.start; i < location.end; ++i) {
        if (source.data[i] == '\t') fprintf(stderr, "    ");
        else fputc(source.data[i], stderr);
    }
    if (is_terminal) fprintf(stderr, "\033[m");
    for (u32 i = location.end; i < line_end; ++i) {
        if (source.data[i] == '\t') fprintf(stderr, "    ");
        else fputc(source.data[i], stderr);
    }
    fprintf(stderr, "\n");

    /// Underline the region with tildes.
    size_t spaces = !line ? 1 : (u32) (log10(line) + 1);
    for (size_t i = 0; i < spaces; i++)
        fprintf(stderr, " ");
    fprintf(stderr, "  | %s", is_terminal ? diagnostic_level_colours[level] : "");
    for (u32 i = line_start; i < location.start; ++i) {
        if (source.data[i] == '\t') fprintf(stderr, "    ");
        else fputc(' ', stderr);
    }
    for (u32 i = location.start; i < location.end; ++i) {
        if (source.data[i] == '\t') fprintf(stderr, "~~~~");
        else fputc('~', stderr);
    }
    if (is_terminal) fprintf(stderr, "\033[m\n");
}

/// ===========================================================================
///  Lexer
/// ===========================================================================

/// Token types.
enum tk {
    tk_invalid,
    tk_eof,
    tk_newline = '\n',
    tk_comma = ',',
    tk_colon = ':',
    tk_lbrace = '{',
    tk_lbrack = '[',
    tk_lparen = '(',
    tk_rbrace = '}',
    tk_rbrack = ']',
    tk_rparen = ')',
    tk_assign = '=',

    tk_ident,
    tk_temp,
    tk_number,
};

#define DEFINE_IR_INSTRUCTION_TYPE(type) type,
enum IrParserInstructionType {
    ALL_IR_INSTRUCTION_TYPES(DEFINE_IR_INSTRUCTION_TYPE)

    /// Used to check if we’ve handled all instruction
    /// types and as the invalid instruction type.
    IR_INSTRUCTIONS_COUNT,

    /// These are only used internally by the parser.
    TAIL,
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
};
#undef DEFINE_IR_INSTRUCTION_TYPE

/// Block in the symbol table.
typedef struct {
    span name;
    loc location;
    IRBlock *block;
    VECTOR(IRBlock **) unresolved;
} block_sym;

/// Temporary in the symbol table.
/// The sigil ('%' or '#') is included in the name.
typedef struct {
    span name;
    loc location;
    IRInstruction *instruction;
    VECTOR(IRInstruction **) unresolved;
} temp_sym;

/// Function in the symbol table.
typedef struct {
    span name;
    loc location;
    IRFunction *function;
    VECTOR(const char **) unresolved;
} function_sym;

/// IR parser context.
typedef struct {
    /// The type of the current token.
    enum tk tok_type;

    /// The last character read.
    char lastc;

    /// Whether the lexer should keep newlines.
    bool keep_newlines;

    /// The context for which to generate code.
    CodegenContext *context;

    /// The name of the file that we’re parsing.
    const char *filename;

    /// The source code that we’re parsing.
    span source;
    const char *source_start;
    const char *source_end;
    const char *source_curr;

    /// The text that makes up the current token.
    span tok;

    /// Numeric value of the current token.
    i64 integer;

    /// Symbol tables.
    VECTOR(block_sym) block_syms;
    VECTOR(temp_sym) temp_syms;
    VECTOR(function_sym) function_syms;

    /// Jump buffer. I don't feel like checking for errors
    /// all the time, and it’s not like we’re going to try
    /// to recover from errors anyway.
    jmp_buf jmp;
} IRParser;

#define IDENT(x) (p->tok.size == sizeof(x) - 1 && memcmp(p->tok.data, x, p->tok.size) == 0)
#define DO_ERR_AT(sev, location, ...)                                         \
    do {                                                                      \
        issue_diagnostic(sev, p->filename, p->source, location, __VA_ARGS__); \
        longjmp(p->jmp, 1);                                                   \
    } while (0)

#define ERR_AT(location, ...) DO_ERR_AT(DIAG_ERR, location, __VA_ARGS__)
#define ERR(...)              ERR_AT(here(p), __VA_ARGS__)
#define SORRY(...)            DO_ERR_AT(DIAG_SORRY, here(p), __VA_ARGS__)

#define WARN(...)                                                                  \
    do {                                                                           \
        issue_diagnostic(DIAG_WARN, p->filename, p->source, here(p), __VA_ARGS__); \
    } while (0)

/// Check if two spans are equal.
static FORCEINLINE bool spans_equal(span a, span b) {
    return a.size == b.size && memcmp(a.data, b.data, a.size) == 0;
}

/// Check if a span is the name of an instruction.
/// Returns IR_COUNT if the span is not the name of an instruction.
static enum IrParserInstructionType irtype_from_span(span s) {
    STATIC_ASSERT((int) IR_INSTRUCTIONS_COUNT == (int) IR_COUNT, "IR Parser must implement all IR instructions");

#define TYPE(string, type)                                                             \
    static const span CAT(span_, type) = {.data = string, .size = sizeof(string) - 1}; \
    if (spans_equal(s, CAT(span_, type))) return type;
    TYPE("tail", TAIL)
    TYPE("call", CALL)
    TYPE("phi", PHI)
    TYPE("copy", COPY)
    TYPE("imm", IMMEDIATE)
    TYPE("add", ADD)
    TYPE("sub", SUBTRACT)
    TYPE("mul", MULTIPLY)
    TYPE("div", DIVIDE)
    TYPE("mod", MODULO)
    TYPE("eq", EQ)
    TYPE("ne", NE)
    TYPE("lt", LT)
    TYPE("le", LE)
    TYPE("gt", GT)
    TYPE("ge", GE)
    TYPE("shl", SHIFT_LEFT)
    TYPE("shr", SHIFT_RIGHT_LOGICAL)
    TYPE("sar", SHIFT_RIGHT_ARITHMETIC)
    TYPE("alloca", STACK_ALLOCATE)
    TYPE("load", LOAD)
    TYPE("store", STORE)
    TYPE("register", REGISTER)
    TYPE("unreachable", UNREACHABLE)
    TYPE("ret", RETURN)
    TYPE("br", BRANCH)
    TYPE("br.cond", BRANCH_CONDITIONAL)
#undef TYPE

    return IR_INSTRUCTIONS_COUNT;
}

/// Check if a character is a delimiter.
static bool is_delim(char c) {
    return c == ',' || c == ':' || c == '=' || //
           c == '}' || c == ']' || c == ')' || //
           c == '{' || c == '[' || c == '(' || //
           c == ';';
}

static loc here(IRParser *p) {
    u32 start = (u32) (p->tok.data - p->source_start);
    return (loc){.start = (u32) start, .end = start + (u32) p->tok.size};
}

/// Lex the next character.
static void next_char(IRParser *p) {
    /// Keep returning EOF once EOF has been reached.
    if (p->source_curr >= p->source_end) {
        p->lastc = 0;
        return;
    }

    /// Read the next character.
    p->lastc = *p->source_curr++;

    /// Handle newlines.
    if (p->lastc == '\r' || p->lastc == '\n') {
        /// We never return '\r'.
        p->lastc = '\n';

        /// Last character.
        if (p->source_curr >= p->source_end) return;

        /// Replace CRLF and LFCR with a single newline.
        char c = *p->source_curr;
        if ((c == '\r' || c == '\n') && c != p->lastc) p->source_curr++;
    }
}

/// Lex an identifier.
static void next_identifier(IRParser *p) {
    while (isalnum(p->lastc) || p->lastc == '_' || p->lastc == '.' || p->lastc == '%') {
        p->tok.size++;
        next_char(p);
    }

    /// Just '%' is not a valid identifier.
    if (p->tok.size == 1 && p->tok.data[0] == '%') ERR("Invalid name for temporary");
}

/// Parse a number.
static void parse_number(IRParser *p, int base) {
    char *end;
    errno = 0;
    p->integer = (i64) strtoll(p->tok.data, &end, base);
    if (errno == ERANGE) WARN("Integer literal out of range");
    if (end != p->tok.data + p->tok.size) ERR("Invalid integer literal");
}

/// Lex a number.
static void next_number(IRParser *p) {
    /// Discard leading zeroes.
    while (p->lastc == '0') {
        p->tok.size++;
        next_char(p);
    }

    /// Binary.
    if (p->lastc == 'b' || p->lastc == 'B') {
        next_char(p);
        while (p->lastc == '0' || p->lastc == '1') {
            p->tok.size++;
            next_char(p);
        }
        return parse_number(p, 2);
    }

    /// Octal.
    else if (p->lastc == 'o' || p->lastc == 'O') {
        next_char(p);
        while (p->lastc >= '0' && p->lastc <= '7') {
            p->tok.size++;
            next_char(p);
        }
        return parse_number(p, 8);
    }

    /// Hexadecimal.
    else if (p->lastc == 'x' || p->lastc == 'X') {
        next_char(p);
        while (isxdigit(p->lastc)) {
            p->tok.size++;
            next_char(p);
        }
        return parse_number(p, 16);
    }

    /// Some people might think that a leading zero is an octal number.
    /// To prevent bugs, we simply do not permit leading zeroes.
    if (p->tok.size > 1 || (p->tok.size && isdigit(p->lastc)))
        ERR("Invalid integer literal. For octal numbers, use the 0o prefix.");

    /// Any other digit means we have a decimal number.
    if (isdigit(p->lastc)) {
        do {
            p->tok.size++;
            next_char(p);
        } while (isdigit(p->lastc));
        return parse_number(p, 10);
    }

    /// If the next character is a space or delimiter, then this is a literal 0.
    if (isspace(p->lastc) || is_delim(p->lastc)) return;

    /// Anything else is an error.
    ERR("Invalid integer literal");
}

/// Lex the next token.
static void next_token(IRParser *p) {
    /// Keep returning EOF once EOF has been reached.
    if (!p->lastc) {
        p->tok_type = tk_eof;
        return;
    }

    /// Set the token to invalid in case there is an error.
    p->tok_type = tk_invalid;

    /// Skip whitespace.
    while (isspace(p->lastc) && (p->lastc != '\n' || !p->keep_newlines)) next_char(p);

    /// Start of the token.
    p->tok.data = p->source_curr - 1;

    /// Lex the token.
    switch (p->lastc) {
        /// EOF.
        case 0:
            p->tok_type = tk_eof;
            break;

        /// Single-character tokens.
        case '\n':
        case ',':
        case ':':
        case '{':
        case '}':
        case '(':
        case ')':
        case '[':
        case ']':
        case '=':
            p->tok_type = (enum tk) p->lastc;
            p->tok.size = 1;
            next_char(p);
            break;

        /// Comment.
        case ';':
            while (p->lastc && p->lastc != '\n') next_char(p);
            next_token(p);
            return;

        /// Temporary or identifier.
        case '%':
            p->tok_type = tk_temp;
            p->tok.size = 1;
            next_char(p);
            next_identifier(p);
            break;

        case '-':
            p->tok.size = 1;
            next_char(p);
            if (!isdigit(p->lastc)) ERR("Invalid integer literal");
            goto negative;

        /// Number or identifier.
        default:
            /// Identifier.
            if (isalpha(p->lastc) || p->lastc == '_') {
                p->tok.size = 0;
                p->tok_type = tk_ident;
                next_identifier(p);
                break;
            }

            /// Number.
            if (isdigit(p->lastc)) {
                p->tok.size = 0;
            negative:
                p->tok_type = tk_number;
                p->integer = 0;
                next_number(p);
                break;
            }

            /// Anything else is invalid.
            ERR("Unknown token");
    }
}

/// ===========================================================================
///  Parser
/// ===========================================================================
#define MAKE(thing_name, type, param_type, field)                                                           \
    /** Add a thing to the current symbol table. **/                                                        \
    static void CAT(make_, type)(IRParser * p, loc location, span name, param_type param) {                 \
        /** Issue an error if the thing already exists. **/                                                 \
        CAT(type, _sym) * index;                                                                            \
        VECTOR_FIND_IF(p->CAT(type, _syms), index, i, spans_equal(p->CAT(type, _syms).data[i].name, name)); \
        if (index) {                                                                                        \
            if (index->field) ERR("Redefinition of " thing_name " '%.*s'", (int) name.size, name.data);     \
            index->field = param;                                                                           \
            return;                                                                                         \
        }                                                                                                   \
                                                                                                            \
        /** Add the thing to the symbol table. **/                                                          \
        CAT(type, _sym)                                                                                     \
        sym = {                                                                                             \
            .name = name,                                                                                   \
            .field = param,                                                                                 \
            .location = location,                                                                           \
            .unresolved = {0}};                                                                             \
        VECTOR_PUSH(p->CAT(type, _syms), sym);                                                              \
    }

MAKE("temporary", temp, IRInstruction *, instruction)
MAKE("block", block, IRBlock *, block)
MAKE("function", function, IRFunction *, function)

/// Find a temporary in the current parser context.
static IRInstruction *try_resolve_temp(IRParser *p, span name) {
    temp_sym *inst;
    VECTOR_FIND_IF(p->temp_syms, inst, i, spans_equal(p->temp_syms.data[i].name, name));
    return inst ? inst->instruction : NULL;
}

/// Find a temporary in the current parser context. If the temporary
/// is not found, an error is raised.
static IRInstruction *resolve_temp(IRParser *p, loc location, span name) {
    IRInstruction *inst = try_resolve_temp(p, name);
    if (!inst) ERR_AT(location, "Unknown temporary '%.*s'", (int) name.size, name.data);
    return inst;
}

/// Resolve the current token as a temporary.
static IRInstruction *resolve_curr_temp(IRParser *p) {
    return resolve_temp(p, here(p), p->tok);
}

#define RESOLVE_OR_DECLARE(type, param_type, null_field_name, assign_field_name)                           \
    static void CAT(resolve_or_declare_, type)(IRParser * p, loc location, span name, param_type * user) { \
        /** Try to find the thing. **/                                                                     \
        CAT(type, _sym) * ptr;                                                                             \
        VECTOR_FIND_IF(p->CAT(type, _syms), ptr, i, spans_equal(p->CAT(type, _syms).data[i].name, name));  \
                                                                                                           \
        /** If the thing does not exist yet, add it. **/                                                   \
        if (!ptr) {                                                                                        \
            CAT(type, _sym)                                                                                \
            sym = {                                                                                        \
                .name = name,                                                                              \
                .location = location,                                                                      \
                .null_field_name = NULL,                                                                   \
                .unresolved = {0},                                                                         \
            };                                                                                             \
                                                                                                           \
            VECTOR_PUSH(sym.unresolved, user);                                                             \
            VECTOR_PUSH(p->CAT(type, _syms), sym);                                                         \
        }                                                                                                  \
                                                                                                           \
        /** Add an entry to the thing if it exists, but isn't resolved yet. **/                            \
        else if (!ptr->null_field_name) {                                                                  \
            VECTOR_PUSH(ptr->unresolved, user);                                                            \
        }                                                                                                  \
                                                                                                           \
        /** Otherwise, resolve it. **/                                                                     \
        else {                                                                                             \
            *user = ptr->assign_field_name;                                                                \
        }                                                                                                  \
    }

RESOLVE_OR_DECLARE(function, const char *, function, function->name)
RESOLVE_OR_DECLARE(block, IRBlock *, block, block)
RESOLVE_OR_DECLARE(temp, IRInstruction *, instruction, instruction)

#define RESOLVE_ALL(p, err_name, type, param_type, field, value, on_err) \
    do {                                                                 \
        bool success = true;                                             \
        VECTOR_FOREACH (CAT(type, _sym), sym, (p)->CAT(type, _syms)) {   \
            /** An unresolved function is a parse error. **/             \
            if (!sym->field) {                                           \
                issue_diagnostic(                                        \
                    DIAG_ERR,                                            \
                    (p)->filename,                                       \
                    (p)->source,                                         \
                    sym->location,                                       \
                    "Unknown " err_name " '%.*s'",                       \
                    (int) sym->name.size, sym->name.data);               \
                success = false;                                         \
            } /** Otherwise, resolve the function. **/                   \
            else {                                                       \
                VECTOR_FOREACH (param_type *, user, sym->unresolved)     \
                    **user = sym->value;                                 \
            }                                                            \
        }                                                                \
        if (!success) { on_err; }                                        \
    } while (0)

/// This function handles the bulk of the parsing.
/// It returns true if the parsed instruction was a branch, and false otherwise.
///
/// <instruction>       ::= [ <temp> [ ":" <register> ] "=" ] <value-instruction> "\n"
///                       | <void-instruction> "\n"
static bool parse_instruction_or_branch(IRParser *p) {
    if (p->tok_type != tk_ident && p->tok_type != tk_temp)
        ERR("Expected instruction name or temporary");

    /// An instruction may be assigned a temporary or physical register.
    span name = {0};
    loc name_location = {0};
    IRInstruction *i = NULL;
    bool void_instruction = false;
    bool have_temp = false;
    bool is_tail_call = false;
    bool is_branch = false;

    /// Parse the temporary or physical register if there is one.
    if (p->tok_type == tk_temp) {
        name = p->tok;
        name_location = here(p);
        have_temp = true;
        next_token(p);

        /// Yeet "=".
        if (p->tok_type != tk_assign) ERR("Expected '=' after temporary or register");
        next_token(p);
    }

    /// An instruction may be a number.
    /// <value-instruction> ::= NUMBER
    if (p->tok_type == tk_number) {
        i = ir_immediate(p->context, p->integer);
        next_token(p);
    }

    /// Otherwise, the next token must be an identifier.
    else if (p->tok_type != tk_ident)
        ERR("Expected instruction name");

    /// The current token is an identifier; we now need to parse the instruction.
    else {
        /// Get the instruction type.
        enum IrParserInstructionType type = irtype_from_span(p->tok);
        loc i_loc = here(p);

        /// Parse the instruction.
        switch (type) {
            /// Should never happen.
            default: ASSERT(false, "Unhandled instruction type %d", type);

            /// Invalid instruction name.
            case IR_INSTRUCTIONS_COUNT: ERR("Unknown instruction name '%.*s'", (int) p->tok.size, p->tok.data);

            /// [ TAIL ] CALL ( <name> | <temp> ) "(" <parameters> ")"
            case TAIL:
                next_token(p);
                if (p->tok_type != tk_ident || irtype_from_span(p->tok) != CALL)
                    ERR_AT(i_loc, "Expected 'call' after 'tail'");
                is_tail_call = true;
                i_loc = here(p);
                FALLTHROUGH;
            case CALL: {
                next_token(p);

                /// We need to create the call manually here.
                INSTRUCTION(call, IR_CALL)
                ir_insert(p->context, call);
                call->value.call.tail_call = is_tail_call;

                /// Set the call type and target.
                switch (p->tok_type) {
                    default: ERR_AT(i_loc, "Expected function name or temporary after call");

                    /// Direct call.
                    case tk_ident: {
                        call->value.call.type = IR_CALLTYPE_DIRECT;
                        resolve_or_declare_function(p, here(p), p->tok, &call->value.call.value.name);
                    } break;

                    /// Indirect call.
                    case tk_temp: {
                        /// Resolve the temporary.
                        call->value.call.type = IR_CALLTYPE_INDIRECT;
                        call->value.call.value.callee = resolve_curr_temp(p);
                        mark_used(call->value.call.value.callee, i);
                    } break;
                }

                /// Parameter list.
                next_token(p);
                if (p->tok_type != tk_lparen) ERR("Expected '(' after function name");
                next_token(p);

                /// Parse the parameters.
                while (p->tok_type != tk_rparen) {
                    /// Create a parameter reference.
                    if (p->tok_type != tk_temp) ERR("Expected temporary after '(' or ','");
                    ir_add_function_call_argument(p->context, call, resolve_curr_temp(p));
                    next_token(p);

                    /// Yeet the comma if there is one.
                    if (p->tok_type != tk_comma) break;
                    next_token(p);
                }

                /// Yeet ")".
                if (p->tok_type != tk_rparen) ERR("Expected ')' after function parameters");
                i = call;
                next_token(p);
            } break;

            /// PHI { "[" <name> ":" <temp> "]" }
            case PHI: {
                next_token(p);
                i = ir_phi(p->context);

                /// Parse the phi arguments.
                while (p->tok_type == tk_lbrack) {
                    next_token(p);

                    /// Unfortunately, we need to do this manually.
                    IRPhiArgument *arg = calloc(1, sizeof *arg);

                    /// Block.
                    if (p->tok_type != tk_ident) ERR("Expected block name after '[' in PHI");
                    resolve_or_declare_block(p, here(p), p->tok, &arg->block);
                    next_token(p);

                    /// Yeet ":".
                    if (p->tok_type != tk_colon) ERR("Expected ':' after block name in PHI");
                    next_token(p);

                    /// Temporary. This is the *only* place in the grammar
                    /// where a we allow forward references to temporaries.
                    resolve_or_declare_temp(p, here(p), p->tok, &arg->value);
                    next_token(p);

                    /// Add the argument to the PHI.
                    ir_phi_add_argument(i, arg);

                    /// Yeet ",".
                    if (p->tok_type != tk_comma) break;
                    next_token(p);
                }

                /// Yeet "]".
                if (p->tok_type != tk_rbrack) ERR("Expected ']' after PHI arguments");
                next_token(p);
            } break;

            /// IMM <temp>
            case IMMEDIATE: {
                next_token(p);
                if (p->tok_type != tk_number) ERR("Expected number after 'imm'");
                i = ir_immediate(p->context, p->integer);
                next_token(p);
            } break;

            /// <unary> <temp>
            /// <unary> ::= COPY
            /// COPY <temp>
            case COPY: {
                next_token(p);

                /// Parse the temporary.
                if (p->tok_type != tk_temp) ERR_AT(i_loc, "Expected temporary after unary instruction");
                i = ir_copy(p->context, resolve_curr_temp(p));
                ir_insert(p->context, i);
                next_token(p);
            } break;

            /// <binary> <temp> "," <temp>
            /// <binary> ::= ADD | SUB | MUL | DIV | MOD | EQ | NE | LT | LE | GT | GE | SHL | SHR | SAR
            case ADD:
            case SUBTRACT:
            case MULTIPLY:
            case DIVIDE:
            case MODULO:
            case EQ:
            case NE:
            case LT:
            case LE:
            case GT:
            case GE:
            case SHIFT_LEFT:
            case SHIFT_RIGHT_LOGICAL:
            case SHIFT_RIGHT_ARITHMETIC: {
                next_token(p);

                /// Parse the first temporary.
                if (p->tok_type != tk_temp) ERR_AT(i_loc, "Expected temporary after binary instruction");
                IRInstruction *a = resolve_curr_temp(p);
                next_token(p);

                /// Yeet ",".
                if (p->tok_type != tk_comma) ERR_AT(i_loc, "Expected ',' after first temporary in binary instruction");
                next_token(p);

                /// Parse the second temporary.
                if (p->tok_type != tk_temp) ERR_AT(i_loc, "Expected temporary after ',' in binary instruction");
                IRInstruction *b = resolve_curr_temp(p);
                next_token(p);

                /// Create the instruction.
                switch (type) {
                    case ADD: i = ir_add(p->context, a, b); break;
                    case SUBTRACT: i = ir_subtract(p->context, a, b); break;
                    case MULTIPLY: i = ir_multiply(p->context, a, b); break;
                    case DIVIDE: i = ir_divide(p->context, a, b); break;
                    case MODULO: i = ir_modulo(p->context, a, b); break;
                    case EQ: i = ir_comparison(p->context, COMPARE_EQ, a, b); break;
                    case NE: i = ir_comparison(p->context, COMPARE_NE, a, b); break;
                    case LT: i = ir_comparison(p->context, COMPARE_LT, a, b); break;
                    case LE: i = ir_comparison(p->context, COMPARE_LE, a, b); break;
                    case GT: i = ir_comparison(p->context, COMPARE_GT, a, b); break;
                    case GE: i = ir_comparison(p->context, COMPARE_GE, a, b); break;
                    case SHIFT_LEFT: i = ir_shift_left(p->context, a, b); break;
                    case SHIFT_RIGHT_LOGICAL: i = ir_shift_right_logical(p->context, a, b); break;
                    case SHIFT_RIGHT_ARITHMETIC: i = ir_shift_right_arithmetic(p->context, a, b); break;
                    default: UNREACHABLE();
                }
            } break;

            /// LOAD ( <temp> | <name> )
            case LOAD: {
                next_token(p);

                /// Parse the temporary or name.
                if (p->tok_type == tk_temp) i = ir_load(p->context, resolve_curr_temp(p));
                else if (p->tok_type == tk_ident) i = ir_load_global(p->context, strndup(p->tok.data, p->tok.size));
                else ERR_AT(i_loc, "Expected temporary or name after LOAD");
            } break;

            /// <void-instruction> ::= STORE <temp> "," ( <temp> | <name> )
            case STORE: {
                next_token(p);
                void_instruction = true;

                /// Parse the temporary.
                if (p->tok_type != tk_temp) ERR_AT(i_loc, "Expected temporary after STORE");
                IRInstruction *a = resolve_curr_temp(p);
                next_token(p);

                /// Yeet ",".
                if (p->tok_type != tk_comma) ERR_AT(i_loc, "Expected ',' after temporary in STORE");
                next_token(p);

                /// Parse the temporary or name.
                if (p->tok_type == tk_temp) i = ir_store(p->context, a, resolve_curr_temp(p));
                else if (p->tok_type == tk_ident) i = ir_store_global(p->context, a, strndup(p->tok.data, p->tok.size));
                else ERR_AT(i_loc, "Expected temporary or name after ',' in STORE");
                next_token(p);
            } break;

            /// REGISTER NUMBER
            case REGISTER: {
                next_token(p);

                /// Parse the register.
                if (p->tok_type != tk_number) ERR_AT(i_loc, "Expected physical register after REGISTER");
                INSTRUCTION(reg, IR_REGISTER)
                ir_insert(p->context, reg);
                reg->result = (Register) p->integer;
                i = reg;
                next_token(p);
            } break;

            /// ALLOCA
            case STACK_ALLOCATE: {
                next_token(p);
                i = ir_stack_allocate(p->context, 8);
            } break;

            /// <branch> ::= UNREACHABLE "\n" | ...
            case UNREACHABLE: {
                next_token(p);
                void_instruction = true;
                is_branch = true;
                INSTRUCTION(u, IR_UNREACHABLE)
                ir_insert(p->context, u);
                i = u;
            } break;

            /// RETURN [ <temp> ] "\n"
            case IR_RETURN: {
                next_token(p);
                void_instruction = true;
                is_branch = true;
                INSTRUCTION(r, IR_RETURN)
                ir_insert(p->context, r);
                i = r;

                /// Parse the return value if there is one.
                if (p->tok_type == tk_temp) {
                    r->value.reference = resolve_curr_temp(p);
                    mark_used(r->value.reference, i);
                    next_token(p);
                }
            } break;

            /// BR <name> "\n"
            case BRANCH: {
                next_token(p);
                void_instruction = true;
                is_branch = true;
                INSTRUCTION(b, IR_BRANCH)
                ir_insert(p->context, b);
                i = b;

                /// Parse the name.
                if (p->tok_type != tk_ident) ERR_AT(i_loc, "Expected block name after BR");
                resolve_or_declare_block(p, here(p), p->tok, &b->value.block);
                next_token(p);
            } break;

            /// BR-COND <temp> "," <name> "," <name> "\n"
            case BRANCH_CONDITIONAL: {
                next_token(p);
                void_instruction = true;
                is_branch = true;
                INSTRUCTION(b, IR_BRANCH_CONDITIONAL)
                ir_insert(p->context, b);
                i = b;

                /// Parse the temporary.
                if (p->tok_type != tk_temp) ERR_AT(i_loc, "Expected temporary after BR.COND");
                b->value.conditional_branch.condition = resolve_curr_temp(p);
                mark_used(b->value.conditional_branch.condition, i);
                next_token(p);

                /// Yeet ",".
                if (p->tok_type != tk_comma) ERR_AT(i_loc, "Expected ',' after temporary in BR.COND");
                next_token(p);

                /// Parse the first name.
                if (p->tok_type != tk_ident) ERR_AT(i_loc, "Expected block name after ',' in BR.COND");
                resolve_or_declare_block(p, here(p), p->tok, &b->value.conditional_branch.true_branch);
                next_token(p);

                /// Yeet ",".
                if (p->tok_type != tk_comma) ERR_AT(i_loc, "Expected ',' after block name in BR-COND");
                next_token(p);

                /// Parse the second name.
                if (p->tok_type != tk_ident) ERR_AT(i_loc, "Expected block name after ',' in BR-COND");
                resolve_or_declare_block(p, here(p), p->tok, &b->value.conditional_branch.false_branch);
                next_token(p);
            } break;
        }
    }

    /// If the instruction is a void instruction, then a name is not allowed.
    /// Otherwise, add it to the symbol table.
    if (have_temp) {
        if (void_instruction) ERR_AT(name_location, "Instructions that return nothing cannot be assigned to a temporary");
        make_temp(p, name_location, name, i);
    }

    /// An instruction must be followed by a newline.
    if (p->tok_type != tk_newline) ERR("Expected newline after instruction");
    next_token(p);

    /// Return true if the instruction is a branch.
    return is_branch;
}

/// <block-body> ::= <instruction>* <branch>
static void parse_block_body(IRParser *p) {
    p->keep_newlines = true;
    for (;;) {
        /// An rbrace is not allowed here since we haven’t seen the branch yet.
        /// This check is just so we issue a better diagnostic in this case.
        if (p->tok_type == tk_rbrace) {
            loc current_block_location;
            if (!p->block_syms.size) current_block_location = here(p);
            else current_block_location = p->block_syms.data[p->block_syms.size - 1].location;
            ERR_AT(current_block_location, "Missing branch in block");
        }

        /// Parse the next instruction or branch.
        if (parse_instruction_or_branch(p)) break;
    }
    p->keep_newlines = false;
}

/// <block> ::= <name> ":" <block-body>
static void parse_block(IRParser *p) {
    /// Parse the block name and create a new block.
    if (p->tok_type != tk_ident) ERR("Expected block");
    IRBlock *block = ir_block_create();
    ir_block_attach_to_function(VECTOR_BACK(*p->context->functions), block);
    p->context->block = block;
    make_block(p, here(p), p->tok, VECTOR_BACK(*p->context->functions)->blocks.last);
    next_token(p);
    if (p->tok_type != tk_colon) ERR("expected ':' after block name");

    /// From now on, we care about newlines.
    next_token(p);

    /// Parse the body.
    parse_block_body(p);
}

/// Check if the parser is currently looking at a block name.
///
/// Since this is the only place in the language that would
/// require a token of lookahead, we instead resort to a
/// rather ugly hack that involves scanning forward until
/// we find the next newline or non-whitespace token. If it
/// is a colon, then this is a block name.
static bool at_block_name(IRParser *p) {
    /// A block name is an identifier followed by a colon.
    if (p->tok_type != tk_ident) return false;

    /// Equivalent to: return lookahead(p)->tok_type == tk_colon.
    while (isspace(p->lastc) && p->lastc != '\n') next_char(p);
    return p->lastc == ':';
}

/// <body> ::= <first-block> <block>*
/// <first-block> ::= ( <name> ":" ) <block-body>
static void parse_body(IRParser *p) {
    /// The first block is special, because it can be unnamed.
    if (!at_block_name(p)) {
        make_block(p, here(p), p->tok, VECTOR_BACK(*p->context->functions)->blocks.first);

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
    /// Parameter list.
    if (p->tok_type != tk_lparen) ERR("Expected '(' after function name");
    next_token(p);

    /// Parse the parameters.
    size_t param_count = 0;
    while (p->tok_type != tk_rparen) {
        /// Create a parameter reference.
        if (p->tok_type != tk_temp) ERR("Expected temporary after '(' or ','");
        if (p->tok.data[0] == '#') ERR("Function parameter must be a temporary register");
        ir_add_parameter_to_function(VECTOR_BACK(*p->context->functions));
        IRInstruction *param = ir_parameter(p->context, param_count++);
        make_temp(p, here(p), p->tok, ir_load_local(p->context, param));
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
static void parse_function(IRParser *p) {
    next_token(p); /// Yeet 'defun'.

    /// Function name.
    if (p->tok_type != tk_ident) ERR("Expected function name after 'defun'");
    span name = p->tok;
    loc location = here(p);
    ir_function(p->context, strndup(p->tok.data, p->tok.size), 0);
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

    /// Add an entry for the function.
    make_function(p, location, name, VECTOR_BACK(*p->context->functions));
}

/// <ir> ::= { <function> | <extern> }
/// TODO: All parse functions may leak memory if there is a parse error.
///       Do we care about that? (If this ever ends up in a library, we should).
static bool parse_ir(IRParser *p) {
    /// Set up error handling.
    if (setjmp(p->jmp)) return false;

    /// Read the first character and lex the first token.
    next_char(p);
    next_token(p);

    /// Actually parse the IR.
    for (;;) {
        /// Parse a top-level declaration.
        switch (p->tok_type) {
            case tk_ident: {
                if (IDENT("defun")) parse_function(p);
                else if (IDENT("declare")) parse_extern(p);
                else ERR("Expected 'defun' or 'declare'");

                /// After parsing a function, resolve temporaries and blocks.
                RESOLVE_ALL(p, "block", block, IRBlock *, block, block, return false);
                RESOLVE_ALL(p, "temporary", temp, IRInstruction *, instruction, instruction, return false);
                VECTOR_CLEAR(p->block_syms);
                VECTOR_CLEAR(p->temp_syms);
            } break;
            case tk_eof: return true;
            default: ERR("Expected 'defun' or 'declare'");
        }
    }
}

/// Do *not* use ERR() after this point
#undef ERR

bool ir_parse(CodegenContext *context, const char *filename, string ir) {
    /// Save and restore this at end of scope.
    IRFunction *f = context->function;
    IRBlock *b = context->block;
    filename = filename ? filename : "<string>";

    /// Create a parser.
    IRParser parser = {
        .tok_type = tk_invalid,
        .lastc = ' ', /// Cause the lexer to skip whitespace.
        .keep_newlines = false,
        .context = context,
        .filename = filename,
        .source = {.size = ir.size, .data = ir.data},
        .source_start = ir.data,
        .source_end = ir.data + ir.size,
        .source_curr = ir.data,
        .tok = {0},
        .integer = 0,
        .block_syms = {0},
        .temp_syms = {0},
        .function_syms = {0},
    };

    /// Parse the IR.
    bool parse_ok = parse_ir(&parser);

    /// Restore the current function and block.
    context->function = f;
    context->block = b;

    /// Resolve functions.
    if (parse_ok) {
        RESOLVE_ALL(&parser, "function", function, const char *, function, function->name, parse_ok = false);
        if (parse_ok) return parse_ok;
    }

    /// There was an error. First, clean up the parser context.
    VECTOR_DELETE(parser.temp_syms);
    VECTOR_DELETE(parser.block_syms);
    VECTOR_DELETE(parser.function_syms);

    /// Remove all functions we added from the context.
    bool found = false;
    usz removed = 0;
    VECTOR_FOREACH_PTR (IRFunction *, func, *context->functions) {
        /// Do *not* delete anything before f!
        if (func == f) {
            found = true;
            continue;
        }
        if (!found) continue;

        /// Delete all blocks and all instructions.
        for (IRBlock *block = func->blocks.first; block;) {
            IRBlock *next = block->next;
            for (IRInstruction *i = block->instructions.first; i;) {
                IRInstruction *next_i = i->next;
                free(i);
                i = next_i;
            }
            free(block);
            block = next;
        }

        /// Delete the function.
        free(func->name);
        free(func);
        removed++;
    }
    context->functions->size -= removed;
    return false;
}