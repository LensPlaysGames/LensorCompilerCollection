#include <codegen/intermediate_representation.h>
#include <error.h>
#include <ir_parser.h>
#include <math.h>
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>

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
FORMAT(printf, 5, 6)
void issue_diagnostic(
    /// Error level and source location.
    enum diagnostic_level level,
    const char *filename,
    const char *source,
    loc location,

    /// The actual error message.
    const char *fmt,
    ...) {
    ASSERT(level >= 0 && level < DIAG_COUNT);

    /// Check if stderr is a terminal.
    bool is_terminal = isatty(fileno(stderr));

    /// Seek to the start of the error.
    usz line = 0;
    usz column = 0;
    for (usz i = 0; i < location.start; i++) {
        /// Handle newlines.
        if (source[i] == '\r' || source[i] == '\n') {
            line++;
            column = 0;

            /// Last character.
            if (!source[i + 1]) break;

            /// Replace CRLF and LFCR with a single newline.
            char c = source[i + 1];
            if ((c == '\r' || c == '\n') && c != source[i]) i++;
        } else if (!source[i]) {
            break;
        } else column++;
    }

    /// The start/end of the error.
    const char *err_start = source + location.start;
    const char *err_end = source + location.end;

    /// Find the start/end of the line.
    const char *line_start = source + location.start;
    while (line_start > source && line_start[-1] != '\n') line_start--;
    const char *line_end = source + location.end;
    while (*line_end && *line_end != '\n') line_end++;

    /// Print file, function, and line.
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
    fprintf(stderr, "%s:%zu:%zu: ", filename, line + 1, column + 1);
    if (is_terminal) fprintf(stderr, "\033[m%s", diagnostic_level_colours[level]);
    fprintf(stderr, "%s: ", diagnostic_level_names[level]);
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");

    /// Print the error message.
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    /// Print the line up to the start of the error location.
    if (is_terminal) fprintf(stderr, "\033[m");
    fprintf(stderr, "\n %zu | %*s\n", line + 1, (int) (err_start - line_start), line_start);

    /// Print the error location highlighted.
    if (is_terminal) fprintf(stderr, "\033[m%s", diagnostic_level_colours[level]);
    fprintf(stderr, "%*s", (int) (err_end - err_start), err_start);

    /// Print the line after the end of the error location.
    if (is_terminal) fprintf(stderr, "\033[m");
    fprintf(stderr, "%*s\n", (int) (line_end - err_end), err_end);

    /// Determine the length of the line number and print that many spaces.
    const char *spaces = "                                     ";
    usz linum_sz = line ? (usz) log10((double) (line + 1)) + 1 : 1;
    fprintf(stderr, "%*s| ", (int) linum_sz, spaces);

    /// Print a space for each character up to the start of the error.
    /// Replace tabs w/ 4 spaces. If your tabs are not 4 spaces, then
    /// the caret will be misaligned. In that case, you have brought
    /// that upon yourself.
    for (const char *c = line_start; c < err_start; c++) {
        if (*c == '\t') fprintf(stderr, "    ");
        else fprintf(stderr, " ");
    }

    /// Print a tilde for each character in the error.
    if (is_terminal) fprintf(stderr, "\033[m%s", diagnostic_level_colours[level]);
    for (const char *c = err_start; c < err_end; c++) {
        if (*c == '\t') fprintf(stderr, "~~~~");
        else fprintf(stderr, "~");
    }

    /// Print a newline, and with this, we’re finally done.
    if (is_terminal) fprintf(stderr, "\033[m");
    fprintf(stderr, "\n");
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
    VECTOR(IRBlock **) unresolved;
} block_sym;

/// Temporary in the symbol table.
/// The sigil ('%' or '#') is included in the name.
typedef struct {
    span name;
    loc location;
    IRInstruction *instruction;
    VECTOR(char **) unresolved;
} temp_sym;

/// Function in the symbol table.
typedef struct {
    span name;
    loc location;
    IRFunction *function;
    VECTOR(IRFunction **) unresolved;
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
    const char *source_start;
    const char *source_curr;

    /// The text that makes up the current token.
    span tok;

    /// Numeric value of the current token.
    i64 integer;

    /// The start/end positions of the current token.
    loc location;

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
#define DO_ERR_AT(sev, location, ...)                                               \
    do {                                                                            \
        issue_diagnostic(sev, p->filename, p->source_start, location, __VA_ARGS__); \
        longjmp(p->jmp, 1);                                                         \
    } while (0)

#define ERR_AT(location, ...) DO_ERR_AT(DIAG_ERR, location, __VA_ARGS__)
#define ERR(...)              ERR_AT(p->location, __VA_ARGS__)
#define SORRY(...)            DO_ERR_AT(DIAG_SORRY, p->location, __VA_ARGS__)

#define WARN(...)                                                                            \
    do {                                                                                     \
        issue_diagnostic(DIAG_WARN, p->filename, p->source_start, p->location, __VA_ARGS__); \
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

/// Lex the next character.
static void next_char(IRParser *p) {
    /// Keep returning EOF once EOF has been reached.
    if (!p->lastc) return;

    /// Read the next character.
    p->lastc = *p->source_curr++;

    /// Handle newlines.
    if (p->lastc == '\r' || p->lastc == '\n') {
        /// We never return '\r'.
        p->lastc = '\n';

        /// Last character.
        if (!*p->source_curr) return;

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

/// Add a temporary to the current symbol table.
static bool make_temporary(IRParser *p, loc location, span name, IRInstruction *temp) {
    TODO();
}

/// Add a block to the current symbol table.
static bool make_block(IRParser *p, loc location, span name, IRBlock *block) {
    TODO();
}

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
    return resolve_temp(p, p->location, p->tok);
}

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

    /// Parse the temporary or physical register if there is one.
    if (p->tok_type != tk_ident) {
        name = p->tok;
        name_location = p->location;
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
        loc i_loc = p->location;

        /// Parse the instruction.
        switch (type) {
            /// Should never happen.
            default: ASSERT(false, "Unhandled instruction type %d", type);

            /// [ TAIL ] CALL ( <name> | <temp> ) "(" <parameters> ")"
            case TAIL:
                next_token(p);
                if (p->tok_type != tk_ident || irtype_from_span(p->tok) != IR_CALL) ERR_AT(i_loc, "Expected 'call' after 'tail'");
                is_tail_call = true;
                i_loc = p->location;
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
                        resolve_or_declare_function(p, p->tok, &call->value.call.value.name);
                    } break;

                    /// Indirect call.
                    case tk_temp: {
                        /// Resolve the temporary.
                        call->value.call.type = IR_CALLTYPE_INDIRECT;
                        call->value.call.value.callee = resolve_curr_temp(p);
                    } break;
                }

                /// Parameter list.
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
                    resolve_or_declare_block(p, p->tok, &arg->block);
                    next_token(p);

                    /// Yeet ":".
                    if (p->tok_type != tk_colon) ERR("Expected ':' after block name in PHI");
                    next_token(p);

                    /// Temporary. This is the *only* place in the grammar
                    /// where a we allow forward references to temporaries.
                    resolve_or_declare_temp(p, p->tok, &arg->value);
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

            /// <unary> <temp>
            /// <unary> ::= COPY
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
                INSTRUCTION(u, IR_UNREACHABLE)
                ir_insert(p->context, u);
                i = u;
            } break;

            /// RETURN [ <temp> ] "\n"
            case IR_RETURN: {
                next_token(p);
                void_instruction = true;
                INSTRUCTION(r, IR_RETURN)
                ir_insert(p->context, r);
                i = r;

                /// Parse the return value if there is one.
                if (p->tok_type == tk_temp) {
                    r->value.reference = resolve_curr_temp(p);
                    next_token(p);
                }
            } break;

            /// BR <name> "\n"
            case BRANCH: {
                next_token(p);
                void_instruction = true;
                INSTRUCTION(b, IR_BRANCH)
                ir_insert(p->context, b);
                i = b;

                /// Parse the name.
                if (p->tok_type != tk_ident) ERR_AT(i_loc, "Expected block name after BR");
                resolve_or_declare_block(p, p->tok, &b->value.block);
                next_token(p);
            } break;

            /// BR-COND <temp> "," <name> "," <name> "\n"
            case BRANCH_CONDITIONAL: {
                next_token(p);
                void_instruction = true;
                INSTRUCTION(b, IR_BRANCH_CONDITIONAL)
                ir_insert(p->context, b);
                i = b;

                /// Parse the temporary.
                if (p->tok_type != tk_temp) ERR_AT(i_loc, "Expected temporary after BR.COND");
                b->value.conditional_branch.condition = resolve_curr_temp(p);
                next_token(p);

                /// Yeet ",".
                if (p->tok_type != tk_comma) ERR_AT(i_loc, "Expected ',' after temporary in BR.COND");
                next_token(p);

                /// Parse the first name.
                if (p->tok_type != tk_ident) ERR_AT(i_loc, "Expected block name after ',' in BR.COND");
                resolve_or_declare_block(p, p->tok, &b->value.conditional_branch.true_branch);
                next_token(p);

                /// Yeet ",".
                if (p->tok_type != tk_comma) ERR_AT(i_loc, "Expected ',' after block name in BR-COND");
                next_token(p);

                /// Parse the second name.
                if (p->tok_type != tk_ident) ERR_AT(i_loc, "Expected block name after ',' in BR-COND");
                resolve_or_declare_block(p, p->tok, &b->value.conditional_branch.false_branch);
                next_token(p);
            } break;
        }
    }

    /// If the instruction is a void instruction, then a name is not allowed.
    /// Otherwise, add it to the symbol table.
    if (have_temp) {
        if (void_instruction) ERR_AT(name_location, "Instructions that return nothing cannot be assigned to a temporary");
        make_temporary(p, name_location, name, i);
    }

    /// An instruction must be followed by a newline.
    if (p->tok_type != tk_newline) ERR("Expected newline after instruction");
    next_token(p);
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
        if (parse_instruction_or_branch(p)) break;
    }
}

/// <block>       ::= <name> ":" <block-body>
static void parse_block(IRParser *p) {
    /// Parse the block name and create a new block.
    if (p->tok_type != tk_ident) ERR("Expected block");
    make_block(p, p->tok, ir_block_attach_to_function(VECTOR_BACK(*p->context->functions), ir_block_create()));
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
    if (p->tok_type == tk_ident && lookahead(p)->tok_type == tk_colon) {
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
        if (p->tok.data[0] == '#') ERR("Function parameter must be a temporary register");
        make_temporary(p, p->location, p->tok, ir_parameter_reference(p->context, param_count++));
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
    /// Set up error handling.
    if (setjmp(p->jmp)) return false;

    /// Read the first character and lex the first token.
    next_char(p);
    next_token(p);

    /// Actually parse the IR.
    for (;;) {
        /// Parse a top-level declaration.
        switch (p->tok_type) {
            case tk_ident:
                if (IDENT("defun")) parse_function(p);
                else if (IDENT("declare")) parse_extern(p);
                else ERR("Expected 'defun' or 'declare'");
            case tk_eof: return true;
            default: ERR("Expected 'defun' or 'declare'");
        }
    }
}

bool ir_parse(CodegenContext *context, const char *ir, size_t sz) {
    IRFunction *f = context->function;

    TODO();

    context->function = f;
    return true;
}