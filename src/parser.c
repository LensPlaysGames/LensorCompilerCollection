#include <ast.h>
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <parser.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utils.h>
#include <vector.h>

// Forward declares.
typedef struct Parser Parser;
static Type *parse_type(Parser *p);
static Node *parse_expr(Parser *p);

static usz is_power_of_two(usz i) {
  return (i & (i - 1)) == 0;
}

/// ===========================================================================
///  Error handling.
/// ===========================================================================
#define ISSUE_DIAGNOSTIC(sev, loc, parser, ...)                                        \
  do {                                                                                 \
    issue_diagnostic((sev), (parser)->filename, (parser)->source, (loc), __VA_ARGS__); \
  } while (0)
#define ISSUE_FATAL_DIAGNOSTIC(sev, loc, parser, ...)                                  \
  do {                                                                                 \
    issue_diagnostic((sev), (parser)->filename, (parser)->source, (loc), __VA_ARGS__); \
    longjmp(parser->error_buffer, 1);                                                  \
  } while (0)
#define WARN_AT(loc, ...) ISSUE_DIAGNOSTIC(DIAG_ERR, loc, p, __VA_ARGS__)
#define WARN(...) WARN_AT(p->tok.source_location, __VA_ARGS__)
#define ERR_AT(loc, ...) ISSUE_FATAL_DIAGNOSTIC(DIAG_ERR, loc, p, __VA_ARGS__)
#define ERR(...) ERR_AT(p->tok.source_location, __VA_ARGS__)

/// ===========================================================================
///  Types and enums.
/// ===========================================================================
enum {
  PREFIX_PRECEDENCE = 10000,
};

typedef struct Token {
  enum TokenType type;
  loc source_location;

  // Yes, this is cursed. There is an AST node stored in a token because
  // of macro reasons.
  Node *node;

  // Just a simple number.
  u64 integer;

  // Any text; may be an identifier, keyword, etc.
  string_buffer text;

  // True iff identifier found in `text` was created via escaping.
  bool artificial;

  // True iff `node` shouldn't be deep copied when parsed, but used
  // directly (for expressions with side effects).
  bool expr_once;

} Token;

// A macro argument selector (i.e. the "token" part of `$name:token`)
// This is the value of the `integer` member of the macro argument
// token.
typedef enum MacroArgumentSelector {
  MACRO_ARG_SEL_TOKEN,
  MACRO_ARG_SEL_EXPR,
  MACRO_ARG_SEL_EXPR_ONCE,
} MacroArgumentSelector;

typedef Vector(Token) TokenVector;
typedef struct Macro {
  string name;
  TokenVector parameters;
  TokenVector expansion;
  loc source_location;
  usz gensym_count;
} Macro;
typedef Vector(Macro) MacroVector;

typedef struct NamedToken {
  span name;
  Token token;
} NamedToken;

typedef struct MacroExpansion {
  // This is an index into the Parser.macros vector where we will find
  // the macro being expanded by this expansion.
  usz macro_index;

  // Index into Macro.expansion vector.
  usz expansion_index;

  Vector(NamedToken) bound_arguments;

  loc source_location;

  // Index into this vector using TK_GENSYM.integer value.
  Vector(string) gensyms;
} MacroExpansion;

typedef struct Parser {
  /// The source code that we’re parsing.
  span source;

  /// The name of the file that we’re parsing.
  const char *filename;

  /// The last character read.
  char lastc;

  /// Lexer state.
  const char *curr;
  const char *end;
  MacroVector macros;
  Vector(MacroExpansion) macro_expansion_stack;
  // When true, don't expand anything, like macros.
  bool raw_mode;

  usz gensym_counter;

  /// Whether we’re in a function.
  bool in_function;

  /// The current token.
  Token tok;

  /// The AST of the program.
  AST *ast;

  /// For error handling.
  jmp_buf error_buffer;
} Parser;

/// ===========================================================================
///  Lexer
/// ===========================================================================
/// All keywords.
const struct {
  span kw;
  enum TokenType type;
} keywords[12] = {
  {literal_span_raw("if"), TK_IF},
  {literal_span_raw("else"), TK_ELSE},
  {literal_span_raw("while"), TK_WHILE},
  {literal_span_raw("ext"), TK_EXT},
  {literal_span_raw("as"), TK_AS},
  {literal_span_raw("type"), TK_TYPE},
  {literal_span_raw("void"), TK_VOID},
  {literal_span_raw("byte"), TK_BYTE},
  {literal_span_raw("integer"), TK_INTEGER_KW},
  {literal_span_raw("for"), TK_FOR},
  {literal_span_raw("return"), TK_RETURN},
  {literal_span_raw("export"), TK_EXPORT}
};

/// Check if a character may start an identifier.
static bool isstart(char c) {
  return isalpha(c) || c == '_';
}

/// Check if a character may be part of an identifier.
static bool iscontinue(char c) {
  return isstart(c) || isdigit(c);
}

/// Lex the next character.
static void next_char(Parser *p) {
  /// Keep returning EOF once EOF has been reached.
  if (p->curr >= p->end) {
    p->lastc = 0;
    return;
  }

  /// Read the next character.
  p->lastc = *p->curr++;
  if (p->lastc == 0) ERR("Lexer can not handle null bytes");

  /// Collapse CRLF and LFCR to a single newline,
  /// but keep CRCR and LFLF as two newlines.
  if (p->lastc == '\r' || p->lastc == '\n') {
      /// Two newlines in a row.
      if (p->curr != p->end && (*p->curr == '\r' || *p->curr == '\n')) {
          bool same = p->lastc == *p->curr;
          p->lastc = '\n';

          /// CRCR or LFLF
          if (same) return;

          /// CRLF or LFCR
          p->curr++;
      }

      /// Either CR or LF followed by something else.
      p->lastc = '\n';
  }
}

/// Lex an identifier.
/// This function assumes that the Parser.lastc member refers to the first
/// character in the identifier.
static void next_identifier(Parser *p) {
  p->tok.type = TK_IDENT;

  /// The start of the identifier.
  vector_clear(p->tok.text);
  vector_push(p->tok.text, p->lastc);
  next_char(p);

  /// Read the rest of the identifier.
  while (iscontinue(p->lastc)) {
    vector_push(p->tok.text, p->lastc);
    next_char(p);
  }
}

/// Lex a string.
static void next_string(Parser *p) {
  /// Yeet the delimiter and clear the string.
  char delim = p->lastc;
  vector_clear(p->tok.text);
  next_char(p);

  /// Single-quoted strings are not escaped.
  if (delim == '\'') {
    while (p->lastc != delim) {
      if (p->lastc == 0) ERR("Unterminated string literal");
      vector_push(p->tok.text, p->lastc);
      next_char(p);
    }
  }

  /// Double-quoted strings are escaped.
  else {
    ASSERT(delim == '"');
    while (p->lastc != delim) {
      if (p->lastc == 0) ERR("Unterminated string literal");

      /// Handle escape sequences.
      if (p->lastc == '\\') {
        next_char(p);
        switch (p->lastc) {
          case 'n': vector_push(p->tok.text, '\n'); break;
          case 'r': vector_push(p->tok.text, '\r'); break;
          case 't': vector_push(p->tok.text, '\t'); break;
          case 'f': vector_push(p->tok.text, '\f'); break;
          case 'v': vector_push(p->tok.text, '\v'); break;
          case 'a': vector_push(p->tok.text, '\a'); break;
          case 'b': vector_push(p->tok.text, '\b'); break;
          case 'e': vector_push(p->tok.text, '\033'); break;
          case '0': vector_push(p->tok.text, '\0'); break;
          case '\'': vector_push(p->tok.text, '\''); break;
          case '\"': vector_push(p->tok.text, '\"'); break;
          case '\\': vector_push(p->tok.text, '\\'); break;
          default: ERR("Invalid escape sequence");
        }
      }

      /// Just append the character if it’s not an escape sequence.
      else { vector_push(p->tok.text, p->lastc); }
      next_char(p);
    }
  }

  /// Make sure the string is terminated by the delimiter.
  if (p->lastc != delim) ERR("Unterminated string literal");
  p->tok.type = TK_STRING;
  next_char(p);
}

/// Parse a number.
static void parse_number(Parser *p, int base) {
  /// Zero-terminate the string or else `strtoull()` might try
  /// to convert data left over from the previous token.
  string_buf_zterm(&p->tok.text);

  /// Convert the number.
  char *end;
  errno = 0;
  p->tok.integer = (u64) strtoull(p->tok.text.data, &end, base);
  if (errno == ERANGE) ERR("Integer literal too large");
  if (end != p->tok.text.data + p->tok.text.size) ERR("Invalid integer literal");
}

/// Lex a number.
static void next_number(Parser *p) {
  /// Record the start of the number.
  vector_clear(p->tok.text);
  p->tok.integer = 0;
  p->tok.type = TK_NUMBER;

  /// At least one leading zero.
  if (p->lastc == '0') {
    /// Discard the zero.
    next_char(p);

    /// Another zero is an error.
    if (p->lastc == '0') ERR("Leading zeroes are not allowed in decimal literals. Use 0o/0O for octal literals.");

#define DO_PARSE_NUMBER(name, chars, condition, base)                       \
  /** Read all chars that are part of the literal. **/                      \
  if (p->lastc == chars[0] || p->lastc == chars[1]) {                       \
    /** Yeet the prefix. **/                                                \
    next_char(p);                                                           \
                                                                            \
    /** Lex the digits. **/                                                 \
    while (condition) {                                                     \
      vector_push(p->tok.text, p->lastc);                                   \
      next_char(p);                                                         \
    }                                                                       \
                                                                            \
    /** We need at least one digit. **/                                     \
    p->tok.source_location.end = (u32) ((p->curr - 1) - p->source.data);    \
    if (p->tok.text.size == 0) ERR("Expected at least one " name " digit"); \
                                                                            \
    /** Actually parse the number. **/                                      \
    return parse_number(p, base);                                           \
  }

    DO_PARSE_NUMBER("binary", "bB", p->lastc == '0' || p->lastc == '1', 2)
    DO_PARSE_NUMBER("octal", "oO", isdigit(p->lastc) && p->lastc < '8', 8)
    DO_PARSE_NUMBER("hexadecimal", "xX", isxdigit(p->lastc), 16)

#undef DO_PARSE_NUMBER

    /// If the next character is a space or delimiter, then this is a literal 0.
    if (isspace(p->lastc) || !isalpha(p->lastc)) return;

    /// Anything else is an error.
    ERR("Invalid integer literal");
  }

  /// Any other digit means we have a decimal number.
  if (isdigit(p->lastc)) {
    do {
      vector_push(p->tok.text, p->lastc);
      next_char(p);
    } while (isdigit(p->lastc));
    return parse_number(p, 10);
  }

  /// Anything else is an error.
  ERR("Invalid integer literal");
}

static bool token_has_string(enum TokenType tt) {
  return tt == TK_MACRO_ARG || tt == TK_IDENT || tt == TK_STRING;
}

static bool token_has_integer(enum TokenType tt) {
  return tt == TK_ARBITRARY_INT || tt == TK_NUMBER || tt == TK_MACRO_ARG;
}

static Token copy_token(Token token) {
  Token new_tok = token;

  if (token_has_string(new_tok.type)) {
    // This horribleness is because we don't have a way to duplicate
    // string_buffer!
    string new_string = string_dup(new_tok.text);
    new_tok.text.data = new_string.data;
    new_tok.text.size = new_string.size;
    new_tok.text.capacity = new_tok.text.size;
  } else new_tok.text = (string_buffer){0};

  return new_tok;
}

static string gensym(usz number) {
  return format("_G_xX_%Z_Xx_G_", number);
}

static void next_token(Parser *p);
static void next_macro(Parser *p) {
  // At this point, the parser state is at the "macro" keyword.
  p->raw_mode = true;

  // Yeet "macro"
  next_token(p);

  // Parse identifier (name of macro)
  if (p->tok.type != TK_IDENT)
    ERR("Expected identifier following macro keyword");

  Macro out = {0};
  out.source_location = p->tok.source_location;
  out.name = string_dup(p->tok.text);

  // Yeet name of macro
  next_token(p);

  // Parse token list
  while (!(p->tok.type == TK_IDENT && !p->tok.artificial && (string_eq(p->tok.text, literal_span("emits")) || string_eq(p->tok.text, literal_span("defines"))))) {
    if (p->tok.type == TK_EOF)
      ERR_AT(out.source_location, "Reached EOF when lexing argument tokens of macro");

    // Ensure macro arg doesn't exist in parameter list
    if (p->tok.type == TK_MACRO_ARG) {
      Token *found = vector_find_if(
        el,
        out.parameters,
        el->type == TK_MACRO_ARG && string_eq(el->text, p->tok.text)
      );

      if (found) ERR("Duplicate macro argument identifier! Pick another name.");
    }

    // Push deep-copied token into parameters.
    Token new_tok = copy_token(p->tok);
    vector_push(out.parameters, new_tok);

    next_token(p);
  }

  // If we hit a "defines" contextual keyword, then we need to parse a
  // list of definitions and keep track of them for proper handling during
  // lexing of the expansion tokens.
  Vector(string) gensym_definitions = {0};
  if (string_eq(p->tok.text, literal_span("defines"))) {
    // Skip "defines"
    next_token(p);
    while (!(p->tok.type == TK_IDENT && !p->tok.artificial && string_eq(p->tok.text, literal_span("emits")))) {
      if (p->tok.type == TK_EOF)
        ERR_AT(out.source_location, "Reached EOF when lexing gensym definitions of macro");

      if (p->tok.type != TK_IDENT)
        ERR("Expected identifier within macro's \"defines\" list");

      if (vector_find_if(dup, gensym_definitions, string_eq(p->tok.text, *dup)))
        ERR("Duplicate identifier in gensym definitions of macro");

      vector_push(gensym_definitions, string_dup(p->tok.text));

      next_token(p);

      // Eat commas in-between definitions.
      if (p->tok.type == TK_COMMA) next_token(p);
    }
  }

  // Skip "emits"
  next_token(p);

  // Parse output token list
  while (!(p->tok.type == TK_IDENT && !p->tok.artificial && string_eq(p->tok.text, literal_span("endmacro")))) {
    if (p->tok.type == TK_EOF)
      ERR_AT(out.source_location, "Reached EOF when lexing expansion tokens of macro; expected \"endmacro\" keyword.");

    if (p->tok.type == TK_MACRO_ARG) {
      // Ensure macro arg exists in parameter list
      Token *found = vector_find_if(
        t,
        out.parameters,
        t->type == TK_MACRO_ARG && string_eq(t->text, p->tok.text)
      );

      if (!found) ERR("Macro argument identifier does not refer to a bound macro argument! Maybe a typo?");
    } else if (p->tok.type == TK_IDENT && gensym_definitions.size) {
      // If the token in the expansion list is an identifier and there
      // are gensym definitions for this macro, we need to replace the
      // uses of the gensym'd identifier with a gensym token, which the
      // parser will replace with a generated identifier.
      foreach_index (i, gensym_definitions) {
        string *def = gensym_definitions.data + i;
        if (string_eq(*def, p->tok.text)) {
          p->tok.type = TK_GENSYM;
          p->tok.integer = i;
          break;
        }
      }
    }

    // Push deep-copied token into parameters.
    Token new_tok = copy_token(p->tok);
    vector_push(out.expansion, new_tok);

    next_token(p);
  }

  if (vector_find_if(m, p->macros, string_eq(m->name, out.name)))
    ERR("Redefinition of macro %S", out.name);

  out.gensym_count = gensym_definitions.size;
  vector_delete(gensym_definitions);

  vector_push(p->macros, out);

  // Skip "endmacro"
  p->raw_mode = false;
  next_token(p);
}

static bool token_equals(Token *A, Token *B) {
  if (!A || !B) return false;
  if (A->type != B->type) return false;
  if (token_has_string(A->type) && !string_eq(A->text, B->text)) return false;
  if (token_has_integer(A->type) && A->integer != B->integer) return false;
  return true;
}

static void expand_macro(Parser *p, Macro *m) {
  // Found macro invocation, do expansion things!
  MacroExpansion expansion = {0};
  expansion.macro_index = (usz)(m - p->macros.data);
  expansion.source_location.start = p->tok.source_location.start;

  p->raw_mode = true;

  foreach (param_tok, m->parameters) {
    next_token(p);

    // param_tok == parameter
    // p->tok == argument

    if (param_tok->type == TK_MACRO_ARG) {
      switch ((MacroArgumentSelector)param_tok->integer) {
      case MACRO_ARG_SEL_TOKEN: {
        NamedToken bound_arg = {0};
        bound_arg.name = as_span(param_tok->text);
        bound_arg.token = copy_token(p->tok);
        vector_push(expansion.bound_arguments, bound_arg);
      } break;
      case MACRO_ARG_SEL_EXPR_ONCE: FALLTHROUGH;
      case MACRO_ARG_SEL_EXPR: {
        u32 beg = p->tok.source_location.start;

        // Yes, this is cursed. The lexer calls into the parser. This is called
        // mutual recursion. But macros go brr, amirite?
        Node *expr = parse_expr(p);
        NamedToken bound_arg = {0};
        bound_arg.name = as_span(param_tok->text);
        bound_arg.token.type = TK_AST_NODE;
        bound_arg.token.node = expr;
        bound_arg.token.source_location.start = beg;
        bound_arg.token.source_location.end = p->tok.source_location.end;
        bound_arg.token.expr_once = param_tok->integer == MACRO_ARG_SEL_EXPR_ONCE;
        vector_push(expansion.bound_arguments, bound_arg);
      } break;
      default: ICE("Unhandled macro argument selector type");
      }
      continue;
    }

    if (!token_equals(&p->tok, param_tok))
      ERR("Ill-formed macro invocation");
  }

  expansion.source_location.end = p->tok.source_location.end;

  for (usz i = 0; i < m->gensym_count; ++i)
    vector_push(expansion.gensyms, gensym(p->gensym_counter++));

  vector_push(p->macro_expansion_stack, expansion);
  p->raw_mode = false;
  next_token(p);
}

/// Further lex an identifier, as it may be a keyword, arbitrary integer type, etc.
/// NOTE: Expects Parser.tok member to be the identifier.
static void handle_identifier(Parser *p) {
  // Handle macro definition.
  // "macro" within argument list of macro == ill-formed program.
  if (!p->raw_mode && !p->tok.artificial && string_eq(p->tok.text, literal_span("macro"))) {
    next_macro(p);
    return;
  }

  Macro *found_macro = vector_find_if(m, p->macros, string_eq(m->name, p->tok.text));
  if (found_macro) {
    expand_macro(p, found_macro);
    return;
  }

  for (size_t i = 0; i < sizeof keywords / sizeof *keywords; i++) {
    if (string_eq(keywords[i].kw, p->tok.text)) {
      p->tok.type = keywords[i].type;
      return;
    }
  }

  // Try and parse a number just after encountering `s` or `u` at the
  // beginning of an identifier.
  if (p->tok.text.size > 1 && (p->tok.text.data[0] == 's' || p->tok.text.data[0] == 'i' || p->tok.text.data[0] == 'u') && isdigit(p->tok.text.data[1])) {
    /// Zero-terminate the string or else `strtoull()` might try
    /// to convert data left over from the previous token.
    string_buf_zterm(&p->tok.text);

    /// Convert the number.
    char *end;
    errno = 0;
    p->tok.integer = (u64) strtoull(p->tok.text.data + 1, &end, 10);
    if (errno == ERANGE) ERR("Bit width of integer is too large.");
    // If the identifier is something like `s64iam`, it's simply an identifier.
    if (end != p->tok.text.data + p->tok.text.size) return;

    p->tok.type = TK_ARBITRARY_INT;
  }
}

/// Lex the next token.
static void next_token(Parser *p) {
  // Pop empty macro expansions off of the expansion stack.
  foreach_rev (expansion, p->macro_expansion_stack) {
    Macro *expandee = p->macros.data + expansion->macro_index;
    if (expansion->expansion_index >= expandee->expansion.size) {
      foreach (s, expansion->gensyms) {
        if (s->data) free(s->data);
      }
      vector_delete(expansion->gensyms);
      vector_delete(expansion->bound_arguments);
      (void)vector_pop(p->macro_expansion_stack);
    }
  }
  // Iff there are macro expansions to handle, get tokens from there
  // instead of from the file.
  if (p->macro_expansion_stack.size) {
    MacroExpansion *expansion = &vector_back(p->macro_expansion_stack);
    Macro *expandee = p->macros.data + expansion->macro_index;
    Token *macro_expansion_token = expandee->expansion.data + expansion->expansion_index;
    if (macro_expansion_token->type == TK_MACRO_ARG) {
      ASSERT(expansion->bound_arguments.size,
             "Macro argument \"%S\" encountered, but none are defined for this macro",
             as_span(macro_expansion_token->text));
      // Get it from bound arguments
      NamedToken *found = vector_find_if(
        t,
        expansion->bound_arguments,
        string_eq(t->name, macro_expansion_token->text)
      );
      ASSERT(found, "Macro argument \"%S\" does not exist (lexer screwed up!)",
             as_span(macro_expansion_token->text));
      p->tok = copy_token(found->token);
    } else p->tok = copy_token(*macro_expansion_token);
    ++expansion->expansion_index;

    // Set artificial to false, because, for example, if we are inserting
    // "endmacro", then we want the inserted identifier to *not* be
    // artificial and be treated *seriously*.
    p->tok.artificial = false;
    // macros within macros within macros within ...
    if (p->tok.type == TK_IDENT && string_eq(p->tok.text, literal_span("macro")))
      next_macro(p);

    return;
  }

  // Reset artificial flag.
  p->tok.artificial = false;

  /// Keep returning EOF once EOF has been reached.
  if (!p->lastc) {
    p->tok.type = TK_EOF;
    return;
  }

  /// Set the token to invalid in case there is an error.
  p->tok.type = TK_INVALID;

  /// Skip whitespace.
  while (isspace(p->lastc)) next_char(p);

  /// Start of the token.
  p->tok.source_location.start = (u32) (p->curr - p->source.data - 1);

  /// Lex the token.
  switch (p->lastc) {
    /// EOF.
    case 0:
      p->tok.type = TK_EOF;
      break;

    case '\\': {
      // Yeet backslash;
      next_char(p);
      // Get identifier
      bool in_raw_mode = p->raw_mode;
      p->raw_mode = true;
      next_token(p);
      p->raw_mode = in_raw_mode;
      if (p->tok.type != TK_IDENT) {
        switch (p->tok.type) {
        case TK_IDENT: break;
        case TK_MACRO_ARG: {
          // Prepend dollar sign.
          string text = format("$%S", as_span(p->tok.text));
          p->tok.text = as_string_buffer(text);
        } break;
        case TK_STRING: {
          // Wrap in double quotes.
          string text = format("\"%S\"", as_span(p->tok.text));
          p->tok.text = as_string_buffer(text);
        } break;
        case TK_NUMBER: {
          // Convert number to string
          string text = format("%U", p->tok.integer);
          p->tok.text = as_string_buffer(text);
        } break;
        case TK_ARBITRARY_INT: {
          string text = format("%c%U", p->tok.text.data[0], p->tok.integer);
          p->tok.text = as_string_buffer(text);
        } break;
        default: {
          string text = string_create(token_type_to_string(p->tok.type));
          p->tok.text = as_string_buffer(text);
        } break;
        }
      }
      p->tok.type = TK_IDENT;
      p->tok.artificial = true;
    } break;

    case '$': {
      if (p->raw_mode) {
        // Yeet '$';
        next_char(p);
        // Get name of macro argument (identifier)
        next_token(p);
        if (p->tok.type != TK_IDENT)
          ERR("Expected identifier following '$' to name macro argument");

        string name = string_dup(p->tok.text);

        // Parse token category term or whatever (sets integer token member)
        if (p->lastc == ':') {
          // Yeet ':'
          next_char(p);
          // Get selector identifier.
          next_token(p);
          if (p->tok.type != TK_IDENT)
            ERR("Expected identifier following ':' in named macro argument");

          MacroArgumentSelector selector = MACRO_ARG_SEL_TOKEN;
          if (string_eq(p->tok.text, literal_span("expr")))
            selector = MACRO_ARG_SEL_EXPR;
          else if (string_eq(p->tok.text, literal_span("expr_once")))
            selector = MACRO_ARG_SEL_EXPR_ONCE;
          else if (string_eq(p->tok.text, literal_span("token")))
            ;
          else ERR("Unrecognised macro argument selector identifier");

          p->tok.integer = (usz)selector;
        }

        p->tok.text = as_string_buffer(name);
        p->tok.type = TK_MACRO_ARG;
      } else {
        next_identifier(p);
        handle_identifier(p);
        break;
      }
    } break;

    case '(':
      p->tok.type = TK_LPAREN;
      next_char(p);
      break;

    case ')':
      p->tok.type = TK_RPAREN;
      next_char(p);
      break;

    case '[':
      p->tok.type = TK_LBRACK;
      next_char(p);
      break;

    case ']':
      p->tok.type = TK_RBRACK;
      next_char(p);
      break;

    case '{':
      p->tok.type = TK_LBRACE;
      next_char(p);
      break;

    case '}':
      p->tok.type = TK_RBRACE;
      next_char(p);
      break;

    case ',':
      p->tok.type = TK_COMMA;
      next_char(p);
      break;

    case '@':
      p->tok.type = TK_AT;
      next_char(p);
      break;

    case ':':
      next_char(p);
      if (p->lastc == '=') {
        p->tok.type = TK_COLON_EQ;
        next_char(p);
      } else if (p->lastc == ':') {
        p->tok.type = TK_COLON_COLON;
        next_char(p);
      } else if (p->lastc == '>') {
        p->tok.type = TK_COLON_GT;
        next_char(p);
      } else {
        p->tok.type = TK_COLON;
      }
      break;

    case ';':
      // Yeet ';'
      next_char(p);
      // Line comments begin with `;;`
      if (p->lastc == ';') {
        while (p->lastc && p->lastc != '\n') next_char(p);
        return next_token(p);
      }
      p->tok.type = TK_SEMICOLON;
      break;

    case '#':
      next_char(p);
      p->tok.type = TK_HASH;
      break;

    case '.':
      next_char(p);
      p->tok.type = TK_DOT;
      break;

    case '+':
      next_char(p);
      p->tok.type = TK_PLUS;
      break;

    case '-':
      next_char(p);
      if (isdigit(p->lastc)) {
        next_number(p);
        p->tok.integer = -p->tok.integer;

        /// The character after a number must be a whitespace or delimiter.
        if (isalpha(p->lastc)) ERR("Invalid integer literal");
      } else {
        p->tok.type = TK_MINUS;
      }
      break;

    case '*':
      next_char(p);
      p->tok.type = TK_STAR;
      break;

    case '/':
      next_char(p);
      p->tok.type = TK_SLASH;
      break;

    case '%':
      next_char(p);
      p->tok.type = TK_PERCENT;
      break;

    case '&':
      next_char(p);
      p->tok.type = TK_AMPERSAND;
      break;

    case '|':
      next_char(p);
      p->tok.type = TK_PIPE;
      break;

    case '^':
      next_char(p);
      p->tok.type = TK_CARET;
      break;

    case '~':
      next_char(p);
      p->tok.type = TK_TILDE;
      break;

    case '!':
      next_char(p);
      if (p->lastc == '=') {
        p->tok.type = TK_NE;
        next_char(p);
      } else {
        p->tok.type = TK_EXCLAM;
      }
      break;

    case '=':
      next_char(p);
      p->tok.type = TK_EQ;
      break;

    case '<':
      next_char(p);
      if (p->lastc == '=') {
        p->tok.type = TK_LE;
        next_char(p);
      } else if (p->lastc == '<') {
        p->tok.type = TK_SHL;
        next_char(p);
      } else {
        p->tok.type = TK_LT;
      }
      break;

    case '>':
      next_char(p);
      if (p->lastc == '=') {
        p->tok.type = TK_GE;
        next_char(p);
      } else if (p->lastc == '>') {
        p->tok.type = TK_SHR;
        next_char(p);
      } else {
        p->tok.type = TK_GT;
      }
      break;

    // String.
    case '"':
    case '\'':
      next_string(p);
      break;

    /// Number or identifier.
    default:
      /// Identifier.
      if (isstart(p->lastc)) {
        next_identifier(p);
        handle_identifier(p);
        break;
      }

      /// Number.
      if (isdigit(p->lastc)) {
        next_number(p);

        /// The character after a number must be a whitespace or delimiter.
        if (isalpha(p->lastc)) ERR("Invalid integer literal");
        break;
      }

      /// Anything else is invalid.
      ERR("Invalid token");
  }

  /// Set the end of the token.
  p->tok.source_location.end = (u32) (p->curr - p->source.data - 1);
}

/// ===========================================================================
///  Parser helpers.
/// ===========================================================================
/// Get the current scope.
static Scope *curr_scope(Parser *p) { return vector_back(p->ast->scope_stack); }

/// Consume a token; error if it's not the expected type.
static void consume(Parser *p, enum TokenType tt) {
  if (p->tok.type != tt) ERR("Expected %s, but got %s",
    token_type_to_string(tt), token_type_to_string(p->tok.type));
  next_token(p);
}

/// Check if a token can be a postfix operator.
static bool is_postfix_operator(enum TokenType tt) {
  switch (tt) {
    default: return false;
  }
}

/// Get the binary precedence of a token.
/// TODO: User-defined operators.
static isz binary_operator_precedence(Parser *p, Token t) {
  (void) p;
  switch (t.type) {
    case TK_DOT: return 1000000000;
    case TK_AS: return 1000;

    case TK_STAR:
    case TK_SLASH:
    case TK_PERCENT:
      return 600;

    case TK_PLUS:
    case TK_MINUS:
      return 500;

    case TK_SHL:
    case TK_SHR:
      return 400;

    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_CARET:
      return 300;

    case TK_EQ:
    case TK_NE:
    case TK_LT:
    case TK_GT:
    case TK_LE:
    case TK_GE:
      return 200;

    case TK_COLON_EQ:
    case TK_COLON_COLON:
      return 100;

    /// Not an operator.
    default: return -1;
  }
}

/// Check if an operator is right-associative.
/// TODO: User-defined operators.
static bool is_right_associative(Parser *p, Token t) {
  (void) p;
  switch (t.type) {
    case TK_STAR:
    case TK_SLASH:
    case TK_PERCENT:
    case TK_PLUS:
    case TK_MINUS:
    case TK_SHL:
    case TK_SHR:
    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_CARET:
    case TK_EQ:
    case TK_NE:
    case TK_LT:
    case TK_GT:
    case TK_LE:
    case TK_GE:
      return false;

    case TK_COLON_EQ:
    case TK_COLON_COLON:
      return true;

    /// Not an operator.
    default: return false;
  }
}

/// ===========================================================================
///  Attributes
/// ===========================================================================

STATIC_ASSERT(ATTR_COUNT == 3, "Exhaustive handling of attributes");

static struct {
  span name;
  AttributeKind kind;
} function_attributes[] = {
  {literal_span_raw("nomangle"), ATTR_NOMANGLE},
  {literal_span_raw("discardable"), ATTR_DISCARDABLE},
};

static struct {
  span name;
  AttributeKind kind;
} struct_type_attributes[] = {
  {literal_span_raw("alignas"), ATTR_ALIGNAS},
};

/// Helper to apply each attribute within attribs to function. Calls ERR
/// if a non-function attribute is present in the given attribute list.
static void apply_function_attributes(Parser *p, Type *func, Attributes attribs) {
  STATIC_ASSERT(ATTR_COUNT == 3, "Exhaustive handling of function attribute types");
  foreach(attr, attribs) {
    switch (attr->kind) {
    case ATTR_NOMANGLE: {
      func->function.nomangle = true;
    } break;
    case ATTR_DISCARDABLE: {
      func->function.discardable = true;
    } break;
    default: {
      // TODO: Actually print out the attribute string or something like that.
      ERR_AT(func->source_location, "Attribute cannot be applied to function: %d\n", attr->kind);
    }
    }
  }
}

static void apply_struct_type_attributes(Parser *p, Type *type, Attributes attribs) {
  STATIC_ASSERT(ATTR_COUNT == 3, "Exhaustive handling of type attributes");
  foreach(attr, attribs) {
    switch (attr->kind) {
    case ATTR_ALIGNAS: {
      type->structure.alignment = attr->value.integer;
    } break;
    default: {
      // TODO: Actually print out the attribute string or something like that.
      ERR_AT(type->source_location, "Attribute cannot be applied to type: %d\n", attr->kind);
    }
    }
  }
}

/// ===========================================================================
///  Parser
/// ===========================================================================
static Node *parse_expr_with_precedence(Parser *p, isz current_precedence);
static Type *parse_type(Parser *p);
static Node *parse_expr(Parser *p) { return parse_expr_with_precedence(p, 0); }

/// <expr-block>     ::= "{" { <expression> } "}"
static Node *parse_block(Parser *p, bool create_new_scope) {
  loc pos = p->tok.source_location;
  consume(p, TK_LBRACE);

  /// Create a new scope.
  if (create_new_scope) scope_push(p->ast);

  /// Collect the children.
  Nodes children = {0};
  while (p->tok.type != TK_RBRACE) vector_push(children, parse_expr(p));
  consume(p, TK_RBRACE);

  /// Pop the scope.
  if (create_new_scope) scope_pop(p->ast);

  /// Create the node.
  return ast_make_block(p->ast, pos, children);
}

/// <expr-if>        ::= IF <expression> <expr-block> [ ELSE <expr-block> ]
static Node *parse_if_expr(Parser *p) {
  /// Yeet "if".
  loc if_loc = p->tok.source_location;
  consume(p, TK_IF);

  /// Parse the condition.
  Node *cond = parse_expr(p);

  /// Parse the "then" block.
  scope_push(p->ast);
  Node *then_block = parse_expr(p);
  scope_pop(p->ast);

  /// Parse the "else" block if there is one.
  Node *else_block = NULL;
  if (p->tok.type == TK_ELSE) {
    next_token(p);
    scope_push(p->ast);
    else_block = parse_expr(p);
    scope_pop(p->ast);
  }

  /// Done.
  return ast_make_if(p->ast, if_loc, cond, then_block, else_block);
}

/// <expr-while>     ::= WHILE <expression> <expression>
static Node *parse_while_expr(Parser *p) {
  /// Yeet "while".
  loc while_loc = p->tok.source_location;
  consume(p, TK_WHILE);

  /// Parse the condition.
  Node *cond = parse_expr(p);

  /// Parse the body.
  scope_push(p->ast);
  Node *body = parse_expr(p);
  scope_pop(p->ast);

  /// Done.
  return ast_make_while(p->ast, while_loc, cond, body);
}

/// <expr-call> ::= <expression> "(" { <expression> [ "," ] } ")"
static Node *parse_call_expr(Parser *p, Node *callee) {
  consume(p, TK_LPAREN);

  /// Collect the arguments.
  Nodes args = {0};
  while (p->tok.type != TK_RPAREN) {
    vector_push(args, parse_expr(p));
    if (p->tok.type == TK_COMMA) next_token(p);
  }

  /// Done.
  Node *call = ast_make_call(p->ast, (loc){callee->source_location.start, p->tok.source_location.end}, callee, args);
  consume(p, TK_RPAREN);
  return call;
}

/// Parse and collect attributes within the `attribs` out parameter.
static void parse_attributes(Parser *p, Attributes *attribs) {
  while (p->tok.type == TK_IDENT) {
    AttributeKind attr_kind = ATTR_COUNT;
    for (size_t i = 0; i < sizeof function_attributes / sizeof *function_attributes; i++) {
      if (string_eq(function_attributes[i].name, p->tok.text)) {
        // We found an attribute!
        attr_kind = function_attributes[i].kind;
        break;
      }
    }
    if (attr_kind == ATTR_COUNT) break;

    // Yeet the attribute identifier that gave us the attribute kind.
    next_token(p);

    // If the attribute requires an argument/data to go along with it,
    // parse that HERE!
    union AttributeValue value = {0};

    // We found an attribute, add it to the list!
    Attribute new_attribute = { attr_kind, value };
    vector_push(*attribs, new_attribute);
  }
}

static void ensure_hygienic_declaration_if_within_macro(Parser *p, span ident, loc *source_location) {
  // If we are
  //   1. reading from a macro expansion, and
  //   2. encounter a variable declaration,
  // we need to **ensure** that there are no identifiers with the same
  // name passed as macro arguments (hygiene).
  if (p->macro_expansion_stack.size) {
    foreach (t, p->macro_expansion_stack.data[0].bound_arguments) {
      if ((t->token.type == TK_IDENT && string_eq(t->token.text, ident)) || (t->token.type == TK_AST_NODE && t->token.node->kind == NODE_VARIABLE_REFERENCE && string_eq(t->token.node->var->name, ident))) {
        if (source_location)
          ISSUE_DIAGNOSTIC(DIAG_NOTE, *source_location, p, "This declaration within a macro would shadow a passed identifier\n");
        ERR_AT(p->macro_expansion_stack.data[0].source_location, "Unhygienic expansion of macro. Probably need \"defines %S\" specified for macro\n", as_span(ident));
      }
    }
  }
}

/// Parse the body of a function.
///
/// This is basically just a wrapper around `parse_block()` that
/// also injects declarations for all the function parameters.
static Node *parse_function_body(Parser *p, Type *function_type, Nodes *param_decls, Attributes *attribs) {
  /// Save state.
  bool save_in_function = p->in_function;
  p->in_function = true;

  // Collect attributes and return them through an out parameter.
  parse_attributes(p, attribs);

  /// Yeet "=" if found.
  if (p->tok.type == TK_EQ) next_token(p);
  // TODO: Probably should ensure that if it's not an `=` it's a `{` (start of block)

  /// Push a new scope for the body and parameters.
  scope_push(p->ast);

  /// Create a declaration for each parameter.
  foreach (param, function_type->function.parameters) {
    Node *var = ast_make_declaration(p->ast, param->source_location, param->type, as_span(param->name), NULL);
    ensure_hygienic_declaration_if_within_macro(p, as_span(param->name), &param->source_location);
    if (!scope_add_symbol(curr_scope(p), SYM_VARIABLE, as_span(var->declaration.name), var))
      ERR_AT(var->source_location, "Redefinition of parameter '%S'", var->declaration.name);
    vector_push(*param_decls, var);
  }

  /// Parse the body.
  Node *block = parse_expr(p);

  /// Pop the scope.
  scope_pop(p->ast);
  p->in_function = save_in_function;

  /// Done.
  return block;
}

/// Parse an expression that starts with a type.
///
/// <expr-lambda>    ::= <type-function> <expr-block>
static Node *parse_type_expr(Parser *p, Type *type) {
  /// If this is a function type, then this is a lambda expression.
  if (type->kind == TYPE_FUNCTION) {
    /// Parse the function body.
    Nodes params = {0};
    Attributes attribs = {0};
    Node *body = parse_function_body(p, type, &params, &attribs);

    apply_function_attributes(p, type, attribs);
    vector_delete(attribs);

    /// Create a function for the lambda.
    string name = format("_XLambda_%Z", p->ast->counter++);
    Node *func = ast_make_function(p->ast, type->source_location, type, params, body, as_span(name));
    free(name.data);
    type->function.global = false;
    return func;
  }

  /// Otherwise, this is an error.
  /// TODO: Struct literals.
  ERR_AT(type->source_location, "Expected expression, got type");
}

/// <param-decl> ::= <decl-start> <type>
static Parameter parse_param_decl(Parser *p) {
  loc start = p->tok.source_location;

  /// Parse the name, colon, and type.
  string name = string_dup(p->tok.text);
  consume(p, TK_IDENT);
  consume(p, TK_COLON);
  Type *type = parse_type(p);

  /// Function types are converted to their corresponding pointer type
  /// when used as a parameter type.
  if (type->kind == TYPE_FUNCTION) type = ast_make_type_pointer(p->ast, type->source_location, type);

  /// Done.
  return (Parameter){.source_location = start, .type = type, .name = name};
}

static Member parse_struct_member(Parser *p) {
  loc start = p->tok.source_location;

  /// Parse the name, colon, and type.
  string name = string_dup(p->tok.text);
  consume(p, TK_IDENT);
  consume(p, TK_COLON);
  Type *type = parse_type(p);

  /// Function types are converted to their corresponding pointer type
  /// when used as a parameter type.
  if (type->kind == TYPE_FUNCTION) type = ast_make_type_pointer(p->ast, type->source_location, type);

  /// Done.
  return (Member){.source_location = start, .type = type, .name = name, .byte_offset = 0};
}

/// <type-derived>  ::= <type-array> | <type-function>
/// <type-array>    ::= <type> "[" <expression> "]"
/// <type-function> ::= <type> "(" { <param-decl> [ "," ]  } ")"
static Type *parse_type_derived(Parser *p, Type *base) {
  ASSERT(base);

  /// Parse the rest of the type.
  for (;;) {
    switch (p->tok.type) {
      /// Array type.
      case TK_LBRACK: {
        next_token(p);
        Node *size = parse_expr(p);

        /// TODO: Evaluate the size as a constant expression.
        if (size->kind != NODE_LITERAL || size->literal.type != TK_NUMBER)
          ISSUE_DIAGNOSTIC(DIAG_SORRY, size->source_location, p,
            "Non-literal array size not supported");
        usz dim = size->literal.integer;

        /// Yeet "]" and record the location.
        loc l = {.start = base->source_location.start, .end = p->tok.source_location.end};
        consume(p, TK_RBRACK);

        /// Base type must not be incomplete.
        if (type_is_incomplete(base)) ERR_AT(l, "Cannot create array of incomplete type: %T", base);

        /// Create the array type.
        base = ast_make_type_array(p->ast, l, base, dim);
      } break;

      /// Function type.
      case TK_LPAREN: {
        next_token(p);

        /// Collect the arguments.
        Parameters args = {0};
        while (p->tok.type != TK_RPAREN) {
          Parameter decl = parse_param_decl(p);
          vector_push(args, decl);
          if (p->tok.type == TK_COMMA) next_token(p);
        }


        /// Yeet ")".
        loc l = {.start = base->source_location.start, .end = p->tok.source_location.end};
        consume(p, TK_RPAREN);

        /// Create the function type.
        base = ast_make_type_function(p->ast, l, base, args);
      } break;

      /// Done.
      default: return base;
    }
  }
}

/// <type>           ::= <type-base> | <type-pointer> | <type-derived> | <type-struct>
/// <type-pointer>   ::= "@" { "@" } ( IDENTIFIER | "(" <type> ")" )
/// <type-struct>    ::= TYPE <struct-body>
/// <type-base>      ::= IDENTIFIER | INTEGER | BYTE | VOID | ARBITRARY_INT
static Type *parse_type(Parser *p) {
  loc start = p->tok.source_location;

  /// Collect pointers.
  usz level = 0;
  while (p->tok.type == TK_AT) {
    level++;
    next_token(p);
  }

  Type *out = NULL;

  /// Parse the base type.
  switch (p->tok.type) {

  case TK_IDENT: {
    /// Make sure the identifier is a type.
    Symbol *sym = scope_find_or_add_symbol(curr_scope(p), SYM_TYPE, as_span(p->tok.text), false);
    if (sym->kind != SYM_TYPE) ERR("'%S' is not a type!", as_span(p->tok.text));

    /// Create a named type from it.
    out = ast_make_type_named(p->ast, p->tok.source_location, sym);

    /// Yeet the identifier and parse the rest of the type.
    out->source_location.start = start.start;
    next_token(p);
  } break;

  /// Alternatively, we allow any type, enclosed in parens.
  case TK_LPAREN: {
    // Yeet "(" and parse the type.
    next_token(p);
    out = parse_type(p);

    /// Yeet ")" and parse the rest of the type.
    out->source_location.start = start.start;
    consume(p, TK_RPAREN);
  } break;

  // Reference
  case TK_AMPERSAND: {
    // Save start location of type and yeet "&"
    loc ref_type_loc = p->tok.source_location;
    next_token(p);
    out = parse_type(p);
    // Set end location of type
    ref_type_loc.end = out->source_location.end;

    out = ast_make_type_reference(p->ast, ref_type_loc, out);
  } break;

  // Builtin types
  case TK_VOID: {
    out = t_void;
    next_token(p);
  } break;
  case TK_BYTE: {
    out = t_byte;
    next_token(p);
  } break;
  case TK_INTEGER_KW: {
    out = t_integer;
    next_token(p);
  } break;
  case TK_ARBITRARY_INT: {
    bool is_signed = p->tok.text.data[0] == 's' || p->tok.text.data[0] == 'i';
    out = ast_make_type_integer(p->ast, p->tok.source_location, is_signed, p->tok.integer);
    next_token(p);
  } break;

  // Structure type definition
  case TK_TYPE: {
    loc type_kw_loc = p->tok.source_location;
    // Yeet TK_TYPE
    next_token(p);

    // Collect attributes for this type!
    Attributes attribs = {0};
    while (p->tok.type == TK_IDENT) {
      AttributeKind attr_kind = ATTR_COUNT;
      for (size_t i = 0; i < sizeof struct_type_attributes / sizeof *struct_type_attributes; i++) {
        if (string_eq(struct_type_attributes[i].name, p->tok.text)) {
          // We found an attribute!
          attr_kind = struct_type_attributes[i].kind;
          break;
        }
      }
      if (attr_kind == ATTR_COUNT)
        ERR("Unexpected identifier when parsing type attributes: \"%S\"", as_span(p->tok.text));

      // Yeet the attribute identifier that gave us the attribute kind.
      next_token(p);

      Attribute new_attribute = { attr_kind, {0} };

      // If the attribute requires an argument/data to go along with it,
      // parse that HERE!
      STATIC_ASSERT(ATTR_COUNT == 3, "Exhaustive handling of type attributes");
      switch (new_attribute.kind) {
      case ATTR_ALIGNAS: {
        if (p->tok.type != TK_NUMBER)
          ERR("The alignas type attribute requires an integer number");
        if (!is_power_of_two(p->tok.integer))
          ERR("The alignas type attribute requires a power of two, which %U is not", p->tok.integer);

        new_attribute.value.integer = p->tok.integer;
        // Yeet the number!
        next_token(p);
      } break;
      default:
        ERR("Invalid type attribute: %d\n", new_attribute.kind);
      }

      // We found an attribute, add it to the list!
      vector_push(attribs, new_attribute);
    }

    consume(p, TK_LBRACE);
    Members members = {0};
    while (p->tok.type != TK_RBRACE) {
      Member member_decl = parse_struct_member(p);
      vector_push(members, member_decl);
      if (p->tok.type == TK_COMMA) next_token(p);
    }
    consume(p, TK_RBRACE);
    out = ast_make_type_struct(p->ast, type_kw_loc, members);

    apply_struct_type_attributes(p, out, attribs);

  } break;

  default:
    /// Invalid base type.
    ERR("Expected base type, got %s", token_type_to_string(p->tok.type));
  }

  /// If we have pointer indirection levels, wrap the type in a pointer.
  while (level--) out = ast_make_type_pointer(p->ast, (loc){start.start--, p->tok.source_location.end}, out);

  return parse_type_derived(p, out);
}

/// <expr-decl>      ::= <decl-start> <decl-rest>
/// <decl-rest>      ::= <type-function> <expression>
///                    | <type> [ "=" <expression> ]
///                    | <decl-start> EXT <type-function>
static Node *parse_decl_rest(Parser *p, string ident, loc location) {
  /// If the next token is "ext", then this is an external declaration.
  bool is_ext = false;
  if (p->tok.type == TK_EXT) {
    is_ext = true;
    next_token(p);
  }

  /// Parse the type.
  Type *type = parse_type(p);

  /// If the type is a function type, parse the body if it isn’t extern.
  if (type->kind == TYPE_FUNCTION) {
    /// Not external.
    if (!is_ext) {
      /// Create a symbol table entry before parsing the body.
      Symbol *sym = scope_add_symbol_unconditional(curr_scope(p), SYM_FUNCTION, as_span(ident), NULL);

      if (sym->kind != SYM_FUNCTION || sym->val.node)
        ERR_AT(location, "Redefinition of symbol '%S'", as_span(ident));

      /// Parse the body, create the function, and update the symbol table.
      Nodes params = {0};
      Attributes attribs = {0};
      Node *body = parse_function_body(p, type, &params, &attribs);

      apply_function_attributes(p, type, attribs);
      vector_delete(attribs);

      Node *func = ast_make_function(p->ast, location, type, params, body, as_span(ident));
      sym->val.node = func;
      Node *funcref = ast_make_function_reference(p->ast, location, as_span(ident));
      funcref->funcref.resolved = sym;
      funcref->type = type;
      return funcref;
    }

    /// External.
    else {
      /// Create a symbol table entry.
      Symbol *sym = scope_find_or_add_symbol(curr_scope(p), SYM_FUNCTION, as_span(ident), true);
      if (sym->kind != SYM_FUNCTION || sym->val.node)
        ERR_AT(location, "Redefinition of symbol '%S'", as_span(ident));

      /// Parse the function's attributes, if any.
      Attributes attribs = {0};
      parse_attributes(p, &attribs);
      apply_function_attributes(p, type, attribs);
      vector_delete(attribs);

      /// Create the function.
      Node *func = ast_make_function(p->ast, location, type, (Nodes){0}, NULL, as_span(ident));
      // External functions should *not* be mangled
      type->function.nomangle = true;
      sym->val.node = func;
      Node *funcref = ast_make_function_reference(p->ast, location, as_span(ident));
      funcref->funcref.resolved = sym;
      funcref->type = type;
      return funcref;
    }
  }

  /// Create the declaration.
  Node *decl = ast_make_declaration(p->ast, location, type, as_span(ident), NULL);
  ensure_hygienic_declaration_if_within_macro(p, as_span(ident), &location);

  /// Make the variable static if we’re at global scope.
  decl->declaration.static_ = p->ast->scope_stack.size == 1;

  /// Add the declaration to the current scope.
  if (!scope_add_symbol(curr_scope(p), SYM_VARIABLE, as_span(ident), decl))
    ERR_AT(location, "Redefinition of symbol '%S'", ident);

  /// A non-external declaration may have an initialiser.
  if (p->tok.type == TK_EQ) {
    if (is_ext) ERR("An \"ext\" declaration may not have an initialiser");
    next_token(p);

    /// Need to do this manually because the symbol that is the variable
    /// may appear in its initialiser, albeit only in unevaluated contexts.
    decl->declaration.init = parse_expr(p);
    decl->declaration.init->parent = decl;
  }

  /// Done.
  return decl;
}

static Node *parse_declaration(Parser *p, string ident, loc location) {
  /// If the next token is a colon, then this is some sort of declaration.
  switch (p->tok.type) {
  default: break;

  case TK_COLON: {
    /// Parse the rest of the declaration.
    next_token(p);
    Node *decl = parse_decl_rest(p, ident, location);
    free(ident.data);
    return decl;
  }

  case TK_COLON_GT: {
    next_token(p);

    /// Parse the type.
    Type *type = parse_type(p);

    if (type->kind == TYPE_STRUCT) {
      Symbol *struct_decl_sym = scope_find_or_add_symbol(curr_scope(p), SYM_TYPE, as_span(ident), true);
      struct_decl_sym->val.type = type;
      Node *struct_decl = ast_make_structure_declaration(p->ast, location, struct_decl_sym);
      type->structure.decl = struct_decl;
      struct_decl->type = type;
      free(ident.data);
      return struct_decl;
    }
    free(ident.data);
    TODO("Named type alias not implemented");
  }

  case TK_COLON_COLON: {
    /// Create the declaration.
    Node *decl = ast_make_declaration(p->ast, location, NULL, as_span(ident), NULL);
    ensure_hygienic_declaration_if_within_macro(p, as_span(ident), &location);

    /// Make the variable static if we’re at global scope.
    decl->declaration.static_ = p->ast->scope_stack.size == 1;

    /// Add the declaration to the current scope.
    if (!scope_add_symbol(curr_scope(p), SYM_VARIABLE, as_span(ident), decl))
      ERR_AT(location, "Redefinition of symbol '%S'", ident);

    /// A type-inferred declaration MUST have an initialiser.
    next_token(p);

    /// Need to do this manually because the symbol that is the variable
    /// may appear in its initialiser, albeit only in unevaluated contexts.
    decl->declaration.init = parse_expr(p);
    decl->declaration.init->parent = decl;

    /// Done.
    free(ident.data);
    return decl;
  }

  } // switch (p->tok.type)
  ERR_AT(location, "Expected declaration!");
}

/// Declaration, call, or cast.
///
/// <decl-start>   ::= IDENTIFIER ":"
/// <expr-primary> ::= NUMBER | IDENTIFIER
static Node *parse_ident_expr(Parser *p) {
  /// We know that we’re looking at an identifier; save it for later.
  ASSERT(p->tok.type == TK_IDENT,
         "parse_ident_expr() may only be called with identifier token, but it was called with %s.",
         token_type_to_string(p->tok.type));
  string ident = string_dup(p->tok.text);
  loc location = p->tok.source_location;
  next_token(p);

  if (p->tok.type == TK_COLON || p->tok.type == TK_COLON_GT || p->tok.type == TK_COLON_COLON)
    return parse_declaration(p, ident, location);

  // FIXME: DONT ASSUME THIS IS A FUNCTION
  // FIXME: MOVE ALL OF THIS TO SEMANTIC ANALYSIS

  foreach_val(import, p->ast->imports) {
    if (string_eq(import->module_name, ident))
      return ast_make_module_reference(p->ast, p->tok.source_location, import);
  }

  /// Otherwise, check if the identifier is a declared symbol; if it isn’t,
  /// it can only be a function name, so add it as a symbol.
  Symbol *sym = scope_find_symbol(curr_scope(p), as_span(ident), false);

  /// If the symbol is a variable or function, then create a variable or
  /// function reference, and we’re done here.
  if (!sym || sym->kind == SYM_FUNCTION) {
    Node *ref = ast_make_function_reference(p->ast, location, as_span(ident));
    free(ident.data);
    return ref;
  }

  /// Identifier is no longer needed at this point.
  free(ident.data);
  if (sym->kind == SYM_VARIABLE) return ast_make_variable_reference(p->ast, location, sym);

  /// If the symbol is a type, then parse the rest of the type and delegate.
  if (sym->kind == SYM_TYPE) {
    Type *type = parse_type_derived(p, ast_make_type_named(p->ast, location, sym));
    return parse_type_expr(p, type);
  }

  /// Should never get here.
  UNREACHABLE();
}

/// Parse an expression. This function handles the following rules:
///
/// <expression> ::= <expr-decl>
///              | <expr-if>
///              | <expr-while>
///              | <expr-block>
///              | <expr-lambda>
///              | <expr-call>
///              | <expr-cast>
///              | <expr-subs>
///              | <expr-paren>
///              | <expr-prefix>
///              | <expr-binary>
///              | <expr-primary>
///
/// <expr-subs>    ::= <expression> "[" <expression> "]"
/// <expr-paren>   ::= "(" <expression> ")"
/// <expr-prefix>  ::= <prefix> <expression>
/// <expr-binary>  ::= <expression> <binary> <expression>
/// <expr-primary> ::= NUMBER | IDENTIFIER
static Node *parse_expr_with_precedence(Parser *p, isz current_precedence) {
  /// Left-hand side of operator.
  Node *lhs = NULL;

  /// Parse the LHS.
  switch (p->tok.type) {
    default: ERR("Expected expression, got %s", token_type_to_string(p->tok.type));

  case TK_GENSYM: {
    ASSERT(p->macro_expansion_stack.size, "Sorry, cannot currently handle GENSYM token when not inside of macro expansion");

    vector_clear(p->tok.text);
    // Cursed use of macro to append string (non-vector) to string buffer
    string generated_sym = vector_back(p->macro_expansion_stack).gensyms.data[p->tok.integer];
    vector_append(p->tok.text, generated_sym);
    // From this point on, matches TK_IDENTIFIER handling.
    p->tok.type = TK_IDENT;
    lhs = parse_ident_expr(p);
  } break;

    case TK_AST_NODE: {
      ASSERT(p->tok.node);
      if (p->tok.expr_once) {
        lhs = p->tok.node;
        next_token(p);
      } else {
        // FIXME: WE NEED TO DEEP COPY AST NODES FOR THIS TO WORK PROPERLY, AND
        // THAT IS SO MUCH WORK IT'S FUCKING INSANE. SO RIGHT NOW WE SHALLOW COPY
        // AND IT'S ON YOU IF YOU WRITE A MACRO THAT REQUIRES A DEEP COPIED AST
        // NODE EXPRESSION
        lhs = ast_make_integer_literal(p->ast, p->tok.source_location, p->tok.integer);
        *lhs = *p->tok.node;
        next_token(p);
      }
    } break;

    case TK_EXPORT: {
      // FIXME: Only allow at top level?

      // Yeet "export"
      next_token(p);

      if (p->tok.type != TK_IDENT) ERR("Expected identifier following \"export\"");
      string ident = string_dup(p->tok.text);

      next_token(p);
      lhs = parse_declaration(p, ident, p->tok.source_location);
      lhs->declaration.exported = true;

      vector_push(p->ast->exports, lhs);
    } break;

    /// An identifier can either be a declaration, function call, or cast.
    case TK_IDENT: lhs = parse_ident_expr(p); break;
    case TK_VOID:
    case TK_BYTE:
    case TK_INTEGER_KW: {
      lhs = parse_type_expr(p, parse_type(p));
    } break;
    case TK_FOR: {
      // Yeet "for"
      next_token(p);

      loc source_location = p->tok.source_location;

      Node *init = parse_expr(p);
      if (p->tok.type == TK_COMMA) next_token(p);
      Node *condition = parse_expr(p);
      if (p->tok.type == TK_COMMA) next_token(p);
      Node *iterator = parse_expr(p);
      Node *body = parse_expr(p);

      source_location.end = body->source_location.end;

      lhs = ast_make_for(p->ast, source_location, init, condition, iterator, body);

    } break;
    case TK_RETURN: {
      loc source_location = p->tok.source_location;

      // Yeet "return"
      next_token(p);

      source_location.end = p->tok.source_location.end;

      Node *expr = NULL;
      if (p->tok.type != TK_SEMICOLON)
        expr = parse_expr(p);

      if (p->tok.type != TK_SEMICOLON)
        ERR_AT(source_location, "`;` is required after return expression.");

      lhs = ast_make_return(p->ast, source_location, expr);
      next_token(p);
    } break;
    case TK_IF: lhs = parse_if_expr(p); break;
    case TK_ELSE: ERR("'else' without 'if'");
    case TK_WHILE: lhs = parse_while_expr(p); break;
    case TK_LBRACE: lhs = parse_block(p, true); break;
    case TK_NUMBER:
      lhs = ast_make_integer_literal(p->ast, p->tok.source_location, p->tok.integer);
      next_token(p);
      break;
    case TK_STRING:
      lhs = ast_make_string_literal(p->ast, p->tok.source_location, as_span(p->tok.text));
      next_token(p);
      break;
    case TK_LBRACK: {
      lhs = ast_make_compound_literal(p->ast, p->tok.source_location);
      next_token(p); //> Yeet "["
      while (p->tok.type != TK_RBRACK) {
        Node *expr = parse_expr(p);
        vector_push(lhs->literal.compound, expr);
        if (p->tok.type == TK_COMMA) next_token(p);
      }
      lhs->source_location.end = p->tok.source_location.end - 1;
      consume(p, TK_RBRACK);
    } break;
    case TK_LPAREN:
      next_token(p); //> Yeet "("
      lhs = parse_expr(p);
      consume(p, TK_RPAREN);
      break;
    case TK_RPAREN: ERR("Unmatched ')'");
    case TK_RBRACK: ERR("Unmatched ']'");
    case TK_RBRACE: ERR("Unmatched '}'");

    /// '@' is complicated because it can either be a dereference or a cast.
    case TK_AT: {
      /// Collect all at signs.
      usz at_count = 0;
      do {
        at_count++;
        next_token(p);
      } while (p->tok.type == TK_AT);

      /// If the next token can be the start of a <type-base>, then this is
      /// a type; parse the type and wrap it in a pointer type.
      if (p->tok.type == TK_IDENT) {
        Symbol *sym = scope_find_symbol(curr_scope(p), as_span(p->tok.text), false);
        if (sym && sym->kind == SYM_TYPE) {
          Type *type = ast_make_type_named(p->ast, p->tok.source_location, sym);
          next_token(p);
          while (at_count--) type = ast_make_type_pointer(p->ast, p->tok.source_location, parse_type_derived(p, type));
          lhs = parse_type_expr(p, type);
          break;
        }
      }

      /// Otherwise, this is a dereference.
      lhs = parse_expr_with_precedence(p, PREFIX_PRECEDENCE);

      /// Wrap it in an appropriate number of dereferences.
      for (usz i = 0; i < at_count; i++) {
        loc l = lhs->source_location;
        lhs = ast_make_unary(p->ast, (loc){.start = l.start - 1, .end = l.end}, TK_AT, false, lhs);
      }
    } break;

    /// Unary operators.
    case TK_MINUS:
    case TK_AMPERSAND:
    case TK_TILDE:
    case TK_EXCLAM:
    case TK_STAR: {
      u32 start = p->tok.source_location.start;
      enum TokenType tt = p->tok.type;
      next_token(p);
      Node *operand = parse_expr_with_precedence(p, PREFIX_PRECEDENCE);
      lhs = ast_make_unary(p->ast, (loc){.start = start, .end = operand->source_location.end}, tt, false, operand);
    } break;
  }

  /// This *must* not be NULL.
  if (!lhs) ICE("LHS is NULL");

  /// The rules for operator precedence parsing are as follows:
  ///     - unary prefix operators are unambiguously handled up above;
  ///     - if the current token is a, unary postfix operator, then the
  ///       current LHS is its operand;
  ///     - if the current token is a binary operator whose precedence is
  ///       higher than the current precedence, or higher than or equal to
  ///       the current precedence if the operator is right-associative, then
  ///       the current LHS is the LHS of that operator;
  ///     - if the current token is "(" or "[", then this is a call/subscript
  ///       expression. We handle these explicitly here since they usually have
  ///       the highest precedence anyway.
  ///     - otherwise, return the current LHS as its own expression.
  for (;;) {
    /// TODO: Postfix operators also need to respect precedence.
    /// Handle unary postfix operators.
    if (is_postfix_operator(p->tok.type)) {
      lhs = ast_make_unary(p->ast, (loc){.start = lhs->source_location.start, p->tok.source_location.end}, p->tok.type, true, lhs);
      next_token(p);
      continue;
    }

    /// Handle calls.
    if (p->tok.type == TK_LPAREN) {
      lhs = parse_call_expr(p, lhs);
      continue;
    }

    /// Handle subscripts.
    if (p->tok.type == TK_LBRACK) {
      next_token(p);
      Node *index = parse_expr(p);
      consume(p, TK_RBRACK);
      lhs = ast_make_binary(p->ast, (loc){.start = lhs->source_location.start, .end = index->source_location.end}, TK_LBRACK, lhs, index);
      continue;
    }

    /// Handle binary operators. We can just check if the precedence of the current
    /// token is less than the current precedence, even if the current token is not
    /// an operator because `binary_operator_precedence` returns -1 in that case.
    isz prec = binary_operator_precedence(p, p->tok);

    /// If the precedence of the current token is less than the current precedence,
    /// then we're done.
    if (prec < current_precedence) return lhs;

    /// If the precedence is the same, we’re done if the token is left-associative.
    if (prec == current_precedence && !is_right_associative(p, p->tok)) return lhs;

    /// Otherwise, we need to parse the RHS.
    enum TokenType tt = p->tok.type;
    next_token(p);

    /// The `as` operator is special because its RHS is a type.
    if (tt == TK_AS) {
      Type *type = parse_type(p);
      lhs = ast_make_cast(p->ast, (loc){.start = lhs->source_location.start, .end = type->source_location.end}, type, lhs);
      continue;
    }

    /// The `as` operator is special because its RHS is a type.
    if (tt == TK_DOT) {
      if (p->tok.type != TK_IDENT) ERR("RHS of operator '.' must be an identifier.");
      lhs = ast_make_member_access(p->ast, (loc){.start = lhs->source_location.start, .end = p->tok.source_location.end}, as_span(p->tok.text), lhs);
      // Yeet identifier token
      next_token(p);
      continue;
    }

    /// Otherwise, the RHS is a regular expression.
    else {
      Node *rhs = parse_expr_with_precedence(p, prec);

      /// Combine the LHS and RHS into a binary expression.
      lhs = ast_make_binary(p->ast, (loc){.start = lhs->source_location.start, .end = rhs->source_location.end}, tt, lhs, rhs);
    }
  }
}

/// ===========================================================================
///  API
/// ===========================================================================
AST *parse(span source, const char *filename) {
  Parser p = {0};
  p.source = source;
  p.filename = filename;
  p.curr = source.data;
  p.end = source.data + source.size - 1;
  p.raw_mode = false;
  p.lastc = ' ';
  p.ast = ast_create();
  p.ast->filename = string_create(filename);
  p.ast->source = string_dup(source);

  /// Set up error handling.
  if (setjmp(p.error_buffer)) {
    ast_free(p.ast);
    return NULL;
  }

  /// Lex the first character and token.
  next_char(&p);
  next_token(&p);

  if (p.tok.type == TK_IDENT && string_eq(p.tok.text, literal_span("module")) && !p.tok.artificial) {
    // Yeet "module"
    next_token(&p);
    if (p.tok.type != TK_IDENT)
      ISSUE_FATAL_DIAGNOSTIC(DIAG_ERR, p.tok.source_location, (&p),
                             "Expected module name following \"module\"");

    p.ast->is_module = true;
    p.ast->module_name = string_dup(p.tok.text);

    next_token(&p);
  }

  while (p.tok.type == TK_SEMICOLON) next_token(&p);

  while (p.tok.type == TK_IDENT && string_eq(p.tok.text, literal_span("import")) && !p.tok.artificial) {
    // Yeet "import"
    next_token(&p);
    if (p.tok.type != TK_IDENT)
      ISSUE_FATAL_DIAGNOSTIC(DIAG_ERR, p.tok.source_location, (&p),
                             "Expected module name following \"import\"");

    AST *module = calloc(1, sizeof(AST));
    module->module_name = string_dup(p.tok.text);
    vector_push(p.ast->imports, module);

    next_token(&p);

    while (p.tok.type == TK_SEMICOLON) next_token(&p);
  }

  /// Parse the file.
  /// <file> ::= { <expression> }
  while (p.tok.type != TK_EOF) {
    if (p.tok.type == TK_SEMICOLON) next_token(&p);
    if (p.tok.type == TK_EOF) break;
    Node *expr = parse_expr(&p);
    vector_push(p.ast->root->root.children, expr);
    expr->parent = p.ast->root;
  }
  return p.ast;
}

NODISCARD const char *token_type_to_string(enum TokenType type) {
  STATIC_ASSERT(TK_COUNT == 54, "Exhaustive handling of token types in token type to string conversion");
  switch (type) {
    case TK_COUNT:
    case TK_INVALID: return "invalid";
    case TK_EOF: return "EOF";
    case TK_IDENT: return "identifier";
    case TK_NUMBER: return "number";
    case TK_STRING: return "string";
    case TK_IF: return "if";
    case TK_ELSE: return "else";
    case TK_WHILE: return "while";
    case TK_EXT: return "ext";
    case TK_AS: return "as";
    case TK_EXPORT: return "export";
    case TK_TYPE: return "type";
    case TK_VOID: return "void";
    case TK_BYTE: return "byte";
    case TK_INTEGER_KW: return "integer";
    case TK_ARBITRARY_INT: return "arbitrary_integer";
    case TK_FOR: return "for";
    case TK_RETURN: return "return";
    case TK_LPAREN: return "\"(\"";
    case TK_RPAREN: return "\")\"";
    case TK_LBRACK: return "\"[\"";
    case TK_RBRACK: return "\"]\"";
    case TK_LBRACE: return "\"{\"";
    case TK_RBRACE: return "\"}\"";
    case TK_COMMA: return "\",\"";
    case TK_COLON: return "\":\"";
    case TK_SEMICOLON: return "\";\"";
    case TK_DOT: return "\".\"";
    case TK_PLUS: return "\"+\"";
    case TK_MINUS: return "\"-\"";
    case TK_STAR: return "\"*\"";
    case TK_SLASH: return "\"/\"";
    case TK_PERCENT: return "\"%\"";
    case TK_AMPERSAND: return "\"&\"";
    case TK_PIPE: return "\"|\"";
    case TK_CARET: return "\"^\"";
    case TK_TILDE: return "\"~\"";
    case TK_EXCLAM: return "\"!\"";
    case TK_AT: return "\"@\"";
    case TK_HASH: return "\"#\"";
    case TK_SHL: return "\"<<\"";
    case TK_SHR: return "\">>\"";
    case TK_EQ: return "\"=\"";
    case TK_NE: return "\"!=\"";
    case TK_LT: return "\"<\"";
    case TK_GT: return "\">\"";
    case TK_LE: return "\"<=\"";
    case TK_GE: return "\">=\"";
    case TK_COLON_EQ: return "\":=\"";
    case TK_COLON_COLON: return "\"::\"";
    case TK_COLON_GT: return "\":>\"";
    case TK_GENSYM: return "gensym";
    case TK_MACRO_ARG: return "macro_arg";
    case TK_AST_NODE: return "ast_node";
  }
  UNREACHABLE();
}
