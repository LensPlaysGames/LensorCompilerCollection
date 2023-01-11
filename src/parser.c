#include <ast.h>
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <parser.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector.h>

/// ===========================================================================
///  Error handling.
/// ===========================================================================
#define ISSUE_DIAGNOSTIC(sev, loc, parser, ...)                                        \
  do {                                                                                 \
    issue_diagnostic((sev), (parser)->filename, (parser)->source, (loc), __VA_ARGS__); \
    longjmp(parser->error_buffer, 1);                                                  \
  } while (0)
#define ERR_AT(loc, ...) ISSUE_DIAGNOSTIC(DIAG_ERR, loc, p, __VA_ARGS__)
#define ERR(...)         ERR_AT(p->tok.source_location, __VA_ARGS__)

/// ===========================================================================
///  Types and enums.
/// ===========================================================================
enum {
  PREFIX_PRECEDENCE = 10000,
};

typedef struct Token {
  enum TokenType type;
  loc source_location;
  span text;
  u64 integer;
} Token;

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
} keywords[5] = {
    {literal_span_raw("if"), TK_IF},
    {literal_span_raw("else"), TK_ELSE},
    {literal_span_raw("while"), TK_WHILE},
    {literal_span_raw("ext"), TK_EXT},
    {literal_span_raw("as"), TK_AS},
};

/// Check if a character may start an identifier.
static bool isstart(char c) {
  return isalpha(c) || c == '_' || c == '$' || c == '.' || c == '@';
}

/// Check if a character may be part of an identifier.
static bool iscontinue(char c) {
  return isstart(c) || isdigit(c) || c == '%';
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
  if (p->lastc == '\r') p->lastc = '\n';
}

/// Lex an identifier.
static void next_identifier(Parser *p) {
  /// The start of the identifier.
  p->tok.text.data = p->curr - 1;
  p->tok.text.size = 1;
  p->tok.type = TK_IDENT;
  next_char(p);

  /// Read the rest of the identifier.
  while (iscontinue(p->lastc)) {
    p->tok.text.size++;
    next_char(p);
  }
}

/// Parse a number.
static void parse_number(Parser *p, int base) {
  char *end;
  errno = 0;
  p->tok.integer = (u64) strtoull(p->tok.text.data, &end, base);
  if (errno == ERANGE) ERR("Integer literal too large");
  if (end != p->tok.text.data + p->tok.text.size) ERR("Invalid integer literal");
}

/// Lex a number.
static void next_number(Parser *p) {
  /// Record the start of the number.
  p->tok.text.size = 0;
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
    next_char(p);                                                           \
    p->tok.text.data = p->curr - 1;                                         \
    while (condition) {                                                     \
      p->tok.text.size++;                                                   \
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
    p->tok.text.data = p->curr - 1;
    do {
      p->tok.text.size++;
      next_char(p);
    } while (isdigit(p->lastc));
    return parse_number(p, 10);
  }

  /// Anything else is an error.
  ERR("Invalid integer literal");
}

/// Lex the next token.
static void next_token(Parser *p) {
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
      } else {
        p->tok.type = TK_COLON;
      }
      break;

    case ';':
      while (p->lastc && p->lastc != '\n') next_char(p);
      return next_token(p);

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
        p->tok.type = TK_NUMBER;
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

    /// Number or identifier.
    default:
      /// Identifier.
      if (isstart(p->lastc)) {
        next_identifier(p);

        /// Check if the identifier is a keyword.
        for (size_t i = 0; i < sizeof keywords / sizeof *keywords; i++) {
          if (string_eq(keywords[i].kw, p->tok.text)) {
            p->tok.type = keywords[i].type;
            goto done;
          }
        }
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

done:
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
      return true;

    /// Not an operator.
    default: return false;
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

/// Check if a type is valid as a declaration type.
static void validate_decltype(Parser *p, Type *type) {
  /// Strip typedefs.
  Typeinfo actual_type = ast_typeinfo(p->ast, type);

  /// Strip arrays and recursive typedefs.
  Type *base_type = actual_type.type;
  while (base_type) {
    if (base_type->kind == TYPE_NAMED) base_type = base_type->named ? base_type->named->type : NULL;
    else if (base_type->kind == TYPE_ARRAY) base_type = base_type->array.of;
    else break;
  }

  /// Make sure this isn’t an array of incomplete type.
  if (actual_type.is_incomplete || !base_type) {
    ERR_AT(type->source_location, "Cannot declare %s of incomplete type '%T'",
           actual_type.is_incomplete ? "variable" : "array", type);
  }

  if (actual_type.type->kind == TYPE_FUNCTION || base_type->kind == TYPE_FUNCTION) {
    ERR_AT(type->source_location, "Cannot declare %s of function type '%T'",
           actual_type.is_incomplete ? "variable" : "array", type);
  }
}

/// Parse the body of a function.
///
/// This is basically just a wrapper around `parse_block()` that
/// also injects declarations for all the function parameters.
static Node *parse_function_body(Parser *p, Type *function_type, Nodes *param_decls) {
  /// Save state.
  bool save_in_function = p->in_function;
  p->in_function = true;

  /// Push a new scope for the body and parameters.
  scope_push(p->ast);

  /// Create a declaration for each parameter.
  foreach (Parameter , param, function_type->function.parameters) {
    validate_decltype(p, param->type);
    Node *var = ast_make_declaration(p->ast, param->source_location, param->type, as_span(param->name), NULL);
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
/// <expr-cast>      ::= <type> <expression>
/// <expr-lambda>    ::= <type-function> <expr-block>
static Node *parse_type_expr(Parser *p, Type *type) {
  /// If this is a function type, then this is a lambda expression.
  if (type->kind == TYPE_FUNCTION) {
    /// Parse the function body.
    Nodes params = {0};
    Node *body = parse_function_body(p, type, &params);

    /// Create a function for the lambda.
    string name = format("_XLambda_%Z", p->ast->counter++);
    Node *func = ast_make_function(p->ast, type->source_location, type, params, body, as_span(name));
    free(name.data);
    func->function.global = false;
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
  span name = p->tok.text;
  consume(p, TK_IDENT);
  consume(p, TK_COLON);
  Type *type = parse_type(p);

  /// Function types are converted to their corresponding pointer type
  /// when used as a parameter type.
  if (type->kind == TYPE_FUNCTION) type = ast_make_type_pointer(p->ast, type->source_location, type);

  /// Done.
  return (Parameter){.source_location = start, .type = type, .name = string_dup(name)};
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
        if (ast_type_is_incomplete(base)) ERR_AT(l, "Cannot create array of incomplete type: %T", base);

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

/// <type>         ::= <type-base> | <type-pointer> | <type-derived>
/// <type-pointer> ::= "@" { "@" } ( IDENTIFIER | "(" <type> ")" )
/// <type-base>    ::= IDENTIFIER
static Type *parse_type(Parser *p) {
  loc start = p->tok.source_location;

  /// Collect pointers.
  usz level = 0;
  while (p->tok.type == TK_AT) {
    level++;
    next_token(p);
  }

  /// Parse the base type.
  if (p->tok.type == TK_IDENT) {
    /// Make sure the identifier is a type.
    Symbol *sym = scope_find_symbol(curr_scope(p), p->tok.text, false);
    if (!sym || sym->kind != SYM_TYPE) ERR("Unknown type '%S'", p->tok.text);

    /// Create a named type from it.
    Type *base = ast_make_type_named(p->ast, p->tok.source_location, sym);

    /// If we have pointer indirection levels, wrap the type in a pointer.
    while (level--) base = ast_make_type_pointer(p->ast, (loc){start.start--, p->tok.source_location.end}, base);

    /// Yeet the identifier and parse the rest of the type.
    base->source_location.start = start.start;
    next_token(p);
    return parse_type_derived(p, base);
  }

  /// Alternatively, we allow any type, enclosed in parens.
  if (p->tok.type == TK_LPAREN) {
    next_token(p);
    Type *base = parse_type(p);

    /// If we have pointer indirection levels, wrap the type in a pointer.
    while (level--) base = ast_make_type_pointer(p->ast, (loc){start.start--, p->tok.source_location.end}, base);

    /// Yeet ")" and parse the rest of the type.
    base->source_location.start = start.start;
    consume(p, TK_RPAREN);
    return parse_type_derived(p, base);
  }

  /// Invalid base type.
  ERR("Expected base type, got %d", (int) p->tok.type);
}

/// <expr-decl>      ::= <decl-start> <decl-rest>
/// <decl-rest>      ::= <type-function> <expression>
///                    | <type> [ "=" <expression> ]
///                    | <decl-start> EXT <type-function>
static Node *parse_decl_rest(Parser *p, Token ident) {
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
      Symbol *sym = scope_add_symbol_unconditional(curr_scope(p), SYM_FUNCTION, ident.text, NULL);

      if (sym->kind != SYM_FUNCTION || sym->node)
        ERR_AT(ident.source_location, "Redefinition of symbol '%S'", ident.text);

      /// Parse the body, create the function, and update the symbol table.
      Nodes params = {0};
      Node *body = parse_function_body(p, type, &params);
      Node *func = ast_make_function(p->ast, ident.source_location, type, params, body, ident.text);
      sym->node = func;
      Node *funcref = ast_make_function_reference(p->ast, ident.source_location, ident.text);
      funcref->funcref.resolved = sym;
      return funcref;
    }

    /// External.
    else {
      /// Create a symbol table entry.
      Symbol *sym = scope_find_or_add_symbol(curr_scope(p), SYM_FUNCTION, ident.text, true);
      if (sym->kind != SYM_FUNCTION || sym->node)
        ERR_AT(ident.source_location, "Redefinition of symbol '%S'", ident.text);

      /// Create the function.
      Node *func = ast_make_function(p->ast, ident.source_location, type, (Nodes){0}, NULL, ident.text);
      sym->node = func;
      Node *funcref = ast_make_function_reference(p->ast, ident.source_location, ident.text);
      funcref->funcref.resolved = sym;
      return funcref;
    }
  }

  /// Make sure we can perform this declaration.
  validate_decltype(p, type);

  /// Create the declaration.
  Node *decl = ast_make_declaration(p->ast, ident.source_location, type, ident.text, NULL);

  /// Make the variable static if we’re at global scope.
  decl->declaration.static_ = p->ast->scope_stack.size == 1;

  /// Add the declaration to the current scope.
  if (!scope_add_symbol(curr_scope(p), SYM_VARIABLE, ident.text, decl))
    ERR_AT(ident.source_location, "Redefinition of symbol '%S'", ident.text);

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

/// Declaration, call, or cast.
///
/// <decl-start>   ::= IDENTIFIER ":"
/// <expr-primary> ::= NUMBER | IDENTIFIER
static Node *parse_ident_expr(Parser *p) {
  /// We know that we’re looking at an identifier; save it for later.
  Token ident = p->tok;
  next_token(p);

  /// If the next token is a colon, then this is some sort of declaration.
  if (p->tok.type == TK_COLON) {
    /// Parse the rest of the declaration.
    next_token(p);
    return parse_decl_rest(p, ident);
  }

  /// Otherwise, check if the identifier is a declared symbol; if it isn’t,
  /// it can only be a function name, so add it as a symbol.
  Symbol *sym = scope_find_symbol(curr_scope(p), ident.text, false);

  /// If the symbol is a variable or function, then we’re done here.
  if (!sym || sym->kind == SYM_FUNCTION) return ast_make_function_reference(p->ast, ident.source_location, ident.text);
  if (sym->kind == SYM_VARIABLE) return ast_make_variable_reference(p->ast, ident.source_location, sym);

  /// If the symbol is a type, then parse the rest of the type and delegate.
  if (sym->kind == SYM_TYPE) {
    Type *type = parse_type_derived(p, ast_make_type_named(p->ast, ident.source_location, sym));
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

    /// An identifier can either be a declaration, function call, or cast.
    case TK_IDENT: lhs = parse_ident_expr(p); break;
    case TK_IF: lhs = parse_if_expr(p); break;
    case TK_ELSE: ERR("'else' without 'if'");
    case TK_WHILE: lhs = parse_while_expr(p); break;
    case TK_LBRACE: lhs = parse_block(p, true); break;
    case TK_NUMBER:
      lhs = ast_make_integer_literal(p->ast, p->tok.source_location, p->tok.integer);
      next_token(p);
      break;
    case TK_STRING:
      lhs = ast_make_string_literal(p->ast, p->tok.source_location, p->tok.text);
      next_token(p);
      break;
    case TK_LPAREN:
      next_token(p);
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
        Symbol *sym = scope_find_symbol(curr_scope(p), p->tok.text, false);
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
    u32 start = p->tok.source_location.start;
    enum TokenType tt = p->tok.type;
    next_token(p);

    /// The `as` operator is special because its RHS is a type.
    if (tt == TK_AS) {
      Type *type = parse_type(p);
      lhs = ast_make_cast(p->ast, (loc){.start = start, .end = type->source_location.end}, type, lhs);
      continue;
    }

    /// Otherwise, the RHS is a regular expression.
    else {
      Node *rhs = parse_expr_with_precedence(p, prec);

      /// Combine the LHS and RHS into a binary expression.
      lhs = ast_make_binary(p->ast, (loc){.start = start, .end = rhs->source_location.end}, tt, lhs, rhs);
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
  p.end = source.data + source.size;
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

  /// Parse the file.
  /// <file> ::= { <expression> }
  while (p.tok.type != TK_EOF) {
    Node *expr = parse_expr(&p);
    vector_push(p.ast->root->root.children, expr);
    expr->parent = p.ast->root;
  }
  return p.ast;
}

NODISCARD const char *token_type_to_string(enum TokenType type) {
  switch (type) {
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
  }

  return "\?\?\?";
}
