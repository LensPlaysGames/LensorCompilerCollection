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

#define ERR_DO(code, loc, ...)                                                  \
  do {                                                                          \
    issue_diagnostic(DIAG_ERR, (p)->filename, (p)->source, (loc), __VA_ARGS__); \
    code;                                                                       \
    longjmp(p->error_buffer, 1);                                                \
  } while (0)

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
  const char *kw;
  enum TokenType type;
} keywords[4] = {
    {"if", TK_IF},
    {"else", TK_ELSE},
    {"while", TK_WHILE},
    {"ext", TK_EXT},
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
  p->tok.text.data = p->curr - 1;
  p->tok.text.size = 1;
  p->tok.integer = 0;
  p->tok.type = TK_NUMBER;
  next_char(p);

  /// Discard leading zeroes.
  while (p->lastc == '0') {
    p->tok.text.size++;
    next_char(p);
  }

  /// Binary.
  if (p->lastc == 'b' || p->lastc == 'B') {
    next_char(p);
    while (p->lastc == '0' || p->lastc == '1') {
      p->tok.text.size++;
      next_char(p);
    }
    return parse_number(p, 2);
  }

  /// Octal.
  else if (p->lastc == 'o' || p->lastc == 'O') {
    next_char(p);
    while (p->lastc >= '0' && p->lastc <= '7') {
      p->tok.text.size++;
      next_char(p);
    }
    return parse_number(p, 8);
  }

  /// Hexadecimal.
  else if (p->lastc == 'x' || p->lastc == 'X') {
    next_char(p);
    while (isxdigit(p->lastc)) {
      p->tok.text.size++;
      next_char(p);
    }
    return parse_number(p, 16);
  }

  /// Any other digit means we have a decimal number.
  if (isdigit(p->lastc)) {
    do {
      p->tok.text.size++;
      next_char(p);
    } while (isdigit(p->lastc));
    return parse_number(p, 10);
  }

  /// If the next character is a space or delimiter, then this is a literal 0.
  if (isspace(p->lastc) || !isalpha(p->lastc)) return;

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
    case '#':
      while (p->lastc && p->lastc != '\n') next_char(p);
      return next_token(p);

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
          if (strncmp(keywords[i].kw, p->tok.text.data, p->tok.text.size) == 0) {
            p->tok.type = keywords[i].type;
            goto done;
          }
        }
        break;
      }

      /// Number.
      if (isdigit(p->lastc)) {
        next_number(p);
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
static Scope *curr_scope(Parser *p) { return p->ast->scopes.data[0]; }

/// Consume a token; error if it's not the expected type.
static void consume(Parser *p, enum TokenType tt) {
  if (p->tok.type != tt) ERR("Expected token of type %s, got %s",
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
static Node *parse_block(Parser *p) {
  loc pos = p->tok.source_location;
  consume(p, TK_LBRACE);

  /// Collect the children.
  Nodes children = {0};
  while (p->tok.type != TK_RBRACE) VECTOR_PUSH(children, parse_expr(p));
  consume(p, TK_RBRACE);

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
  Node *then_block = parse_block(p);

  /// Parse the "else" block if there is one.
  Node *else_block = NULL;
  if (p->tok.type == TK_ELSE) {
    next_token(p);
    else_block = parse_block(p);
  }

  /// Done.
  return ast_make_if(p->ast, if_loc, cond, then_block, else_block);
}

/// <expr-while>     ::= WHILE <expression> <expr-block>
static Node *parse_while_expr(Parser *p) {
  /// Yeet "while".
  loc while_loc = p->tok.source_location;
  consume(p, TK_WHILE);

  /// Parse the condition.
  Node *cond = parse_expr(p);

  /// Parse the body.
  Node *body = parse_block(p);

  /// Done.
  return ast_make_while(p->ast, while_loc, cond, body);
}

/// <expr-call> ::= <expression> "(" [ <expression> { "," <expression> } ] ")"
static Node *parse_call_expr(Parser *p, Node *callee) {
  loc call_loc = p->tok.source_location;
  consume(p, TK_LPAREN);

  /// Collect the arguments.
  Nodes args = {0};
  if (p->tok.type != TK_RPAREN) {
    VECTOR_PUSH(args, parse_expr(p));
    while (p->tok.type == TK_COMMA) {
      next_token(p);
      VECTOR_PUSH(args, parse_expr(p));
    }
  }
  consume(p, TK_RPAREN);

  /// Done.
  return ast_make_call(p->ast, call_loc, callee, args);
}

/// Parse the body of a function.
///
/// This is basically just a wrapper around `parse_block()` that
/// also injects declarations for all the function parameters.
static Node *parse_function_body(Parser *p, Type *function_type) {
  /// Save state.
  bool save_in_function = p->in_function;
  p->in_function = true;

  /// Push a new scope and add the parameters to it.
  scope_push(p->ast);

  /// Create a declaration for each parameter.
  Nodes body_exprs = {0};
  VECTOR_FOREACH (Parameter , param, function_type->function.parameters) {
    Node *var = ast_make_declaration(p->ast, param->source_location, param->type, as_span(param->name), NULL);
    scope_add_symbol(curr_scope(p), SYM_VARIABLE, as_span(var->declaration.name), var);
    VECTOR_PUSH(body_exprs, var);
  }

  /// Parse the body.
  /// TODO: We could also just allow <expression> here.
  Node *expr = parse_block(p);
  VECTOR_APPEND_ALL(body_exprs, expr->block.children);
  VECTOR_CLEAR(expr->block.children);

  /// Pop the scope created for the function body.
  scope_pop(p->ast);
  p->in_function = save_in_function;

  /// Create a block to hold the parameters and the body.
  return ast_make_block(p->ast, expr->source_location, body_exprs);
}

/// Parse an expression that starts with a type.
///
/// <expr-cast>      ::= <type> <expression>
/// <expr-lambda>    ::= <type-function> <expr-block>
static Node *parse_type_expr(Parser *p, Type *type) {
  /// If this is a function type, and the next token is "{", then this
  /// is a lambda expression.
  if (type->kind == TYPE_FUNCTION && p->tok.type == TK_LBRACE) {
    /// Parse the function body.
    Node *body = parse_function_body(p, type);

    /// Create a function for the lambda.
    char num[64] = {0};
    usz sz = (usz) snprintf(num, sizeof num, "_XLambda_%zu", p->ast->counter++);
    return ast_make_function(p->ast, type->source_location, type, body, (span){.data = num, .size = sz});
  }

  /// Otherwise, this is a cast expression.
  return ast_make_cast(p->ast, type->source_location, type, parse_expr(p));
}

/// <param-decl> ::= <decl-start> <type>
static Parameter parse_param_decl(Parser *p) {
  loc start = p->tok.source_location;

  /// Parse the name, colon, and type.
  span name = p->tok.text;
  consume(p, TK_IDENT);
  consume(p, TK_COLON);
  Type *type = parse_type(p);

  /// Done.
  return (Parameter){.source_location = start, .type = type, .name = string_dup(name)};
}

/// <type-derived>  ::= <type-array> | <type-function>
/// <type-array>    ::= <type> "[" <expression> "]"
/// <type-function> ::= <type> "(" [ <param-decl> { "," <param-decl>  } ] ")"
static Type *parse_type_derived(Parser *p, Type *base) {
  ASSERT(base);

  /// Parse the rest of the type.
  for (;;) {
    switch (p->tok.type) {
      /// Array type.
      case TK_LBRACK: {
        next_token(p);
        Node *size = parse_expr(p);
        consume(p, TK_RBRACK);

        /// TODO: Evaluate the size as a constant expression.
        if (size->kind != NODE_LITERAL || size->literal.type != TK_NUMBER)
          ISSUE_DIAGNOSTIC(DIAG_SORRY, size->source_location, p,
            "Non-literal array size not supported");
        usz dim = size->literal.integer;

        /// Base type must not be incomplete.
        if (ast_type_is_incomplete(base)) {
          string name = ast_typename(base, false);
          ERR_DO(free(name.data), base->source_location, "Cannot create array of incomplete type: %.*s",
            (int) name.size, name.data);
        }

        /// Create the array type.
        base = ast_make_type_array(p->ast, base->source_location, base, dim);
      } break;

      /// Function type.
      case TK_LPAREN: {
        loc fn_loc = p->tok.source_location;
        next_token(p);

        /// Collect the arguments.
        Parameters args = {0};
        if (p->tok.type != TK_RPAREN) {
          VECTOR_PUSH(args, parse_param_decl(p));
          while (p->tok.type == TK_COMMA) {
            next_token(p);
            VECTOR_PUSH(args, parse_param_decl(p));
          }
        }
        consume(p, TK_RPAREN);

        /// Create the function type.
        base = ast_make_type_function(p->ast, fn_loc, base, args);
      } break;

      /// Done.
      default: return base;
    }
  }
}

/// <type>      ::= <type-base> | <type-rest>
/// <type-base> ::= [ "@" ] IDENTIFIER
static Type *parse_type(Parser *p) {
  /// Collect pointers.
  usz level = 0;
  while (p->tok.type == TK_AT) {
    level++;
    next_token(p);
  }

  /// Parse the base type. Currently, this can only be an identifier.
  loc start = p->tok.source_location;
  if (p->tok.type == TK_IDENT) {
    /// Make sure the identifier is a type.
    Symbol *sym = scope_find_symbol(curr_scope(p), p->tok.text, false);
    if (!sym || sym->kind != SYM_TYPE) ERR("Unknown type '%.*s'", (int) p->tok.text.size, p->tok.text.data);

    /// Create a named type from it.
    Type *base = ast_make_type_named(p->ast, p->tok.source_location, sym);

    /// If we have pointer indirection levels, wrap the type in a pointer.
    while (level--) base = ast_make_type_pointer(p->ast, (loc){start.start--, p->tok.source_location.end}, base);

    /// Yeet the identifier and parse the rest of the type.
    next_token(p);
    return parse_type_derived(p, base);
  }

  /// Invalid base type.
  ERR("Expected base type, got %d", p->tok.type);
}

/// <expr-decl>      ::= <decl-start> <decl-rest>
/// <decl-rest>      ::= <type-function> <expr-block>
///                    | <type> [ "=" <expression> ]
///                    | <decl-start> EXT <type-function>
static Node *parse_decl_rest(Parser *p, Token ident) {
  /// Re-declaring symbols is not allowed.
  if (scope_find_symbol(curr_scope(p), ident.text, true))
    ERR("Redeclaration of symbol '%.*s'", (int) ident.text.size, ident.text.data);

  /// If the next token is "ext", then this is an external declaration.
  bool is_ext = false;
  if (p->tok.type == TK_EXT) {
    is_ext = true;
    next_token(p);
  }

  /// Parse the type.
  Type *type = parse_type(p);

  /// If the next token is "{", and the type is a function type, and this
  /// is not an external declaration, then this is a function definition.
  /// TODO: are we handling external symbols correctly?
  if (!is_ext && p->tok.type == TK_LBRACE && type->kind == TYPE_FUNCTION) {
    /// Parse the body, create the function, and add it to the symbol table.
    Node *body = parse_function_body(p, type);
    Node *func = ast_make_function(p->ast, ident.source_location, type, body, ident.text);
    Symbol *sym = scope_add_symbol(curr_scope(p), SYM_FUNCTION, ident.text, func);
    return ast_make_function_reference(p->ast, ident.source_location, sym);
  }

  /// Otherwise, this is a variable declaration.
  Node *decl = ast_make_declaration(p->ast, ident.source_location, type, ident.text, NULL);
  decl->declaration.static_ = !p->in_function;

  /// Add the declaration to the current scope.
  scope_add_symbol(curr_scope(p), SYM_VARIABLE, ident.text, decl);

  /// A non-external declaration may have an initialiser.
  /// TODO: Should we just allow this instead?
  if (p->tok.type == TK_EQ) {
    if (is_ext) ERR("An \"ext\" declaration may not have an initialiser");
    next_token(p);

    /// Need to do this manually because the symbol that is the variable
    /// may appear in its initialiser, albeit only in unevaluated contexts.
    decl->declaration.init = parse_expr(p);
    decl->declaration.init->parent = decl;
  }

  /// Strip array types and resolve named types.
  Type *const saved_type = type;
  while (type) {
    if (type->kind == TYPE_NAMED) type = type->named->type;
    else if (type->kind == TYPE_ARRAY) type = type->array.of;
    else break;
  }

  /// Make sure we’re not trying to declare a variable of incomplete type.
  if (!type) {
    string name = ast_typename(saved_type, false);
    ERR_DO(free(name.data), decl->source_location, "Cannot declare a variable of incomplete type \"%.*s\"",
      (int) name.size, name.data);
  }

  /// Done.
  return decl;
}

/// This function is a bit complicated because there are many rules in the
/// grammar that (may), directly or indirectly, start with an identifier.
///
/// <decl-start>   ::= IDENTIFIER ":"
/// <type>         ::= IDENTIFIER | ...
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
  Symbol *sym = scope_find_or_add_symbol(curr_scope(p), SYM_FUNCTION, ident.text, false);

  /// If the symbol is a variable or function, then we’re done here.
  if (sym->kind == SYM_VARIABLE) return ast_make_variable_reference(p->ast, ident.source_location, sym);
  if (sym->kind == SYM_FUNCTION) return ast_make_function_reference(p->ast, ident.source_location, sym);

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
    case TK_LBRACE: lhs = parse_block(p); break;
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
    Node *rhs = parse_expr_with_precedence(p, prec);

    /// Combine the LHS and RHS into a binary expression.
    lhs = ast_make_binary(p->ast, (loc){.start = start, .end = rhs->source_location.end}, tt, lhs, rhs);
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
    VECTOR_PUSH(p.ast->root->root.children, expr);
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
    case TK_LPAREN: return "(";
    case TK_RPAREN: return ")";
    case TK_LBRACK: return "[";
    case TK_RBRACK: return "]";
    case TK_LBRACE: return "{";
    case TK_RBRACE: return "}";
    case TK_COMMA: return ",";
    case TK_COLON: return ":";
    case TK_SEMICOLON: return ";";
    case TK_PLUS: return "+";
    case TK_MINUS: return "-";
    case TK_STAR: return "*";
    case TK_SLASH: return "/";
    case TK_PERCENT: return "%";
    case TK_AMPERSAND: return "&";
    case TK_PIPE: return "|";
    case TK_CARET: return "^";
    case TK_TILDE: return "~";
    case TK_EXCLAM: return "!";
    case TK_AT: return "@";
    case TK_SHL: return "<<";
    case TK_SHR: return ">>";
    case TK_EQ: return "=";
    case TK_NE: return "!=";
    case TK_LT: return "<";
    case TK_GT: return ">";
    case TK_LE: return "<=";
    case TK_GE: return ">=";
    case TK_COLON_EQ: return ":=";
  }

  return "\?\?\?";
}