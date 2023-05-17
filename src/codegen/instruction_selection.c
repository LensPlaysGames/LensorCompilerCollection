#include <codegen/instruction_selection.h>

#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <codegen/machine_ir.h>
#include <error.h>
#include <inttypes.h>
#include <platform.h>
#include <utils.h>
#include <vector.h>

#include <ctype.h>
#include <errno.h>
#include <stdbool.h>

#define ISSUE_DIAGNOSTIC(sev, loc, parser, ...)                         \
  do {                                                                  \
    issue_diagnostic((sev), (parser)->source_filepath, (parser)->source, (loc), __VA_ARGS__); \
  } while (0)

#define ISSUE_FATAL_DIAGNOSTIC(sev, loc, parser, ...)                   \
  do {                                                                  \
    issue_diagnostic((sev), (parser)->source_filepath, (parser)->source, (loc), __VA_ARGS__); \
    exit(0);                                                            \
  } while (0)

#define WARN_AT(loc, ...) ISSUE_DIAGNOSTIC(DIAG_WARN, (loc), p, __VA_ARGS__)
#define WARN(...) WARN_AT(p->tok.source_location, __VA_ARGS__)

#define ERR_AT(loc, ...) ISSUE_FATAL_DIAGNOSTIC(DIAG_ERR, (loc), p, __VA_ARGS__)
#define ERR(...) ERR_AT(p->tok.source_location, __VA_ARGS__)

typedef enum ISelTokenKind {
  TOKEN_EOF = 0,

  TOKEN_OPKIND_REGISTER = MIR_OP_REGISTER,
  TOKEN_OPKIND_IMMEDIATE = MIR_OP_IMMEDIATE,
  TOKEN_OPKIND_BLOCK = MIR_OP_BLOCK,
  TOKEN_OPKIND_FUNCTION = MIR_OP_FUNCTION,
  TOKEN_OPKIND_NAME = MIR_OP_NAME,
  TOKEN_OPKIND_STATIC = MIR_OP_STATIC_REF,
  TOKEN_OPKIND_LOCAL = MIR_OP_LOCAL_REF,

  // ASCII simply-lexed Operators/Punctuation
  TOKEN_LPAREN = '(',
  TOKEN_RPAREN = ')',
  TOKEN_LBRACE = '{',
  TOKEN_RBRACE = '}',
  TOKEN_LBRACKET = '[',
  TOKEN_RBRACKET = ']',
  TOKEN_LT = '<',
  TOKEN_GT = '>',
  TOKEN_EQ = '=',
  TOKEN_COMMA = ',',

  TOKEN_SEMICOLON,

  // Keywords
  TOKEN_KW_MATCH,
  TOKEN_KW_EMIT,

  TOKEN_IDENTIFIER,
  TOKEN_INTEGER = '0',
} ISelTokenKind;

static const char *isel_token_kind_to_string(ISelTokenKind kind) {
  switch (kind) {
  case TOKEN_EOF: return "EOF";
  case TOKEN_OPKIND_REGISTER: return "OpKind Register";
  case TOKEN_OPKIND_IMMEDIATE: return "OpKind Immediate";
  case TOKEN_OPKIND_BLOCK: return "OpKind Block";
  case TOKEN_OPKIND_FUNCTION: return "OpKind Function";
  case TOKEN_OPKIND_NAME: return "OpKind Name";
  case TOKEN_OPKIND_STATIC: return "OpKind Static";
  case TOKEN_OPKIND_LOCAL: return "OpKind Local";
  case TOKEN_LPAREN: return "(";
  case TOKEN_RPAREN: return ")";
  case TOKEN_LBRACE: return "{";
  case TOKEN_RBRACE: return "}";
  case TOKEN_LBRACKET: return "[";
  case TOKEN_RBRACKET: return "]";
  case TOKEN_LT: return "<";
  case TOKEN_GT: return ">";
  case TOKEN_EQ: return "=";
  case TOKEN_COMMA: return ",";
  case TOKEN_SEMICOLON: return ";";
  case TOKEN_KW_MATCH: return "match";
  case TOKEN_KW_EMIT: return "emit";
  case TOKEN_IDENTIFIER: return "identifier";
  }
  UNREACHABLE();
}

typedef enum ISelEnvironmentEntryKind {
  ISEL_ENV_OPCODE,
  ISEL_ENV_OP_KIND,
} ISelEnvironmentEntryKind;

typedef struct ISelValue {
  ISelEnvironmentEntryKind kind;

  isz integer;
  string_buffer text;
} ISelValue;

typedef struct ISelEnvironmentEntry {
  string key;
  ISelValue value;
} ISelEnvironmentEntry;

typedef struct ISelEnvironment {
  usz size;
  usz capacity;
  ISelEnvironmentEntry *data;
} ISelEnvironment;

typedef struct ISelToken {
  ISelTokenKind kind;

  usz integer;
  string_buffer text;

  loc source_location;
} ISelToken;

typedef struct ISelParser {
  const char *source_filepath;
  span source;
  ISelToken tok;

  // Does not change as things are parsed.
  ISelEnvironment global;

  const char *beg;
  const char *end;
  isz lastc;

} ISelParser;

static usz sdbm(const unsigned char *str) {
  usz hash = 0;
  usz c;

  while ((c = *str++))
    hash = c + (hash << 6) + (hash << 16) - hash;

  return hash;
}

static ISelEnvironment isel_env_create(usz initial_capacity) {
  ISelEnvironment out = {0};
  out.capacity = initial_capacity;
  out.data = calloc(out.capacity, sizeof(out.data[0]));
  return out;
}

static void isel_env_delete(ISelEnvironment *env) {
  ASSERT(env, "Invalid argument");
  for (usz i = 0; i < env->capacity; ++i) {
    ISelEnvironmentEntry *entry = env->data + i;
    if (entry->key.data) free(entry->key.data);
    if (entry->value.text.data) free(entry->value.text.data);
  }
  if (env->data) free(env->data);
}

/// Return pointer to environment entry associated with the given key.
static ISelEnvironmentEntry *isel_env_entry(ISelEnvironment *env, const char *key) {
  ASSERT(env, "Invalid argument");
  ASSERT(env->capacity, "Zero capacity environment; forgot to initialise?");
  ASSERT(key, "Invalid argument");

  usz key_length = strlen(key);

  usz hash = sdbm((const unsigned char*)key);
  usz index = hash % env->capacity;

  ISelEnvironmentEntry *entry = NULL;

  // Begin searching for a matching or empty slot.
  size_t index_it = index;
  for (; index_it < env->capacity; ++index_it) {
    entry = env->data + index_it;
    // Empty entry
    if (entry->key.data == NULL) return entry;
    // If the lengths don't match, they aren't equal.
    if (entry->key.size != key_length) continue;
    // If the lengths match, compare data.
    if (memcmp(entry->key.data, key, key_length) == 0) return entry;
  }

  index_it = 0;
  for (; index_it < index; ++index_it) {
    entry = env->data + index_it;
    // Empty entry
    if (entry->key.data == NULL) return entry;
    // If the lengths don't match, they aren't equal.
    if (entry->key.size != key_length) continue;
    // If the lengths match, compare data.
    if (memcmp(entry->key.data, key, key_length) == 0) return entry;
  }

  TODO("Handle expansion of ISel environment");

  /*
  // Expand and try again.
  // FIXME: It's probably best to expand BEFORE the hash table is
  // completely full...
  isel_env_expand(env);
  return isel_env_entry(env, key);
  */
}

static void isel_env_print_entry(ISelEnvironmentEntry *entry) {
  ASSERT(entry, "Invalid argument");
  print("%S (kind %d | integer %d | text \"%S\")\n", entry->key, entry->value.kind, entry->value.integer, entry->value.text);
}
static void isel_env_print(ISelEnvironment *env) {
  for (usz i = 0; i < env->capacity; ++i) {
    ISelEnvironmentEntry *entry = env->data + i;
    if (entry->key.data) isel_env_print_entry(entry);
  }
}

static ISelEnvironmentEntry *isel_env_insert(ISelEnvironment *env, const char *key, ISelValue to_insert) {
  ISelEnvironmentEntry *entry = isel_env_entry(env, key);
  entry->key = string_create(key);
  entry->value = to_insert;
  return entry;
}

static void isel_env_add_opcode(ISelEnvironment *env, const char *key, usz opcode) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_OPCODE;
  newval.integer = (isz)opcode;
  isel_env_insert(env, key, newval);

  //ISelEnvironmentEntry *entry = isel_env_entry(env, key);
  //isel_env_print_entry(entry);
}

static void isel_env_add_op_kind(ISelEnvironment *env, const char *key, usz opkind) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_OPCODE;
  newval.integer = (isz)opkind;
  isel_env_insert(env, key, newval);

  //ISelEnvironmentEntry *entry = isel_env_entry(env, key);
  //isel_env_print_entry(entry);
}

static void isel_env_init_common_opcodes(ISelEnvironment *env) {
#define ADD_OPCODE(opcode, ...) isel_env_add_opcode(env, STR(CAT(MIR_, opcode)), CAT(MIR_, opcode));
  ALL_IR_INSTRUCTION_TYPES(ADD_OPCODE);
#undef ADD_OPCODE
}

void isel_env_test() {
  ISelEnvironment env = isel_env_create(1024);
  isel_env_init_common_opcodes(&env);
  isel_env_delete(&env);
}


static void isel_next_c(ISelParser *p) {
   /// Keep returning EOF once EOF has been reached.
  if (p->beg >= p->end) {
    // beg is >= end, must be EOF
    p->lastc = 0;
    return;
  }

  /// Read the next character.
  // TODO: Read next utf8 codepoint.
  p->lastc = *p->beg++;
  if (p->lastc == 0) return;

  /// Collapse CRLF and LFCR to a single newline,
  /// but keep CRCR and LFLF as two newlines.
  if (p->lastc == '\r' || p->lastc == '\n') {
    /// Two newlines in a row.
    if (p->beg != p->end && (*p->beg == '\r' || *p->beg == '\n')) {
      bool same = p->lastc == *p->beg;
      p->lastc = '\n';

      /// CRCR or LFLF
      if (same) return;

      /// CRLF or LFCR
      p->beg++;
    }

    /// Either CR or LF followed by something else.
    p->lastc = '\n';
  }
}

static bool isel_isstart(isz codepoint) {
  if (codepoint <= INT8_MAX) return isalpha((char)codepoint) || codepoint == '_';
  // Let unicode through by default (probably a bad idea).
  return true;
}
static bool isel_iscontinue(isz codepoint) {
  if (codepoint <= INT8_MAX) return isel_isstart(codepoint) || isdigit((char)codepoint) || codepoint == '$' || codepoint == '<' || codepoint == '>' || codepoint == '!';
  // Let unicode through by default (probably a bad idea).
  return true;
}

/// Lex an identifier.
static void isel_next_identifier(ISelParser *p) {
  /// The start of the identifier.
  DBGASSERT(isel_isstart(p->lastc), "");
  isel_next_c(p);

  p->tok.kind = TOKEN_IDENTIFIER;

  /// Read the rest of the identifier.
  while (isel_iscontinue(p->lastc)) {
    ASSERT(p->lastc <= INT8_MAX, "TODO: Convert utf8 codepoint to encoded bytes and write them to string buffer");
    vector_push(p->tok.text, (char)p->lastc);
    isel_next_c(p);
  }
}
/// Parse a number.
static void isel_parse_number(ISelParser *p, int base) {
  /// Zero-terminate the string or else `strtoull()` might try
  /// to convert data left over from the previous token.
  string_buf_zterm(&p->tok.text);

  /// Convert the number.
  char *end;
  errno = 0;
  p->tok.integer = (u64) strtoull(p->tok.text.data, &end, base);
  if (errno == ERANGE) ICE("Integer literal too large");
  if (end != p->tok.text.data + p->tok.text.size) ICE("Invalid integer literal");
}

/// Lex a number.
static void isel_next_number(ISelParser *p) {
  /// Record the start of the number.
  vector_clear(p->tok.text);
  p->tok.integer = 0;
  p->tok.kind = TOKEN_INTEGER;

  /// At least one leading zero.
  if (p->lastc == '0') {
    /// Discard the zero.
    isel_next_c(p);

    /// Another zero is an error.
    if (p->lastc == '0') ICE("Leading zeroes are not allowed in decimal literals. Use 0o/0O for octal literals.");

#define DO_PARSE_NUMBER(name, chars, condition, base)                       \
  /** Read all chars that are part of the literal. **/                      \
  if (p->lastc == chars[0] || p->lastc == chars[1]) {                       \
    /** Yeet the prefix. **/                                                \
    isel_next_c(p);                                                         \
                                                                            \
    /** Lex the digits. **/                                                 \
    while (condition) {                                                     \
      vector_push(p->tok.text, (char)p->lastc);                             \
      isel_next_c(p);                                                       \
    }                                                                       \
                                                                            \
    /** We need at least one digit. **/                                     \
    p->tok.source_location.end = (u32) ((p->beg - 1) - p->source.data);     \
    if (p->tok.text.size == 0) ICE("Expected at least one " name " digit"); \
                                                                            \
    /** Actually parse the number. **/                                      \
    return isel_parse_number(p, base);                                      \
  }

    DO_PARSE_NUMBER("binary", "bB", p->lastc == '0' || p->lastc == '1', 2)
    DO_PARSE_NUMBER("octal", "oO", isdigit((char)p->lastc) && p->lastc < '8', 8)
    DO_PARSE_NUMBER("hexadecimal", "xX", isxdigit((char)p->lastc), 16)

#undef DO_PARSE_NUMBER

    /// If the next character is a space or delimiter, then this is a literal 0.
    if (isspace((char)p->lastc) || !isalpha((char)p->lastc)) return;

    /// Anything else is an error.
    ICE("Invalid integer literal");
  }

  /// Any other digit means we have a decimal number.
  if (isdigit((char)p->lastc)) {
    do {
      vector_push(p->tok.text, (char)p->lastc);
      isel_next_c(p);
    } while (isdigit((char)p->lastc));
    return isel_parse_number(p, 10);
  }

  /// Anything else is an error.
  ICE("Invalid integer literal");
}

static const struct {
  span kw;
  ISelTokenKind kind;
} keywords[11] = {
  {literal_span_raw("match"), TOKEN_KW_MATCH},
  {literal_span_raw("emit"), TOKEN_KW_EMIT},
  {literal_span_raw("Register"), TOKEN_OPKIND_REGISTER},
  {literal_span_raw("Immediate"), TOKEN_OPKIND_IMMEDIATE},
  {literal_span_raw("Name"), TOKEN_OPKIND_NAME},
  {literal_span_raw("Block"), TOKEN_OPKIND_BLOCK},
  {literal_span_raw("Function"), TOKEN_OPKIND_FUNCTION},
  {literal_span_raw("Local"), TOKEN_OPKIND_LOCAL},
  {literal_span_raw("Static"), TOKEN_OPKIND_STATIC},
  {literal_span_raw("REG"), TOKEN_OPKIND_REGISTER},
  {literal_span_raw("IMM"), TOKEN_OPKIND_IMMEDIATE},
};

/// Lex the next token.
static void isel_next_tok(ISelParser *p) {
  /// Keep returning EOF once EOF has been reached.
  if (!p->lastc) {
    // Last codepoint is 0 so must be EOF
    p->tok.kind = TOKEN_EOF;
    return;
  }

  /// Set the token to invalid in case there is an error.
  p->tok.kind = TOKEN_EOF;

  /// Skip whitespace.
  while (isspace((char)p->lastc)) isel_next_c(p);

  /// Start of the token.
  p->tok.source_location.start = (u32) (p->beg - p->source.data - 1);

  /// Lex the token.
  switch (p->lastc) {

    /// EOF
    case '\0':
      // Reached null byte in input, must be EOF.
      p->tok.kind = TOKEN_EOF;
      break;

    case TOKEN_LPAREN: FALLTHROUGH;
    case TOKEN_RPAREN: FALLTHROUGH;
    case TOKEN_LBRACE: FALLTHROUGH;
    case TOKEN_RBRACE: FALLTHROUGH;
    case TOKEN_LBRACKET: FALLTHROUGH;
    case TOKEN_RBRACKET: FALLTHROUGH;
    case TOKEN_LT: FALLTHROUGH;
    case TOKEN_GT: FALLTHROUGH;
    case TOKEN_EQ: FALLTHROUGH;
    case TOKEN_COMMA:
      p->tok.kind = (int)p->lastc;
      isel_next_c(p);
      break;

    case ';': {
      p->tok.kind = TOKEN_SEMICOLON;
      // Yeet ';'
      isel_next_c(p);
      // Line comments begin with `;;`
      if (p->lastc == ';') {
        while (p->lastc && p->lastc != '\n') isel_next_c(p);
        isel_next_tok(p);
      }
    } break;

    default: {
      /// Identifier
      if (isel_isstart(p->lastc)) {
        vector_clear(p->tok.text);
        // TODO: Push utf8 encoding of codepoint.
        vector_push(p->tok.text, (char)p->lastc);
        isel_next_identifier(p);

        for (size_t i = 0; i < sizeof keywords / sizeof *keywords; i++) {
          if (string_eq(keywords[i].kw, p->tok.text)) {
            p->tok.kind = keywords[i].kind;
            /// Set the end of the token before returning.
            p->tok.source_location.end = (u32) (p->beg - p->source.data - 1);
            return;
          }
        }

        break;
      }

      /// Number
      if (isdigit((char)p->lastc)) {
        isel_next_number(p);

        /// The character after a number must be a whitespace or delimiter.
        if (isel_isstart(p->lastc)) ICE("Junk after integer literal");
        break;
      }

      /// Anything else is invalid.
      ICE("ISel: unknown how to handle codepoint %I at (%u, %u)", p->lastc, p->tok.source_location.start, p->tok.source_location.end);

    } break;
  }

  /// Set the end of the token.
  p->tok.source_location.end = (u32) (p->beg - p->source.data - 1);
}

/// Consume a token; error if it's not the expected type.
static void isel_consume(ISelParser *p, ISelTokenKind kind) {
  if (p->tok.kind != kind) {
    ERR("Expected %s, but got %s",
        isel_token_kind_to_string(kind), isel_token_kind_to_string(p->tok.kind));
  }
  isel_next_tok(p);
}

static bool isel_token_kind_is_opkind(ISelTokenKind kind) {
  return kind == TOKEN_OPKIND_IMMEDIATE
    || kind == TOKEN_OPKIND_REGISTER
    || kind == TOKEN_OPKIND_BLOCK
    || kind == TOKEN_OPKIND_FUNCTION
    || kind == TOKEN_OPKIND_NAME
    || kind == TOKEN_OPKIND_LOCAL
    || kind == TOKEN_OPKIND_STATIC;
}

static MIROperandKind isel_token_kind_to_opkind(ISelTokenKind kind) {
  switch (kind) {
  case TOKEN_OPKIND_IMMEDIATE: return MIR_OP_IMMEDIATE;
  case TOKEN_OPKIND_REGISTER: return MIR_OP_REGISTER;
  case TOKEN_OPKIND_BLOCK: return MIR_OP_BLOCK;
  case TOKEN_OPKIND_FUNCTION: return MIR_OP_FUNCTION;
  case TOKEN_OPKIND_NAME: return MIR_OP_NAME;
  case TOKEN_OPKIND_LOCAL: return MIR_OP_LOCAL_REF;
  case TOKEN_OPKIND_STATIC: return MIR_OP_STATIC_REF;
  default: break;
  }
  ICE("Cannot convert token kind %s into MIROperandKind", isel_token_kind_to_string(kind));
}

static MIROperandKind isel_parse_operand_kind(ISelParser *p) {
  MIROperandKind out = isel_token_kind_to_opkind(p->tok.kind);
  isel_next_tok(p);
  return out;
}

static ISelValue isel_parse_expression(ISelParser *p) {
  // number or bound identifier
  if (p->tok.kind == TOKEN_IDENTIFIER) {
    // Lookup identifier in global environment
    ISelEnvironmentEntry *entry = isel_env_entry(&p->global, p->tok.text.data);
    // FIXME: Should be ERR
    if (!entry->key.data) WARN("Expected expression, got unknown identifier \"%S\"", as_span(p->tok.text));

    // Yeet identifier
    isel_next_tok(p);

    return entry->value;
  }

  UNREACHABLE();
}

/// <operand> ::= [ OPERAND_KIND ] IDENTIFIER [ "=" <expression> ] [ "," ]
static MIROperand isel_parse_operand(ISelParser *p) {
  MIROperand out = {0};
  out.kind = MIR_OP_IMMEDIATE;

  // Either parse (OPKIND IDENTIFER) and create binding or (IDENTIFIER) with binding lookup
  if (isel_token_kind_is_opkind(p->tok.kind)) {
    // Parse operand kind (should be one of TOKEN_OPKIND_*)
    out.kind = isel_parse_operand_kind(p);
    // Optionally parse identifier (name bound to this operand while parsing this match)
    if (p->tok.kind == TOKEN_IDENTIFIER) {
      // TODO: Bind name in local match environment
      isel_next_tok(p);
    }
  } else {
    // Ensure it is an identifier.
    if (p->tok.kind != TOKEN_IDENTIFIER)
      ERR("Expected operand kind or operand identifier, but got %s instead", isel_token_kind_to_string(p->tok.kind));

    // TODO: Lookup bound name and ensure it is bound to a valid operand.
    WARN("TODO: Lookup identifier in local environment and ensure it is bound to an operand reference.\n");

    // Yeet operand identifier
    isel_next_tok(p);
  }

  // If an "=" is parsed, parse an initialising expression and
  // use it's value to set the operand value. Ensure type makes sense
  // (i.e. don't initialise a register operand with an identifier).
  if (p->tok.kind == TOKEN_EQ) {
    // Yeet '='
    isel_next_tok(p);

    // Parse expression
    isel_parse_expression(p);
  }

  // Eat comma, if present
  if (p->tok.kind == TOKEN_COMMA) isel_next_tok(p);

  return out;
}


// XXX TODO YYY
// Hey stupidface, here's your todo:
// 1. Write like a consume or expect or whatever combination of those
//    helpers that you need for tokens.
// DONE
// 2. Write like a string hashmap that corresponds to a new struct
//    `ISelValue` which may be a reference to an MIROperand or
//    MIRInstruction within an isel pattern, or an immediate/text
//    value; this is for match identifiers given to instructions and
//    operands.
// DONE
// 3. Also use the string hashmap code to implement the top-level
//    environment, which is persistent and immutable all the way
//    through parsing. It contains general MIR opcodes, COMPARE_EQ and
//    friends, and any extras added by the ISA after it is detected in
//    the header.
// Halfway: we still need to add COMAPARE_* and somehow implement backend-specific environment stuff.
// 4. Suck today's dick!
// DONE
// 5. Fill in insane amount of todos in parsing (so like ya know, the
//    parsing bit).
// 6. hope it all comes together and you can actually parse patterns
// DONE
// 7. use patterns to do selection, better matching with aho-corasick, etc
// ZZZ TODO AAA

static MIROpcodeCommon isel_parse_opcode(ISelParser *p) {
  if (p->tok.kind != TOKEN_IDENTIFIER)
    ERR("Expected identifier in order to parse a generalMIR opcode");

  string_buf_zterm(&p->tok.text); // just to make sure
  ISelEnvironmentEntry *entry = isel_env_entry(&p->global, p->tok.text.data);

  // FIXME: Should be an ERR, but for dev I want to keep going.
  if (!entry->key.data) WARN("Unknown opcode \"%s\"\n", p->tok.text.data);

  // Yeet general opcode identifier
  isel_next_tok(p);

  return (MIROpcodeCommon)entry->value.integer;
}

/// <inst-spec> ::= <opcode> IDENTIFIER "(" { <operand> } ")" [ "," ]
static MIRInstruction *isel_parse_inst_spec(ISelParser *p) {
  // TODO: The current token should be an identifier that maps to a general MIR opcode.
  MIRInstruction *out = mir_makenew(MIR_IMMEDIATE);

  // Parse opcode
  out->opcode = (uint32_t)isel_parse_opcode(p);
  // TODO: Lookup name in global environment to find MIR opcode.

  // Parse identifier, if present (name bound to this instruction while parsing this match)
  if (p->tok.kind == TOKEN_IDENTIFIER) {
    // TODO: Bind name in match pattern scope to `out` instruction
    isel_next_tok(p);
  }

  // Consume opening paren
  isel_consume(p, TOKEN_LPAREN);

  // Add operands to instruction as we parse them.
  while (p->tok.kind != TOKEN_RPAREN) {
    if (p->tok.kind == TOKEN_EOF)
      ERR("ISel reached EOF while parsing operands of instruction");
    mir_add_op(out, isel_parse_operand(p));
  }
  // Yeet closing paren
  isel_next_tok(p);

  // Eat comma, if present
  if (p->tok.kind == TOKEN_COMMA) isel_next_tok(p);

  return out;
}

/// <match-pattern> ::= "match" <inst-spec> { <inst-spec> } "emit" "{" <inst-spec> { <inst-spec> } "}"
static ISelPattern isel_parse_match(ISelParser *p) {
  ISelPattern out = {0};

  if (p->tok.kind != TOKEN_KW_MATCH) {
    ICE("Expected match keyword at beginning of match");
  }
  // Yeet "match" keyword.
  isel_next_tok(p);

  // Parse instructions to match in the pattern until the emit keyword is reached.
  while (p->tok.kind != TOKEN_KW_EMIT) {
    if (p->tok.kind == TOKEN_EOF) ICE("ISel reached EOF while parsing input pattern instructions of match definition");
    MIRInstruction *inst = isel_parse_inst_spec(p);
    vector_push(out.input, inst);
  }

  // Yeet "emit" keyword.
  isel_next_tok(p);

  // Parse either a single instruction to match in the pattern or an
  // opening brace and then until the closing one.
  if (p->tok.kind == TOKEN_LBRACE) {
    // Yeet opening brace
    isel_next_tok(p);

    // Ensure there is at least one instruction emitted.
    ASSERT(p->tok.kind != TOKEN_RBRACE, "There must be at least one instruction emitted by a pattern (use discard keyword if on purpose)");

    while (p->tok.kind != TOKEN_RBRACE) {
      if (p->tok.kind == TOKEN_EOF) ICE("ISel reached EOF while parsing output instructions of match definition");
      MIRInstruction *inst = isel_parse_inst_spec(p);
      vector_push(out.output, inst);
    }

    // Yeet closing brace
    isel_next_tok(p);

  } else {
    MIRInstruction *inst = isel_parse_inst_spec(p);
    vector_push(out.output, inst);
  }

  return out;
}

ISelPatterns isel_parse(ISelParser *p) {
  ASSERT(p, "Invalid argument");
  ASSERT(p->global.capacity, "Zero sized environment; forgot to initialise?");
  ASSERT(p->source.data, "NULL source input");
  ASSERT(p->source.size, "Empty source input");

  p->beg = p->source.data;
  p->end = p->source.data + p->source.size;

  // Advance past metadata (find first empty line).
  // TODO: Parse metadata to get ISA, use ISA to setup initial
  // environment (MIR opcode identifiers specific to the ISA)
  {
    bool lfcr = false; // to catch (CR)LFCR(LF) sequences
    bool lflf = false; // to catch LFLF sequences
    char lastc = '\0';
    while (*p->beg) {
      if (lfcr && *p->beg == '\n') {
        printf("Skipped metadata:\n```\n%.*s```\n", (int)(p->beg - p->source.data), p->source.data);
        print("How's Windows treatin ya?\n");
        break;
      }
      lfcr = *p->beg == '\r' && lastc == '\n';
      lflf = *p->beg == lastc && lastc == '\n';
      if (lflf) {
        print("Unix user, I see...\n");
        break;
      }
      lastc = *p->beg++;
    }
  }

  /// Lex the first character and token.
  isel_next_c(p);
  isel_next_tok(p);

  // TODO: Setup common environment (general MIR opcode identifiers, COMPARE_EQ and friends, etc)

  /// Parse the match definitions within the file.
  ISelPatterns patterns = {0};
  while (p->tok.kind != TOKEN_EOF) {
    if (p->tok.kind == TOKEN_EOF) break;
    ISelPattern parsed_pattern = isel_parse_match(p);
    vector_push(patterns, parsed_pattern);
  }

  return patterns;
}

ISelPatterns isel_parse_file(const char *filepath) {
  bool success = false;
  ISelParser p = {0};
  p.global = isel_env_create(1024);
  isel_env_init_common_opcodes(&p.global);

  string contents = platform_read_file(filepath, &success);
  if (!success) ICE("Failed to read file at \"%s\" for instruction selection", filepath);
  p.source_filepath = filepath;
  p.source = as_span(contents);

  ISelPatterns out = isel_parse(&p);

  vector_delete(p.tok.text);
  isel_env_delete(&p.global);
  free(contents.data);
  return out;
}


bool isel_does_pattern_match(ISelPattern pattern, MIRInstructionVector instructions, ISelCompareValueOption compare_value) {
  /// A pattern that is larger than the instructions given means it will never match.
  if (pattern.input.size > instructions.size) return false;

  foreach_index (i, pattern.input) {
    MIRInstruction *inst_pattern = pattern.input.data[i];
    MIRInstruction *inst = instructions.data[i];

    /// Two instructions' opcodes must be equal.
    if (inst->opcode != inst_pattern->opcode) return false;

    /// Two instructions' operand counts must be equal.
    if (inst->operand_count != inst_pattern->operand_count) return false;

    /// Two instructions' operands must be equal (to a varying extent).
    MIROperand *base_pattern = NULL;
    MIROperand *base = NULL;
    if (inst_pattern->operand_count <= MIR_OPERAND_SSO_THRESHOLD) {
      base_pattern = inst_pattern->operands.arr;
      base = inst->operands.arr;
    } else {
      base_pattern = inst_pattern->operands.vec.data;
      base = inst->operands.vec.data;
    }
    for (size_t j = 0; j < inst->operand_count; ++j) {
      MIROperand *op_pattern = base_pattern + j;
      MIROperand *op = base + j;
      // Two operands' kinds must be equal.
      if (op->kind != op_pattern->kind) return false;
      // Two operands' values may be equal or not, depending on parameter.
      if (compare_value == ISEL_DO_COMPARE_VALUE &&
          memcmp(&op->value, &op_pattern->value, sizeof(op->value)) != 0)
        return false;
    }

  }

  return true;
}

void isel_do_selection(MIRFunctionVector mir, ISelPatterns patterns) {
  if (!mir.size) return;
  if (!patterns.size) return;

  ISelEnvironment env = isel_env_create(1024);
  isel_env_init_common_opcodes(&env);

  // TODO: Everything past this point is temporary and will change :P
  // We will eventually use a modified Aho-Corasick pattern matching
  // algorithm, but for now use a quite naive implementation just to get
  // things started without needing to build an entire Aho-Corasick trie,
  // calculate fail nodes, etc.

  // FOR NOW: Sort patterns largest-first, and simply iterate all the
  // patterns every time we have instructions to match. This is FOR NOW
  // because I'm sure the time complexity is n-squared at this point :^&

  // Sort patterns such that the ones with the most elements in the
  // input vector come first.
  foreach_index (i, patterns) {
    // Can't swap last with last, so don't try.
    if (i == patterns.size - 1) break;
    // Find longest in rest of vector
    ISelPattern *pattern_to_swap = patterns.data + i;
    ISelPattern *largest = pattern_to_swap;
    for (size_t j = i; j < patterns.size; j++) {
      ISelPattern *largest_candidate = patterns.data + i;
      if (largest_candidate->input.size > largest->input.size)
        largest = largest_candidate;
    }
    if (pattern_to_swap != largest) {
      ISelPattern tmp = *pattern_to_swap;
      *pattern_to_swap = *largest;
      *largest = tmp;
    }
  }

  usz longest_pattern_length = patterns.data->input.size;

  MIRInstructionVector instructions = {0};

  foreach_ptr (MIRFunction*, f, mir) {
    if (f->origin->is_extern) continue;

    foreach_ptr (MIRBlock*, bb, f->blocks) {
      vector_clear(instructions);

      // Amount of instructions that have been pushed to the
      // instructions vector for instruction selection. Used to continue
      // pushing instructions where we left off.
      usz instructions_handled = 0;

      /// This label acts as a point of continuation when we need to
      /// (potentially) (re)fill the instructions vector and then pattern
      /// match on it.
    add_instructions:
      for (; instructions_handled < bb->instructions.size; ++instructions_handled) {
        MIRInstruction *inst = bb->instructions.data[instructions_handled];

        // Add (up to) `longest_pattern_length` instructions to the instructions vector.
        if (instructions.size >= longest_pattern_length) break;

        vector_push(instructions, inst);
      }

      bool matched = false;
      foreach (ISelPattern, pattern, patterns) {
        if (isel_does_pattern_match(*pattern, instructions, ISEL_DONT_COMPARE_VALUE)) {
          matched = true;

          // Remove first N instructions where N is the amount of
          // instructions that matched in the pattern.
          for (usz j = 0; j < pattern->input.size; ++j)
            vector_remove_index(instructions, 0);

          // Prepend output instructions from pattern to instructions.
          foreach_ptr (MIRInstruction*, pattern_inst, pattern->output) {
            vector_insert(instructions, instructions.data, pattern_inst);
          }

          // Again attempt to pattern match and do all this over again, until nothing happens.
          break;
        }
      }
      // If we get through *all* of the patterns, and none of them
      // matched, we can pop an instruction off the front and emit it
      // into the output, before going back to the "add instructions"
      // bit.
      if (!matched) {
        // If there are more instructions in this block to match
        // against, go back and them, pattern match again, etc.
        // Pop instruction from front of `instructions` and add it to emission output.
        // TODO: Add removed instruction to emission output, or whatever. I
        // think the actual implementation will be like this: we will build a
        // new vector of emitted instructions and then swap the vector in the
        // basic block once we are done iterating over it.
        vector_remove_index(instructions, 0);
      }

      if (instructions_handled < bb->instructions.size)
        goto add_instructions;

    } // foreach_ptr (MIRBlock*, bb, ...)
  } // foreach_ptr (MIRFunction*, f, ...)

  vector_delete(instructions);
  isel_env_delete(&env);
}
