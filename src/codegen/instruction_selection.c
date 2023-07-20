#include <codegen.h>
#include <codegen/codegen_forward.h>
#include <codegen/instruction_selection.h>
#include <codegen/machine_ir.h>
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <inttypes.h>
#include <ir/ir.h>
#include <platform.h>
#include <stdbool.h>
#include <stdlib.h>
#include <utils.h>
#include <vector.h>

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

  TOKEN_INTEGER,
  TOKEN_IDENTIFIER,

  // Keywords
  TOKEN_KW_MATCH,
  TOKEN_KW_EMIT,
  TOKEN_KW_DISCARD,
  TOKEN_KW_CLOBBERS,
  TOKEN_KW_IS,

  TOKEN_SEMICOLON,

  // ADD NEW ONES HERE PLEASE

  // ASCII simply-lexed Operators/Punctuation
  TOKEN_LPAREN = '(',
  TOKEN_RPAREN = ')',
  TOKEN_COMMA = ',',
  TOKEN_LT = '<',
  TOKEN_EQ = '=',
  TOKEN_GT = '>',
  TOKEN_LBRACKET = '[',
  TOKEN_RBRACKET = ']',
  TOKEN_LBRACE = '{',
  TOKEN_RBRACE = '}',
} ISelTokenKind;

static const char *isel_token_kind_to_string(ISelTokenKind kind) {
  switch (kind) {
  case TOKEN_EOF: return "EOF";
  case TOKEN_OPKIND_REGISTER: return "Register";
  case TOKEN_OPKIND_IMMEDIATE: return "Immediate";
  case TOKEN_OPKIND_BLOCK: return "Block";
  case TOKEN_OPKIND_FUNCTION: return "Function";
  case TOKEN_OPKIND_NAME: return "Name";
  case TOKEN_OPKIND_STATIC: return "Static";
  case TOKEN_OPKIND_LOCAL: return "Local";
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
  case TOKEN_KW_DISCARD: return "discard";
  case TOKEN_KW_CLOBBERS: return "clobbers";
  case TOKEN_KW_IS: return "is";
  case TOKEN_IDENTIFIER: return "identifier";
  case TOKEN_INTEGER: return "integer";
  }
  UNREACHABLE();
}

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

  // Pointer to `local` member within currently-being-parsed ISelPattern, or NULL.
  ISelEnvironment *local;

  // Index of instruction being parsed within match pattern.
  unsigned int pattern_instruction_index;
  // Index of operand being parsed within instruction.
  unsigned int operand_index;

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


static void isel_env_init_common(ISelEnvironment *env) {
#define ADD_OPCODE(opcode, ...) isel_env_add_opcode(env, STR(CAT(MIR_, opcode)), CAT(MIR_, opcode));
  ALL_SHARED_IR_AND_MIR_INSTRUCTION_TYPES(ADD_OPCODE);
#undef ADD_OPCODE

  STATIC_ASSERT(MPSEUDO_COUNT == 2, "Exhaustive handling of MIR pseudo-instructions in instruction selection global environment initialisation");
  isel_env_add_opcode(env, "MPSEUDO_R2R", MPSEUDO_R2R);

  isel_env_add_integer(env, "COMPARE_EQ", (isz)COMPARE_EQ);
  isel_env_add_integer(env, "COMPARE_NE", (isz)COMPARE_NE);
  isel_env_add_integer(env, "COMPARE_LT", (isz)COMPARE_LT);
  isel_env_add_integer(env, "COMPARE_GT", (isz)COMPARE_GT);
  isel_env_add_integer(env, "COMPARE_LE", (isz)COMPARE_LE);
  isel_env_add_integer(env, "COMPARE_GE", (isz)COMPARE_GE);
}

ISelEnvironment isel_env_create_empty(usz initial_capacity) {
  ISelEnvironment out = {0};
  out.capacity = initial_capacity;
  out.data = calloc(out.capacity, sizeof(out.data[0]));
  return out;
}

ISelEnvironment isel_env_create(usz initial_capacity) {
  ISelEnvironment out = isel_env_create_empty(initial_capacity);
  isel_env_init_common(&out);
  return out;
}

void isel_env_delete(ISelEnvironment *env) {
  ASSERT(env, "Invalid argument");
  for (usz i = 0; i < env->capacity; ++i) {
    ISelEnvironmentEntry *entry = env->data + i;
    if (entry->key.data) free(entry->key.data);
    if (entry->value.text.data) free(entry->value.text.data);
  }
  if (env->data) free(env->data);
}

ISelEnvironmentEntry *isel_env_entry(ISelEnvironment *env, const char *key) {
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

void isel_env_print_entry(ISelEnvironmentEntry *entry) {
  ASSERT(entry, "Invalid argument");
  STATIC_ASSERT(ISEL_ENV_COUNT == 7, "Exhaustive handling of ISel environment value types during printing");
  print("%S ", entry->key);
  switch (entry->value.kind) {
  case ISEL_ENV_NONE: print("NONE"); break;
  case ISEL_ENV_OPCODE: print("OPCODE %z", entry->value.integer); break;
  case ISEL_ENV_OP_KIND: print("OPKIND %z", entry->value.integer); break;
  case ISEL_ENV_INTEGER: print("INTEGER %z", entry->value.integer); break;
  case ISEL_ENV_OP_REF: print("OP_REF inst:%u op:%u", entry->value.op.pattern_instruction_index, entry->value.op.operand_index); break;
  case ISEL_ENV_INST_REF: print("INST_REF %u", entry->value.inst); break;
  case ISEL_ENV_REGISTER: print("REG %Z.%Z", entry->value.vreg.value, entry->value.vreg.size); break;
  case ISEL_ENV_COUNT:
  default: UNREACHABLE();
  }
  print("\n");
}
void isel_env_print(ISelEnvironment *env) {
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

void isel_env_add_opcode(ISelEnvironment *env, const char *key, usz opcode) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_OPCODE;
  newval.integer = (isz)opcode;
  isel_env_insert(env, key, newval);

  //ISelEnvironmentEntry *entry = isel_env_entry(env, key);
  //isel_env_print_entry(entry);
}

void isel_env_add_integer(ISelEnvironment *env, const char *key, isz value) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_INTEGER;
  newval.integer = value;
  isel_env_insert(env, key, newval);
}

void isel_env_add_register(ISelEnvironment *env, const char *key, VReg vreg) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_REGISTER;
  newval.vreg = vreg;
  isel_env_insert(env, key, newval);
}

static void isel_env_add_operand_reference(ISelEnvironment *env, const char *key, unsigned int pattern_instruction_index, unsigned int operand_index) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_OP_REF;
  newval.op.pattern_instruction_index = pattern_instruction_index;
  newval.op.operand_index = operand_index;
  isel_env_insert(env, key, newval);
}

static void isel_env_add_instruction_reference(ISelEnvironment *env, const char *key, unsigned int pattern_instruction_index) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_INST_REF;
  newval.inst = pattern_instruction_index;
  isel_env_insert(env, key, newval);
}

static void isel_env_add_op_kind(ISelEnvironment *env, const char *key, usz opkind) {
  ISelValue newval = {0};
  newval.kind = ISEL_ENV_OP_KIND;
  newval.integer = (isz)opkind;
  isel_env_insert(env, key, newval);
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
} keywords[14] = {
  {literal_span_raw("match"), TOKEN_KW_MATCH},
  {literal_span_raw("emit"), TOKEN_KW_EMIT},
  {literal_span_raw("discard"), TOKEN_KW_DISCARD},
  {literal_span_raw("clobbers"), TOKEN_KW_CLOBBERS},
  {literal_span_raw("is"), TOKEN_KW_IS},
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
      p->tok.kind = (ISelTokenKind) p->lastc;
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

// number or bound identifier
static ISelValue isel_parse_expression(ISelParser *p) {
  if (p->tok.kind == TOKEN_IDENTIFIER) {
    // Lookup identifier in global environment
    string_buf_zterm(&p->tok.text);
    ISelEnvironmentEntry *entry = isel_env_entry(&p->global, p->tok.text.data);
    if (!entry->key.data) ERR("Expected expression, got unknown identifier \"%S\"", as_span(p->tok.text));

    // Yeet identifier
    isel_next_tok(p);

    return entry->value;
  }

  if (p->tok.kind == TOKEN_INTEGER) {
    ISelValue out = {0};
    out.kind = ISEL_ENV_INTEGER;
    out.integer = (isz)p->tok.integer;

    // Yeet integer

    isel_next_tok(p);
    return out;
  }

  UNREACHABLE();
}

/// <operand> ::= [ OPERAND_KIND ] IDENTIFIER [ "=" <expression> ] [ "," ]
static MIROperand isel_parse_operand(ISelParser *p) {
  MIROperand out = {0};

  // Either parse (OPKIND IDENTIFER) and create binding or (IDENTIFIER) with binding lookup
  if (isel_token_kind_is_opkind(p->tok.kind)) {
    // Parse operand kind (should be one of TOKEN_OPKIND_*)
    out.kind = isel_parse_operand_kind(p);
    // Optionally parse identifier (name bound to this operand while parsing this match)
    if (p->tok.kind == TOKEN_IDENTIFIER) {
      // Bind name in local match environment
      string_buf_zterm(&p->tok.text);
      isel_env_add_operand_reference(p->local, p->tok.text.data, p->pattern_instruction_index, p->operand_index);

      // DEBUG
      //print("Bound \"%s\" to new operand\n", p->tok.text.data);
      //ISelEnvironmentEntry *entry = isel_env_entry(p->local, p->tok.text.data);
      //isel_env_print_entry(entry);

      // Yeet identifier
      isel_next_tok(p);
    }
  } else {
    // Ensure it is an identifier.
    if (p->tok.kind != TOKEN_IDENTIFIER)
      ERR("Expected operand kind or operand identifier, but got %s instead", isel_token_kind_to_string(p->tok.kind));

    // Lookup identifier in local environment and ensure it is bound to an operand reference
    string_buf_zterm(&p->tok.text);
    ISelEnvironmentEntry *entry = isel_env_entry(p->local, p->tok.text.data);
    if (!entry->key.data) ERR("Referenced operand does not exist; maybe a typo?");

    if (entry->value.kind == ISEL_ENV_OP_REF) {
      out.kind = MIR_OP_OP_REF;
      out.value.op_ref.pattern_instruction_index = entry->value.op.pattern_instruction_index;
      out.value.op_ref.operand_index = entry->value.op.operand_index;
    } else if (entry->value.kind == ISEL_ENV_INST_REF) {
      out.kind = MIR_OP_INST_REF;
      out.value.inst_ref = entry->value.inst;
    } else ERR("Identifier is bound in environment but has unexpected value kind");

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
    loc expr_loc = p->tok.source_location;
    ISelValue value = isel_parse_expression(p);
    if (value.kind == ISEL_ENV_INTEGER) {
      if (out.kind != MIR_OP_IMMEDIATE)
        ERR_AT(expr_loc, "Cannot initialise this type of operand with integer expression");
      out.value.imm = value.integer;
    } else if (value.kind == ISEL_ENV_REGISTER) {
      if (out.kind != MIR_OP_REGISTER)
        ERR_AT(expr_loc, "Cannot initialise this type of operand with register expression");
      out.value.reg.value = (uint32_t)value.vreg.value;
      out.value.reg.size = (uint16_t)value.vreg.size;
    } else ERR_AT(expr_loc, "Unhandled expression kind");
  }

  if (p->tok.kind == TOKEN_KW_IS) {
    // Yeet 'is'
    isel_next_tok(p);

    loc op_loc = p->tok.source_location;
    MIROperand op = isel_parse_operand(p);
    switch (op.kind) {
    case MIR_OP_OP_REF: {
      out.value_constraint_kind = MIR_OP_OP_REF;
      out.value_constraint.op_ref = op.value.op_ref;
    } break;
    case MIR_OP_INST_REF: {
      out.value_constraint_kind = MIR_OP_INST_REF;
      out.value_constraint.inst_ref = op.value.inst_ref;
    } break;
    default: ERR_AT(op_loc, "Unhandled operand kind %d following 'is' keyword...", (int)op.kind);
    }
  }

  // Eat comma, if present
  if (p->tok.kind == TOKEN_COMMA) isel_next_tok(p);

  return out;
}

static MIROpcodeCommon isel_parse_opcode(ISelParser *p) {
  if (p->tok.kind != TOKEN_IDENTIFIER)
    ERR("Expected identifier in order to parse an opcode");

  string_buf_zterm(&p->tok.text); // just to make sure
  ISelEnvironmentEntry *entry = isel_env_entry(&p->global, p->tok.text.data);

  if (!entry->key.data) ERR("Unknown opcode \"%s\"\n", p->tok.text.data);

  // Yeet general opcode identifier
  isel_next_tok(p);

  return (MIROpcodeCommon)entry->value.integer;
}

/// <inst-spec> ::= <opcode> IDENTIFIER "(" { <operand> } ")" [ "," ]
static MIRInstruction *isel_parse_inst_spec(ISelParser *p) {
  MIRInstruction *out = mir_makenew(MIR_IMMEDIATE);

  // Parse opcode
  out->opcode = (uint32_t)isel_parse_opcode(p);

  // Parse identifier, if present (name bound to this instruction while parsing this match)
  if (p->tok.kind == TOKEN_IDENTIFIER) {
    // Bind name in match pattern scope to `out` instruction
    string_buf_zterm(&p->tok.text);
    isel_env_add_instruction_reference(p->local, p->tok.text.data, p->pattern_instruction_index);

    // DEBUG
    //print("Bound \"%s\" to new instruction\n", p->tok.text.data);
    //ISelEnvironmentEntry *entry = isel_env_entry(p->local, p->tok.text.data);
    //isel_env_print_entry(entry);

    isel_next_tok(p);
  }

  // Consume opening paren
  isel_consume(p, TOKEN_LPAREN);

  // Add operands to instruction as we parse them.
  p->operand_index = 0;
  while (p->tok.kind != TOKEN_RPAREN) {
    if (p->tok.kind == TOKEN_EOF)
      ERR("ISel reached EOF while parsing operands of instruction");
    MIROperand op = isel_parse_operand(p);
    mir_add_op(out, op);
    ++p->operand_index;
  }

  // Yeet closing paren
  isel_next_tok(p);

  // Parse clobbers
  if (p->tok.kind == TOKEN_KW_CLOBBERS) {
    // Yeet "clobbers" keyword
    isel_next_tok(p);

    while (p->tok.kind != TOKEN_SEMICOLON) {
      if (p->tok.kind == TOKEN_EOF)
        ERR("ISel reached EOF while parsing list of clobbers; did you forget a semi-colon?");


      if (p->tok.kind != TOKEN_IDENTIFIER)
        ERR("ISel expected identifier (bound to something in the environment), but got (a) %s instead", isel_token_kind_to_string(p->tok.kind));

      string_buf_zterm(&p->tok.text);
      ISelEnvironmentEntry *entry = isel_env_entry(p->local, p->tok.text.data);
      if (!entry->key.data) entry = isel_env_entry(&p->global, p->tok.text.data);
      if (!entry->key.data) ERR("ISel expected identifier bound to something in the environment, but %S is not bound in any environment", as_span(p->tok.text));

      if (entry->value.kind != ISEL_ENV_REGISTER)
        ERR("ISel expected identifier bound to a register, but %S is bound to some other kind.", as_span(p->tok.text));

      MIROperandRegister r = {0};
      r.value = entry->value.vreg.value;
      r.size = (u32) entry->value.vreg.size;
      vector_push(out->clobbers, r);

      // Yeet bound identifier
      isel_next_tok(p);

      // Yeet comma, if present
      if (p->tok.kind == TOKEN_COMMA) isel_next_tok(p);
    }

    // Yeet ';'
    isel_next_tok(p);
  }

  // Eat comma, if present
  if (p->tok.kind == TOKEN_COMMA) isel_next_tok(p);

  ++p->pattern_instruction_index;

  return out;
}

///<match-pattern> ::= "match" <inst-spec> { <inst-spec> } ( "emit" | "discard" ) ( <inst-block> | <inst-spec> )
static ISelPattern isel_parse_match(ISelParser *p) {
  ISelPattern out = {0};
  out.local = isel_env_create_empty(16);
  p->local = &out.local;

  if (p->tok.kind != TOKEN_KW_MATCH)
    ICE("Expected match keyword at beginning of match");

  // Yeet "match" keyword.
  isel_next_tok(p);

  // Parse instructions to match in the pattern until the emit keyword is reached.
  p->pattern_instruction_index = 0;
  while (p->tok.kind != TOKEN_KW_EMIT && p->tok.kind != TOKEN_KW_DISCARD) {
    if (p->tok.kind == TOKEN_EOF) ICE("ISel reached EOF while parsing input pattern instructions of match definition");
    MIRInstruction *inst = isel_parse_inst_spec(p);
    vector_push(out.input, inst);
  }

  if (p->tok.kind == TOKEN_KW_EMIT) {
    // Yeet "emit" keyword.
    isel_next_tok(p);

    // Parse either:
    // - a single instruction to match in the pattern, or
    // - an opening brace and then until the closing one.
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
  } else if (p->tok.kind == TOKEN_KW_DISCARD) {
    // Yeet "discard" keyword.
    isel_next_tok(p);
  } else ERR("Unrecognised token following match pattern instructions");

  p->local = NULL;

  return out;
}

/// Add the opcodes and values needed for the arch's ISel.
// TODO: This *can't* be the best way of doing things...
void isel_x86_64_env(ISelEnvironment *env);
static void isel_env_arch_specific(ISelEnvironment *env, CodegenArchitecture arch) {
  STATIC_ASSERT(ARCH_COUNT == 2, "Exhaustive handling of architectures in ISel environment initialisation");
  switch (arch) {
  case ARCH_X86_64: isel_x86_64_env(env); break;
  case ARCH_NONE:
  case ARCH_COUNT:
    UNREACHABLE();
  }
}

static CodegenArchitecture isel_arch_from_string(span arch_string) {
  if (string_eq(arch_string, literal_span("x86_64"))) return ARCH_X86_64;
  ICE("Invalid architecture string \"%S\"\n", arch_string);
}

ISelPatterns isel_parse(ISelParser *p) {
  ASSERT(p, "Invalid argument");
  ASSERT(p->global.capacity, "Zero sized environment; forgot to initialise?");
  ASSERT(p->source.data, "NULL source input");
  ASSERT(p->source.size, "Empty source input");

  p->beg = p->source.data;
  p->end = p->source.data + p->source.size;

  // Advance past metadata (find first empty line).
  // Parse metadata to get ISA, use ISA to setup initial
  // environment (MIR opcode identifiers specific to the ISA)
  {
    bool lfcr = false; // to catch (CR)LFCR(LF) sequences
    bool lflf = false; // to catch LFLF sequences
    char lastc = '\0';
    const char *line_begin = p->beg;
    while (*p->beg) {
      if (lfcr && *p->beg == '\n') {
        //print("How's Windows treatin ya?\n");
        break;
      }
      lfcr = *p->beg == '\r' && lastc == '\n';
      lflf = *p->beg == lastc && lastc == '\n';
      if (lflf) {
        //print("Unix user, I see...\n");
        break;
      }

      // At end of every non-empty line, parse `<left>: <right>` identifier
      if (*p->beg == '\n') {
        usz line_length = (usz)(p->beg - p->source.data);
        if (lastc == '\r' || lastc == '\n') --line_length;
        string_buffer left = {0};
        string_buffer right = {0};
        usz i = 0;
        for (; i < line_length; ++i) {
          char c = line_begin[i];
          if (c == ':') {
            ++i;
            break;
          }
          vector_push(left, c);
        }
        while (i < line_length && isspace(line_begin[i])) ++i;
        for (; i < line_length; ++i) {
          char c = line_begin[i];
          vector_push(right, c);
        }
        ASSERT(left.size, "Metadata line has invalid left hand side!");
        ASSERT(right.size, "Metadata line has invalid right hand side!");
        //print("left:\"%S\" right:\"%S\"\n", as_span(left), as_span(right));

        if (string_eq(left, literal_span("ISA"))) {
          CodegenArchitecture arch = isel_arch_from_string(as_span(right));
          isel_env_arch_specific(&p->global, arch);
        }

        line_begin = p->beg + 1;
      }

      lastc = *p->beg++;
    }
  }

  /// Lex the first character and token.
  isel_next_c(p);
  isel_next_tok(p);

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

bool isel_does_pattern_match(ISelPattern pattern, MIRInstructionVector instructions) {
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
      // Value constraint
      switch (op_pattern->value_constraint_kind) {
      default: ICE("Unhandled value constraint kind %d", (int)op_pattern->value_constraint_kind);
      case MIR_OP_NONE: break;
      case MIR_OP_OP_REF: {
        // RESOLVE INSTRUCTION REFERENCE
        ASSERT(op_pattern->value_constraint.op_ref.pattern_instruction_index < instructions.size,
               "OP_REF type value constraint must only reference a pattern instruction, not an emitted instruction.");
        MIRInstruction *resolved_inst = instructions.data[op_pattern->value_constraint.op_ref.pattern_instruction_index];
        // RESOLVE OPERAND REFERENCE
        ASSERT(op_pattern->value_constraint.op_ref.operand_index < resolved_inst->operand_count,
               "OP_REF type value constraint references an operand that does not exist within pattern instruction.");
        MIROperand *resolved_op = mir_get_op(resolved_inst, op_pattern->value_constraint.op_ref.operand_index);

        // Ensure `op` is a register and has a value that matches the
        // instruction's register.
        if (op->kind != resolved_op->kind) return false;
        if (memcmp(&op->value, &resolved_op->value, sizeof(op->value)) != 0) return false;

      } break;
      case MIR_OP_INST_REF: {
        // RESOLVE INSTRUCTION REFERENCE
        ASSERT(op_pattern->value_constraint.inst_ref < instructions.size,
               "INST_REF type value constraint must only reference a pattern instruction, not an emitted instruction.");
        MIRInstruction *resolved_inst = instructions.data[op_pattern->value_constraint.inst_ref];
        MIROperand resolved_inst_reg = mir_op_reference(resolved_inst);

        // Ensure `op` is a register and has a value that matches the
        // instruction's register.
        if (op->kind != resolved_inst_reg.kind) return false;
        if (memcmp(&op->value, &resolved_inst_reg.value, sizeof(op->value)) != 0) return false;

      } break;
      } // switch (value_constraint_kind)
    }

  }

  return true;
}


typedef Vector(usz) ISelRegisterValues;

static void mark_defining_uses(ISelRegisterValues *regs_seen, MIRBlock *block) {
  foreach_val (inst, block->instructions) {
    FOREACH_MIR_OPERAND(inst, op) {
      if (op->kind == MIR_OP_REGISTER && op->value.reg.value >= MIR_ARCH_START) {
        if (!vector_contains(*regs_seen, op->value.reg.value)) {
          op->value.reg.defining_use = true;
          vector_push(*regs_seen, op->value.reg.value);
        }
      }
    }
  }
}

/// NOTE: Pass empty vectors for visited and doubly_visited when
/// calling on entry of function.
static void calculate_defining_uses_for_block(ISelRegisterValues *regs_seen, MIRBlock *block, MIRBlockVector *visited, MIRBlockVector *doubly_visited) {
  /// Don't visit the same block thrice.
  if (vector_contains(*visited, block)) {
    if (vector_contains(*doubly_visited, block))
      return;
    else vector_push(*doubly_visited, block);
  } else vector_push(*visited, block);

  // At each block of the function, starting at the entry, walk the
  // control flow. The first operand usage of a virtual register will
  // be set to a defining use (i.e. the first place that that register
  // must be classified as "in use" by the register allocator).

  mark_defining_uses(regs_seen, block);

  // To follow control flow of an exit block, we stop.
  if (block->is_exit) return;

  // If a block has only a single successor, we don't need to do
  // anything fancy; just follow the simple control flow for as long as
  // possible.
  while (block->successors.size == 1) {
    block = vector_front(block->successors);
    mark_defining_uses(regs_seen, block);
  }
  // If a block has multiple successors, we will "remember" the
  // registers that we had seen at the beginning, and reset
  // to that after following each one.
  // To follow control flow, we sometimes have to come back to a
  // block after reaching the exit, as some blocks have multiple
  // successors. We use recursion for this.
  foreach_val (successor, block->successors) {
    // Copy registers seen.
    ISelRegisterValues regs_seen_copy = {0};
    vector_append(regs_seen_copy, *regs_seen);

    calculate_defining_uses_for_block(&regs_seen_copy, successor, visited, doubly_visited);

    vector_delete(regs_seen_copy);
  }
}

void isel_do_selection(MIRFunctionVector mir, ISelPatterns patterns) {
  if (!mir.size) return;
  if (!patterns.size) return;

  ISelEnvironment env = isel_env_create(1024);

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
    for (size_t j = i + 1; j < patterns.size; ++j) {
      ISelPattern *largest_candidate = patterns.data + j;
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

  // Instructions currently being expanded/dealt with.
  MIRInstructionVector instructions = {0};

  foreach_val (f, mir) {
    if (!ir_func_is_definition(f->origin)) continue;

    foreach_val (bb, f->blocks) {
      vector_clear(instructions);
      // Instructions that will be output.
      MIRInstructionVector new_instructions = {0};

      // Amount of instructions that have been pushed to the
      // instructions vector for instruction selection. Used to continue
      // pushing instructions where we left off.
      usz instructions_handled = 0;

      /// This label acts as a point of continuation when we need to
      /// (potentially) (re)fill the instructions vector and then pattern
      /// match on it.
    add_instructions:
      for (; instructions_handled < bb->instructions.size; ++instructions_handled) {
        // Add (up to) `longest_pattern_length` instructions to the instructions vector.
        if (instructions.size >= longest_pattern_length) break;

        MIRInstruction *inst = bb->instructions.data[instructions_handled];

        vector_push(instructions, inst);
      }

      bool matched = false;
      foreach (pattern, patterns) {
        if (isel_does_pattern_match(*pattern, instructions)) {
          matched = true;

          // Remove first N instructions where N is the amount of
          // instructions that matched in the pattern.
          // Put these in a "pattern input" tmp buffer as we remove them.
          MIRInstructionVector pattern_input = {0};
          for (usz j = 0; j < pattern->input.size; ++j) {
            vector_push(pattern_input, vector_front(instructions));
            vector_remove_index(instructions, 0);
          }

          // Prepend output instructions from pattern to instructions.
          // Iterate over the pattern output instructions, making a
          // copy of each and populating operands as necessary,
          // corresponding to pattern references.
          MIRInstruction *last_input_inst = vector_back(pattern_input);
          foreach_index (i, pattern->output) {
            MIRInstruction *pattern_inst = pattern->output.data[i];
            MIRInstruction *out = mir_makecopy(pattern_inst);
            if (i == pattern->output.size - 1)
              out->reg = last_input_inst->reg;
            else out->reg = MIR_ARCH_START + f->inst_count++;
            out->origin = last_input_inst->origin;
            out->block = bb;

            FOREACH_MIR_OPERAND(out, op) {
              // Resolve operand and instruction pattern references...
              if (op->kind == MIR_OP_OP_REF) {
                ASSERT(op->value.op_ref.pattern_instruction_index < pattern_input.size + pattern->output.size,
                       "Invalid pattern instruction index in operand reference (parser went wrong)");
                MIRInstruction *inst = NULL;
                if (op->value.op_ref.pattern_instruction_index >= pattern_input.size) {
                  usz pattern_output_inst_index = op->value.op_ref.pattern_instruction_index - pattern_input.size;
                  ASSERT(pattern_output_inst_index <= i, "Cannot forward-reference emission instructions... How'd you even manage this?");
                  // Self-reference or backward-reference
                  if (pattern_output_inst_index == i) inst = out;
                  else inst = instructions.data[pattern_output_inst_index];
                }
                else inst = pattern_input.data[op->value.op_ref.pattern_instruction_index];

                // Get operand from resolved instruction reference
                ASSERT(op->value.op_ref.operand_index < inst->operand_count,
                       "Invalid operand index (parser went wrong)");

                *op = *mir_get_op(inst, op->value.op_ref.operand_index);

              } else if (op->kind == MIR_OP_INST_REF) {
                ASSERT(op->value.inst_ref < pattern_input.size + pattern->output.size,
                       "Invalid pattern instruction index in instruction reference (parser went wrong)");
                if (op->value.op_ref.pattern_instruction_index >= pattern_input.size) {
                  usz pattern_output_inst_index = op->value.op_ref.pattern_instruction_index - pattern_input.size;
                  ASSERT(pattern_output_inst_index <= i, "Cannot forward-reference emission instructions... How'd you even manage this?");
                  // Self-reference
                  if (pattern_output_inst_index == i) {
                    *op = mir_op_reference(out);
                  } else {
                    // Create a register reference to the OUTPUT instruction corresponding to the referenced *pattern* output instruction.
                    MIRInstruction *inst = instructions.data[pattern_output_inst_index];
                    *op = mir_op_reference(inst);
                  }
                } else {
                  MIRInstruction *inst = pattern_input.data[op->value.op_ref.pattern_instruction_index];
                  *op = mir_op_reference(inst);
                }
              }
            }

            vector_insert(instructions, instructions.data + i, out);
          }

          vector_delete(pattern_input);

          // Again attempt to pattern match and do all this over again, until nothing happens.
          break;
        }
      }
      // If we get through *all* of the patterns, and none of them
      // matched, we can pop an instruction off the front and emit it
      // into the output, before going back to the "add instructions"
      // bit.
      if (!matched) {
        if (instructions.size) {
          // Add front of `instructions` to emission output.
          vector_push(new_instructions, instructions.data[0]);
          // Pop instruction.
          vector_remove_index(instructions, 0);
        }
      }

      // If there are more instructions in this block to match
      // against, go back and them, pattern match again, etc.
      if (instructions_handled < bb->instructions.size)
        goto add_instructions;

      // Otherwise, if there are remaining instructions, go again.
      if (instructions.size != 0) goto add_instructions;

      // Fully handled instructions of block; replace old instructions with newly selected ones.
      MIRInstructionVector tmp = bb->instructions;
      bb->instructions = new_instructions; // new_instructions is moved
      // Delete vector of old instructions.
      vector_delete(tmp);
    } // foreach_ptr (MIRBlock*, bb, ...)
  } // foreach_ptr (MIRFunction*, f, ...)

  // Mark defining uses of virtual register operands for RA.
  ISelRegisterValues vregs_seen = {0};
  MIRBlockVector visited = {0};
  MIRBlockVector doubly_visited = {0};
  foreach_val (f, mir) {
    if (!ir_func_is_definition(f->origin)) continue;

    MIRBlock *entry = vector_front(f->blocks);
    ASSERT(entry->is_entry, "First block within MIRFunction is not entry point; we should do more work to find the entry, sorry");

    // NOTE: This function is an absolute doozy; check it out, iff you must.
    calculate_defining_uses_for_block(&vregs_seen, entry, &visited, &doubly_visited);

  }

  vector_delete(vregs_seen);

  vector_delete(instructions);
  isel_env_delete(&env);
}

// Delete ISelPatterns.
void isel_patterns_delete(ISelPatterns *patterns) {
  foreach (pattern, *patterns) {
    isel_env_delete(&pattern->local);
  }
  vector_delete(*patterns);
}

void isel_print_mir_operand(MIROperand *operand) {
  print("%s", mir_operand_kind_string(operand->kind));
  switch (operand->kind) {
  case MIR_OP_NONE: break;
  case MIR_OP_REGISTER: print(" %Z.%u", operand->value.reg.value, operand->value.reg.size); break;
  case MIR_OP_IMMEDIATE: print(" %I", operand->value.imm); break;
  case MIR_OP_BLOCK: break;//print(" %S", operand->value.block->name); break;
  case MIR_OP_FUNCTION: print(" %S", operand->value.function->name); break;
  case MIR_OP_NAME: print("%s", operand->value.name); break;
  case MIR_OP_STATIC_REF: print(" %S", ir_static_ref_var(operand->value.static_ref)->name); break;
  case MIR_OP_LOCAL_REF: print(" %Z", operand->value.local_ref); break;

  case MIR_OP_OP_REF: print(" inst:%u op:%u", operand->value.op_ref.pattern_instruction_index, operand->value.op_ref.operand_index); break;
  case MIR_OP_INST_REF: print(" %u", operand->value.inst_ref); break;

  case MIR_OP_COUNT:
  case MIR_OP_ANY:
    UNREACHABLE();
    break;
  }
}

void isel_print_pattern(ISelPattern *pattern, OpcodeMnemonicFunction opcode_mnemonic) {
  print("\nmatch\n");
  foreach_val (inst, pattern->input) {
    print("%s(", opcode_mnemonic(inst->opcode));
    FOREACH_MIR_OPERAND(inst, op) {
      isel_print_mir_operand(op);
      if (op != opbase + inst->operand_count - 1) print(", ");
    }
    print(")\n");
  }
  print("emit {\n");
  foreach_val (inst, pattern->output) {
    print("  %s(", opcode_mnemonic(inst->opcode));
    FOREACH_MIR_OPERAND(inst, op) {
      isel_print_mir_operand(op);
      if (op != opbase + inst->operand_count - 1) print(", ");
    }
    print(")\n");
  }
  print("}\n");
}

void isel_print_patterns(ISelPatterns *patterns, OpcodeMnemonicFunction opcode_mnemonic) {
  foreach (pattern, *patterns) {
    isel_print_pattern(pattern, opcode_mnemonic);
  }
}
