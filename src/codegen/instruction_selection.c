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
#include <stdbool.h>

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

typedef struct ISelToken {
  ISelTokenKind kind;

  usz integer;
  string_buffer text;
} ISelToken;

typedef struct ISelParser {
  span source;
  ISelToken tok;

  const char *beg;
  const char *end;
  isz lastc;

} ISelParser;

static void isel_next_c(ISelParser *p) {
   /// Keep returning EOF once EOF has been reached.
  if (p->beg >= p->end) {
    p->lastc = 0;
    return;
  }

  /// Read the next character.
  // TODO: Read next utf8 codepoint.
  p->lastc = *p->beg++;
  if (p->lastc == 0) ICE("ISel lexer can not handle null bytes within the input");

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
  DBGASSERT(isel_isstart(p->lastc), "");

  /// The start of the identifier.
  p->tok.kind = TOKEN_IDENTIFIER;
  isel_next_c(p);

  /// Read the rest of the identifier.
  while (isel_iscontinue(p->lastc)) {
    ASSERT(p->lastc <= INT8_MAX, "TODO: Convert utf8 codepoint to encoded bytes and write them to string buffer");
    vector_push(p->tok.text, (char)p->lastc);
    isel_next_c(p);
  }
}
/// Parse a number.
static void parse_number(ISelParser *p, int base) {
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
static void next_number(ISelParser *p) {
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
    /*p->tok.source_location.end = (u32) ((p->beg - 1) - p->source.data);*/ \
    if (p->tok.text.size == 0) ICE("Expected at least one " name " digit"); \
                                                                            \
    /** Actually parse the number. **/                                      \
    return parse_number(p, base);                                           \
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
    return parse_number(p, 10);
  }

  /// Anything else is an error.
  ICE("Invalid integer literal");
}

const struct {
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
    p->tok.kind = TOKEN_EOF;
    return;
  }

  /// Set the token to invalid in case there is an error.
  p->tok.kind = TOKEN_EOF;

  /// Skip whitespace.
  while (isspace((char)p->lastc)) isel_next_c(p);

  /// Start of the token.
  //p->tok.source_location.start = (u32) (p->curr - p->source.data - 1);

  /// Lex the token.
  switch (p->lastc) {

    /// EOF.
    case 0:
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
    case TOKEN_COMMA: FALLTHROUGH;
      p->tok.kind = (int)p->lastc;
      isel_next_c(p);
      break;

    case TOKEN_SEMICOLON: {
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
      // Number or Identifier
      if (isel_isstart(p->lastc)) {
        vector_clear(p->tok.text);
        // TODO: Push utf8 encoding of codepoint.
        vector_push(p->tok.text, (char)p->lastc);
        isel_next_identifier(p);

        for (size_t i = 0; i < sizeof keywords / sizeof *keywords; i++) {
          if (string_eq(keywords[i].kw, p->tok.text)) {
            p->tok.kind = keywords[i].kind;
            return;
          }
        }

        break;
      }

      /// Number.
      if (isdigit((char)p->lastc)) {
        next_number(p);

        /// The character after a number must be a whitespace or delimiter.
        if (isel_isstart(p->lastc)) ICE("Junk after integer literal");
        break;
      }

      /// Anything else is invalid.
      ICE("ISel: invalid token");

    } break;
  }
}

/// <operand> ::= OPERAND_KIND IDENTIFIER [ "=" <expression> ] [ "," ]
static MIROperand isel_parse_operand(ISelParser *p) {
  MIROperand out = {0};

  // TODO: Parse operand kind (should be one of TOKEN_OPKIND_*)

  // TODO: Parse identifier (name bound to this operand while parsing this match)

  // TODO: If an "=" is parsed, parse an initialising expression and
  // use it's value to set the operand value. Ensure type makes sense
  // (i.e. don't initialise a register operand with an identifier).

  // TODO: Eat comma, if present

  return out;
}

/// <inst-spec> ::= <opcode> IDENTIFIER "(" { <operand> } ")" [ "," ]
static MIRInstruction *isel_parse_inst_spec(ISelParser *p) {
  MIRInstruction *out = mir_makenew(MIR_IMMEDIATE);

  // TODO: The current token should be an identifier that maps to a general MIR opcode.

  // TODO: Parse identifier (name bound to this instruction while parsing this match)

  // TODO: Consume opening paren

  // Add operands to instruction as we parse them.
  while (p->tok.kind != TOKEN_RPAREN) {
    if (p->tok.kind == TOKEN_EOF) ICE("ISel reached EOF while parsing operands of instruction");
    mir_add_op(out, isel_parse_operand(p));
  }

  // TODO: Eat comma, if present

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
  ASSERT(p->source.data, "NULL source input");
  ASSERT(p->source.size, "Empty source input");

  p->beg = p->source.data;
  p->end = p->source.data + p->source.size;

  // Advance past metadata (find first empty line).
  // TODO: Parse metadata to get ISA, use ISA to setup initial
  // environment (MIR opcode identifiers specific to the ISA)
  for (const char *last = NULL; p->beg != last && *p->beg && *p->beg != '\n'; p->beg += strcspn(p->beg, "\n") + 1)
    last = p->beg;

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

  string contents = platform_read_file(filepath, &success);
  if (!success) ICE("Failed to read file at \"%s\" for instruction selection", filepath);
  p.source = as_span(contents);

  ISelPatterns out = isel_parse(&p);

  vector_delete(p.tok.text);
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
}
