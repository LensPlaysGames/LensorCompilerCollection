#ifndef INSTRUCTION_SELECTION_H
#define INSTRUCTION_SELECTION_H

#include <codegen/codegen_forward.h>
#include <codegen/machine_ir.h>

typedef enum ISelEnvironmentEntryKind {
  ISEL_ENV_NONE,
  ISEL_ENV_OPCODE,
  ISEL_ENV_OP_KIND,
  ISEL_ENV_INTEGER,
  ISEL_ENV_OP_REF,
  ISEL_ENV_INST_REF,
  ISEL_ENV_COUNT
} ISelEnvironmentEntryKind;

typedef struct ISelValue {
  ISelEnvironmentEntryKind kind;

  isz integer;
  string_buffer text;

  unsigned int inst;
  MIROperandOpRef op;

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

ISelEnvironment isel_env_create_empty(usz initial_capacity);
ISelEnvironment isel_env_create(usz initial_capacity);
void isel_env_delete(ISelEnvironment *env);

/// Return pointer to environment entry associated with the given key.
ISelEnvironmentEntry *isel_env_entry(ISelEnvironment *env, const char *key);

void isel_env_add_integer(ISelEnvironment *env, const char *key, isz value);
void isel_env_add_opcode(ISelEnvironment *env, const char *key, usz opcode);

void isel_env_print_entry(ISelEnvironmentEntry *entry);
void isel_env_print(ISelEnvironment *env);

typedef struct ISelPattern {
  MIRInstructionVector input;
  MIRInstructionVector output;
  ISelEnvironment local;
} ISelPattern;
typedef Vector(ISelPattern)
ISelPatterns;

ISelPatterns isel_parse_file(const char *filepath);

typedef enum ISelCompareValueOption {
  ISEL_DONT_COMPARE_VALUE,
  ISEL_DO_COMPARE_VALUE
} ISelCompareValueOption;
/// Return true iff given instructions match pattern.
bool isel_does_pattern_match(ISelPattern pattern, MIRInstructionVector instructions, ISelCompareValueOption);

void isel_do_selection(MIRFunctionVector mir, ISelPatterns);

void isel_patterns_delete(ISelPatterns *patterns);

void isel_print_mir_operand(MIROperand *operand);

#endif /* INSTRUCTION_SELECTION_H */
