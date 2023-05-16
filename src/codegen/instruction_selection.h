#ifndef INSTRUCTION_SELECTION_H
#define INSTRUCTION_SELECTION_H

#include <codegen/machine_ir.h>

typedef struct ISelPattern {
  MIRInstructionVector input;
  MIRInstructionVector output;
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

#endif /* INSTRUCTION_SELECTION_H */
