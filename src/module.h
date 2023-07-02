#ifndef MODULE_H
#define MODULE_H

#include <ast.h>
#include <codegen/codegen_forward.h>
#include <utils.h>

#define INTC_MODULE_VERSION 1
#define INTC_MODULE_MAG0 'I'
#define INTC_MODULE_MAG1 'N'
#define INTC_MODULE_MAG2 'T'
#define INTC_MODULE_SECTION_NAME ".intercept_module_section"

NODISCARD string serialise_module(CodegenContext *context, AST *module);

AST *deserialise_module(span metadata);

// ModuleDescription { ModuleDeclaration } <anything trailing>
typedef struct ModuleDescription {
  // See INTC_MODULE_VERSION macro.
  uint8_t version;

  // See INTC_MODULE_MAG* macros.
  uint8_t magic[3];

  // Size of entire description including this header.
  uint32_t size;

  uint32_t type_count;
  uint32_t type_table_offset;

  uint32_t declaration_count;

  // Offset to NULL-terminated module name.
  uint32_t name_offset;

  // NOTE: list of ModuleDeclaration follows ModuleDescription

} ModuleDescription;

#endif /* MODULE_H */
