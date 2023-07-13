#include <module.h>

#include <ast.h>
#include <codegen/codegen_forward.h>
#include <utils.h>

#include <stdint.h>

static ModuleDescription make_description() {
  ModuleDescription desc = {0};
  desc.version = INTC_MODULE_VERSION;
  desc.magic[0] = 'I';
  desc.magic[1] = 'N';
  desc.magic[2] = 'T';
  return desc;
}

typedef enum SerialisedFunctionAttribute {
  CEREAL_ATTR_DISCARDABLE,
} SerialisedFunctionAttribute;

typedef struct PACKED SerialisedTypeArray {
  uint64_t element_type_index;
  uint64_t element_count;
} SerialisedTypeArray;

typedef struct PACKED SerialisedTypeInteger {
  uint8_t is_signed;
  uint64_t bit_width;
} SerialisedTypeInteger;

typedef struct PACKED SerialisedMember {
  uint64_t type_index;
  uint32_t byte_offset;
} SerialisedMember;

PACKED_DEFAULT;

typedef Vector(Type *) TypeCache;

static void write_bytes(string_buffer *out, const char *ptr, usz size) {
  /// Synthesise a buffer from the string we need to print.
  span v = {
    .data = ptr,
    .size = size,
  };
  /// Append the buffer to the string.
  vector_append(*out, v);
}

/// Append serialised type to first parameter.
uint64_t serialise_type(string_buffer *out, Type *type, TypeCache *cache) {
  foreach_index (i, *cache) {
    Type *cached_type = cache->data[i];
    if (type_equals(type, cached_type)) return i;
  }

  usz type_index = cache->size;
  vector_push(*cache, type);

  uint8_t tag = (uint8_t)type->kind;
  write_bytes(out, (const char *)&tag, 1);

  switch (type->kind) {
  case TYPE_PRIMITIVE: {
    if (type == t_integer_literal) type = t_integer;

    bool found = false;
    for (Type **primitive = primitive_types; *primitive; ++primitive) {
      if (type == *primitive) {
        uint8_t index = (uint8_t)(primitive - primitive_types);
        write_bytes(out, (const char *)&index, 1);
        found = true;
        break;
      }
    }
    if (!found) ICE("Invalid primitive type, could not serialise...");

  } break;
  case TYPE_NAMED: {
    // [name_length: uint32_t, name]
    uint32_t name_length = (uint32_t)type->named->name.size;
    write_bytes(out, (const char *)&name_length, sizeof(name_length));
    write_bytes(out, type->named->name.data, name_length);
  } break;
  case TYPE_REFERENCE: {
    // Write dummy value of offset of where to find serialised
    // type that is referenced, then serialise the referenced type,
    // then update the dummy value with the real one.
    uint64_t dummy_value = (uint64_t)0xdeadbeeff00dcafeULL;
    usz type_offset_offset = out->size;
    write_bytes(out, (const char *)&dummy_value, sizeof(dummy_value));

    usz type_offset = serialise_type(out, type->reference.to, cache);
    uint64_t *type_offset_offset_ptr = (uint64_t *)(out->data + type_offset_offset);
    *type_offset_offset_ptr = type_offset;

  } break;
  case TYPE_POINTER: {
    // Just like references
    uint64_t dummy_value = (uint64_t)0xdeadbeeff00dcafeULL;
    usz type_offset_offset = out->size;
    write_bytes(out, (const char *)&dummy_value, sizeof(dummy_value));
    usz type_offset = serialise_type(out, type->pointer.to, cache);
    uint64_t *type_offset_offset_ptr = (uint64_t *)(out->data + type_offset_offset);
    *type_offset_offset_ptr = type_offset;
  } break;
  case TYPE_ARRAY: {
    SerialisedTypeArray cereal = {0};
    cereal.element_count = type->array.size;

    usz element_type_index = out->size + offsetof(SerialisedTypeArray, element_type_index);
    write_bytes(out, (const char *)&cereal, sizeof(cereal));

    usz element_type = serialise_type(out, type->array.of, cache);
    uint64_t *element_type_index_ptr = (uint64_t*)(out->data + element_type_index);
    *element_type_index_ptr = element_type;
  } break;
  case TYPE_FUNCTION: {
    // [attributes : uint32_t]
    // [param_count : uint32_t, param_types, return_type],
    // [param_name_length : uint32_t, param_name]*

    uint32_t attr = 0;
    attr |= (uint32_t)type->function.attr_discardable << CEREAL_ATTR_DISCARDABLE;
    write_bytes(out, (const char *)&attr, 4);

    uint32_t param_count = (uint32_t)type->function.parameters.size;
    write_bytes(out, (const char *)&param_count, 4);

    vector_reserve(*out, param_count * sizeof(uint64_t));
    usz params_byte_offset = out->size;
    out->size += param_count * sizeof(uint64_t);

    vector_reserve(*out, sizeof(uint64_t));
    usz return_byte_offset = out->size;
    out->size += sizeof(uint64_t);

    foreach (param, type->function.parameters) {
      uint32_t param_name_length = (uint32_t)param->name.size;
      write_bytes(out, (const char *)&param_name_length, 4);
      format_to(out, "%S", param->name);
    }

    foreach_index (param_index, type->function.parameters) {
      Parameter *param = type->function.parameters.data + param_index;
      uint64_t *member_offset = (uint64_t*)(out->data + params_byte_offset + (param_index * sizeof(uint64_t)));
      *member_offset = serialise_type(out, param->type, cache);
    }

    uint64_t *return_offset = (uint64_t*)(out->data + return_byte_offset);
    *return_offset = serialise_type(out, type->function.return_type, cache);

  } break;
  case TYPE_STRUCT: {
    // [member_count : uint32_t, member_types],
    // [alignment : uint32_t],
    // [name_length : uint32_t, name],
    // [member_byte_offset : uint32_t, member_name_length : uint32_t, member_name]*

    // Writing member count
    uint32_t member_count = (uint32_t)type->structure.members.size;
    write_bytes(out, (const char *)&member_count, 4);

    // Writing member entry table
    vector_reserve(*out, member_count * sizeof(SerialisedMember));
    usz members_byte_offset = out->size;
    out->size += member_count * sizeof(SerialisedMember);

    // Writing alignment
    uint32_t alignment = (uint32_t)type->structure.alignment;
    write_bytes(out, (const char *)&alignment, sizeof(alignment));

    // Writing struct type name
    uint32_t name_length = (uint32_t)type->structure.decl->struct_decl->name.size;
    write_bytes(out, (const char *)&name_length, 4);
    format_to(out, "%S", type->structure.decl->struct_decl->name);

    // Writing member names
    foreach (member, type->structure.members) {
      uint32_t member_name_length = (uint32_t)member->name.size;
      write_bytes(out, (const char *)&member_name_length, 4);
      format_to(out, "%S", member->name);
    }

    // Fixups
    foreach_index (member_index, type->structure.members) {
      Member *member = type->structure.members.data + member_index;
      SerialisedMember *member_offset = (SerialisedMember*)(out->data + members_byte_offset + (member_index * sizeof(uint64_t)));
      member_offset->byte_offset = (uint32_t)member->byte_offset;
      member_offset->type_index = serialise_type(out, member->type, cache);
    }

  } break;
  case TYPE_INTEGER: {
    SerialisedTypeInteger cereal = {0};
    cereal.is_signed = type->integer.is_signed;
    cereal.bit_width = type->integer.bit_width;
    write_bytes(out, (const char *)&cereal, sizeof(cereal));
  } break;

  default: UNREACHABLE();
  case TYPE_COUNT: UNREACHABLE();
  }

  return type_index;
}

// @return The `from` pointer advanced past deserialised type.
uint8_t *deserialise_type(uint8_t *from, Type *type, Type **types) {
  uint8_t type_tag = *from;
  switch ((TypeKind)type_tag) {
  case TYPE_PRIMITIVE: {
    // [index: uint8_t]
    uint8_t index = *(from + 1);
    ASSERT(index < 4, "Deserialised primitive index %d is invalid...", (int)index);

    type->kind = TYPE_NAMED;
    type->named = calloc(1, sizeof(Symbol));
    type->named->kind = SYM_TYPE;
    type->named->val.type = primitive_types[index];
    type->named->name = string_dup(primitive_types[index]->primitive.name);

    return from + 1 + sizeof(uint8_t);
  }
  case TYPE_NAMED: {
    // [name_length, name]
    span name = {0};
    name.size = *(uint32_t*)(from + 1);
    name.data = (const char *)(from + 1 + sizeof(uint32_t));
    type->kind = TYPE_NAMED;

    Symbol *sym = calloc(1, sizeof(Symbol));
    sym->kind = SYM_TYPE;
    sym->name = string_dup(name);
    sym->val.type = type;
    // FIXME: Do we need to set scope? Hopefully not..

    type->named = sym;

    return from + 1 + sizeof(uint32_t) + name.size;
  }
  case TYPE_POINTER: {
    // [type_index]
    uint64_t *type_index = (uint64_t*)(from + 1);
    type->kind = TYPE_POINTER;
    type->reference.to = types[*type_index];
    return from + 1 + sizeof(uint64_t);
  }
  case TYPE_REFERENCE: {
    // [type_index]
    uint64_t *type_index = (uint64_t*)(from + 1);
    type->kind = TYPE_REFERENCE;
    type->reference.to = types[*type_index];
    return from + 1 + sizeof(uint64_t);
  }
  case TYPE_ARRAY: {
    // [SerialisedTypeArray]
    SerialisedTypeArray *array = (SerialisedTypeArray*)(from + 1);
    type->kind = TYPE_ARRAY;
    type->array.size = array->element_count;
    type->array.of = types[array->element_type_index];
    return from + 1 + sizeof(SerialisedTypeArray);
  }
  case TYPE_FUNCTION: {
    // [attributes : uint32_t]
    // [param_count : uint32_t, param_types, return_type],
    // [param_name_length : uint32_t, param_name]*
    uint8_t *from_it = from + 1;

    uint32_t attributes = *(uint32_t*)from_it;
    from_it += sizeof(uint32_t);

    type->kind = TYPE_FUNCTION;

    type->function.attr_discardable = attributes & (1 << CEREAL_ATTR_DISCARDABLE);

    uint32_t param_count = *(uint32_t*)from_it;
    from_it += sizeof(uint32_t);

    vector_clear(type->function.parameters);
    for (uint32_t param_index = 0; param_index < param_count; ++param_index) {
      Parameter param = {0};

      uint64_t param_type_index = *(uint64_t*)from_it;
      from_it += sizeof(uint64_t);

      param.type = types[param_type_index];
      vector_push(type->function.parameters, param);
    }

    uint64_t return_type_index = *(uint64_t*)from_it;
    from_it += sizeof(uint64_t);

    type->function.return_type = types[return_type_index];

    for (uint32_t i = 0; i < param_count; ++i) {
      uint32_t param_name_length = *(uint32_t*)from_it;
      from_it += sizeof(uint32_t);

      ASSERT(param_name_length < 1000, "Sorry, parameter names longer than a kilobyte aren't supported, you psycho");

      string_buffer param_name = {0};
      vector_reserve(param_name, param_name_length);
      param_name.size = param_name_length;
      memcpy(param_name.data, from_it, param_name_length);
      from_it += param_name_length;

      // MOVE param_name vector into param.name string
      type->function.parameters.data[i].name.data = param_name.data;
      type->function.parameters.data[i].name.size = param_name.size;
      param_name.data = NULL;
      param_name.size = 0;
      param_name.capacity = 0;
    }
    return from_it;
  }
  case TYPE_STRUCT: {
    // [member_count : uint32_t, member_types],
    // [alignment : uint32_t],
    // [name_length : uint32_t, name],
    // [member_byte_offset : uint32_t, member_name_length : uint32_t, member_name]*
    type->kind = TYPE_STRUCT;
    TODO("Deserialise structs");
  }
  case TYPE_INTEGER: {
    // [SerialisedTypeInteger]
    SerialisedTypeInteger *integer = (SerialisedTypeInteger *)(from + 1);
    type->kind = TYPE_INTEGER;
    type->integer.bit_width = integer->bit_width;
    type->integer.is_signed = integer->is_signed;
    return from + 1 + sizeof(SerialisedTypeInteger);
  }
  default: ICE("Unrecognized type tag in module metadata");
  }
  UNREACHABLE();
}

Module *deserialise_module(span metadata) {
  Module *module = ast_create();
  module->is_module = true;

  ModuleDescription *desc = (ModuleDescription*)metadata.data;
  if (desc->version != INTC_MODULE_VERSION
      || desc->magic[0] != INTC_MODULE_MAG0
      || desc->magic[1] != INTC_MODULE_MAG1
      || desc->magic[2] != INTC_MODULE_MAG2)
    ICE("Invalid module description header");

  // Preallocate types
  vector_reserve(module->_types_, desc->type_count);
  Type *type_blob = calloc(desc->type_count, sizeof(Type));
  for (size_t i = 0; i < desc->type_count; ++i)
    module->_types_.data[i] = type_blob + i;

  // Deserialise type info
  uint8_t *type_table = (uint8_t*)(metadata.data + desc->type_table_offset);
  for (usz i = 0; i < desc->type_count; ++i) {
    type_table = deserialise_type(type_table, module->_types_.data[i], module->_types_.data);
  }

  // Starting at (metadata.data + sizeof(*desc)),
  // parse module declarations (name + index in type table)
  uint8_t* begin_ptr = (uint8_t*)(metadata.data + sizeof(*desc));

  for (size_t i = 0; i < desc->declaration_count; ++i) {

    // Get pointer within preallocated types using type index
    uint64_t type_index = *(uint64_t *)(begin_ptr);
    Type *type = module->_types_.data[type_index];
    //print("deserialised type: %T\n", type);

    // Get name length and then the name data
    uint32_t name_length = *(uint32_t *)(begin_ptr + sizeof(type_index));
    char *name_ptr = (char *)(begin_ptr + sizeof(type_index) + sizeof(uint32_t));
    string_buffer name = {0};
    vector_reserve(name, name_length);
    name.size = name_length;
    memcpy(name.data, name_ptr, name_length);
    //print("deserialised name: %S\n", as_span(name));

    // Construct an AST node to represent the deserialised declaration
    Node *node = NULL;
    switch (type->kind) {
    case TYPE_FUNCTION: {
      node = ast_make_function_reference(module, (loc){0}, as_span(name));
      node->type = type;
    } break;
    case TYPE_PRIMITIVE:
    case TYPE_NAMED:
    case TYPE_POINTER:
    case TYPE_REFERENCE:
    case TYPE_ARRAY:
    case TYPE_STRUCT:
    case TYPE_INTEGER: {
      node = ast_make_declaration(module, (loc){0}, type, LINKAGE_IMPORTED, as_span(name), NULL);
    } break;

    case TYPE_COUNT:
    default: ICE("Unhandled type kind in switch...");
    }

    // ADD NODE TO MODULE EXPORTS
    vector_push(module->exports, node);

    // ADVANCE deserialisation pointer
    begin_ptr += sizeof(type_index) + sizeof(name_length) + name_length;
  }

  foreach_val (export, module->exports) {
    ast_print_node(export);
  }

  return module;
}

string serialise_module(CodegenContext *context, Module *module) {
  ASSERT(module->is_module, "To serialise an AST, it must be a module");
  ModuleDescription desc = make_description();
  string_buffer out = {0};

  // module code -> object file containing module description -> used
  // by Intercept compiler to typecheck calls to this module and such.

  /// Map types to indices within type table.
  /// Map symbols to types
  // [type_index: uint64_t, name_length: uint32_t, name]
  TypeCache cache = {0};
  string_buffer types = {0};
  string_buffer declarations = {0};
  foreach_val (node, module->exports) {
    uint64_t type_index = serialise_type(&types, node->type, &cache);
    write_bytes(&declarations, (const char*)&type_index, sizeof(type_index));
    if (node->kind == NODE_DECLARATION) {
      uint32_t name_length = (uint32_t)node->declaration.name.size;
      write_bytes(&declarations, (const char*)&name_length, sizeof(name_length));
      write_bytes(&declarations, (const char*)node->declaration.name.data, node->declaration.name.size);
    } else if (node->kind == NODE_FUNCTION_REFERENCE) {
      uint32_t name_length = (uint32_t)node->funcref.name.size;
      write_bytes(&declarations, (const char*)&name_length, sizeof(name_length));
      write_bytes(&declarations, (const char*)node->funcref.name.data, node->funcref.name.size);
    } else {
      ast_print_node(node);
      ICE("You've exported something absolutely ridiculous");
    }
  }

  // BEGIN WRITING

  // Write module description header
  write_bytes(&out, (const char *)&desc, sizeof(desc));

  // Write module declarations
  vector_append(out, declarations);

  // Write type table
  usz type_table_offset = out.size;
  vector_append(out, types);

  usz module_name_offset = out.size;
  vector_append(out, module->module_name);
  vector_push(out, '\0');

  // END WRITING

  // Fixup header with references.
  ModuleDescription *desc_ptr = (ModuleDescription *)out.data;
  desc_ptr->size = (uint32_t)out.size;
  desc_ptr->type_count = (uint32_t)cache.size;
  desc_ptr->type_table_offset = (uint32_t)type_table_offset;
  desc_ptr->name_offset = (uint32_t)module_name_offset;
  desc_ptr->declaration_count = (uint32_t)module->exports.size;

  string ret = {0};
  ret.size = out.size;
  ret.data = out.data;
  return ret;
}
