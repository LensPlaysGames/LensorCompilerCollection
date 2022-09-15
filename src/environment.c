#include <environment.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include <parser.h>

void environment_print(Environment env, long long indent) {
  Binding *binding_it = env.bind;
  long long indent_it = indent;
  while (binding_it) {
    indent_it = indent;
    while (indent_it-- > 0) { putchar(' '); }
    printf("%s -> ", node_text(binding_it->id));
    printf("%s\n", node_text(binding_it->value));
    Node *value_iterator = binding_it->value->children;
    while (value_iterator) {
      putchar(' ');
      indent_it = indent + 2;
      while (indent_it-- > 0) { putchar(' '); }
      printf("%s\n", node_text(value_iterator));
      value_iterator = value_iterator->next_child;
    }
    binding_it = binding_it->next;
  }
}

Environment *environment_create(Environment *parent) {
  Environment *env = malloc(sizeof(Environment));
  ASSERT(env, "Could not allocate memory for new environment");
  env->parent = parent;
  env->bind = NULL;
  return env;
}

int environment_set(Environment *env, Node *id, Node *value) {
  // Over-write existing value if ID is already bound in environment.
  if (!env || !id || !value) {
    return 0;
  }
  Binding *binding_it = env->bind;
  while (binding_it) {
    if (node_compare(binding_it->id, id)) {
      binding_it->value = value;
      return 2;
    }
    binding_it = binding_it->next;
  }
  // Create new binding.
  Binding *binding = malloc(sizeof(Binding));
  ASSERT(binding, "Could not allocate new binding for environment");
  binding->id = id;
  binding->value = value;
  binding->next = env->bind;
  env->bind = binding;
  return 1;
}

int environment_set_end(Environment *env, Node *id, Node *value) {
  // Over-write existing value if ID is already bound in environment.
  if (!env || !id || !value) {
    return 0;
  }
  if (!env->bind) {
    return environment_set(env, id, value);
  }
  Binding *last_binding = env->bind;
  Binding *binding_it = env->bind;
  while (binding_it) {
    if (node_compare(binding_it->id, id)) {
      binding_it->value = value;
      return 2;
    }
    last_binding = binding_it;
    binding_it = binding_it->next;
  }
  // Create new binding.
  Binding *binding = calloc(1,sizeof(Binding));
  ASSERT(binding, "Could not allocate new binding for environment");
  binding->id = id;
  binding->value = value;
  last_binding->next = binding;
  return 1;
}

int environment_get(Environment env, Node *id, Node *result) {
  Binding *binding_it = env.bind;
  while (binding_it) {
    if (node_compare(binding_it->id, id)) {
      *result = *binding_it->value;
      return 1;
    }
    binding_it = binding_it->next;
  }
  return 0;
}

int environment_get_by_symbol(Environment env, char *symbol, Node *result) {
  Node *symbol_node = node_symbol(symbol);
  int status = environment_get(env, symbol_node, result);
  free(symbol_node);
  return status;
}

int environment_get_by_value(Environment env, Node *value, Node *result) {
  Binding *binding_it = env.bind;
  while (binding_it) {
    if (node_compare(binding_it->value, value)) {
      *result = *binding_it->id;
      return 1;
    }
    binding_it = binding_it->next;
  }
  return 0;
}
