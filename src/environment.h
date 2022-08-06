#ifndef COMPILER_ENVIRONMENT_H
#define COMPILER_ENVIRONMENT_H

typedef struct Node Node;

// TODO:
// |-- API to create new Binding.
// `-- API to add Binding to environment.
typedef struct Binding {
  Node *id;
  Node *value;
  struct Binding *next;
} Binding;

// TODO: API to create new Environment.
typedef struct Environment {
  struct Environment *parent;
  Binding *bind;
} Environment;

Environment *environment_create(Environment *parent);

/**
 * @retval 0 Failure.
 * @retval 1 Creation of new binding.
 * @retval 2 Existing binding value overwrite (ID unused).
 */
int environment_set(Environment *env, Node *id, Node *value);

/// @return Boolean-like value; 1 for success, 0 for failure.
int environment_get(Environment env, Node *id, Node *result);
/// @return Boolean-like value; 1 for success, 0 for failure.
int environment_get_by_symbol(Environment env, char *symbol, Node *result);

#endif /* COMPILER_ENVIRONMENT_H */
