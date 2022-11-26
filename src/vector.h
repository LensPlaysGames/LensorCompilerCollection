#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>

/// Iterate from 0 to until (exclusive).
#define FOR(i, until) for (size_t i = 0; i < (until); ++i)

/// Define a vector of `type`.
#define VECTOR(type) \
  struct {           \
    type *data;      \
    size_t size;    \
    size_t capacity; \
  }

/// Free the memory used by a vector, but not the vector itself if it's on the heap.
#define VECTOR_DELETE(vector) \
  do {                        \
    free((vector)->data);     \
    (vector)->data = NULL;    \
    (vector)->size = 0;      \
    (vector)->capacity = 0;   \
  } while (0)

/// Define a vector on the heap.
#define MAKE_VECTOR_HEAP(type) \
  calloc(1, sizeof(struct {    \
    type *data;                \
    size_t size;              \
    size_t capacity;           \
  }))

#if 0
/// Iterate over each element of a vector.
#define VECTOR_FOREACH(element, vector)                        \
  for (__typeof__((vector)->data) element = (vector)->data; \
    element < (vector)->data + (vector)->size;                \
    element++)

/// Iterate over each element of a vector, and dereference the element.
#define VECTOR_FOREACH_PTR(element, vector)                                          \
  for (__typeof__(*(vector)->data) *element##_ptr = (vector)->data, element = NULL; \
    element##_ptr < (vector)->data + (vector)->size && (element = *element##_ptr, 1); /* "=", not "=="! */ \
    element##_ptr++)
#else
#define VECTOR_FOREACH(type, element, vector) \
  for (type *element = (vector)->data; element < (vector)->data + (vector)->size; element++)
#define VECTOR_FOREACH_PTR(type, element, vector) \
  for (type *element##_ptr = (vector)->data, *element = NULL; \
      element##_ptr < (vector)->data + (vector)->size && (element = *element##_ptr, 1); /* "=", not "=="! */ \
      element##_ptr++)
#endif

/// Iterate over each index and element of a vector.
#define VECTOR_FOREACH_INDEX(index, vector) \
  for (size_t index = 0; index < (vector)->size; index++)

/// Ensure that there is space for at least (vector->size + elements) many elements.
#define VECTOR_RESERVE(vector, elements)                                                                             \
  do {                                                                                                               \
    if ((vector)->capacity < (vector)->size + (elements)) {                                                         \
      (vector)->capacity += (elements);                                                                              \
      (vector)->capacity *= 2;                                                                                       \
      if (!(vector)->data) {                                                                                         \
        (vector)->data = calloc((vector)->capacity, sizeof *(vector)->data);                                         \
      } else {                                                                                                       \
        (vector)->data = realloc((vector)->data, (vector)->capacity * sizeof *(vector)->data);                       \
        memset((vector)->data + (vector)->size, 0, ((vector)->capacity - (vector)->size) * sizeof *(vector)->data);\
      }                                                                                                              \
    }                                                                                                                \
  } while (0)

/// Push an element onto the vector.
#define VECTOR_PUSH(vector, element)               \
  do {                                             \
    VECTOR_RESERVE((vector), 1);                   \
    (vector)->data[(vector)->size++] = (element); \
  } while (0)

/// Pop an element from the vector.
#define VECTOR_POP(vector) ((vector)->data[--(vector)->size])

/// Remove an element from a vector by index. This may change the order of elements in the vector.
#define VECTOR_REMOVE_UNORDERED(vector, index)                 \
  do {                                                         \
    (vector)->data[index] = (vector)->data[--(vector)->size]; \
  } while (0)

/// Remove an element from a vector. This may change the order of elements in the vector.
#define VECTOR_REMOVE_ELEMENT_UNORDERED(vector, element)                   \
  do {                                                                     \
    size_t _index = 0;                                                     \
    for (; _index < (vector)->size; _index++) {                           \
      if ((vector)->data[_index] == element) { break; }                    \
    }                                                                      \
    if (_index < (vector)->size) VECTOR_REMOVE_UNORDERED(vector, _index); \
  } while (0)

/// Append a vector to another vector
#define VECTOR_APPEND(to, from) \
  do {                          \
    VECTOR_RESERVE((to), (from)->size); \
    memcpy((to)->data + (to)->size, (from)->data, (from)->size * sizeof *(from)->data); \
    (to)->size += (from)->size; \
  } while (0)

/// Remove all elements from a vector.
#define VECTOR_CLEAR(vector) \
  do {                       \
    (vector)->size = 0;     \
  } while (0)

/// Get the last element of a vector.
/// TODO: Convert ASSERT() into an expression and insert an ASSERT() here.
#define VECTOR_BACK(vector) ((vector)->data[(vector)->size - 1])

#endif // VECTOR_H