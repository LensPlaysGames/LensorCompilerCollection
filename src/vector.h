#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>
#include <string.h>

/// Iterate from 0 to until (exclusive).
#define FOR(i, until) for (size_t i = 0; i < (until); ++i)

/// Define a vector of `type`.
#define VECTOR(type)   \
  struct {             \
    type *data;        \
    size_t size;       \
    size_t capacity;   \
  }

/// Free the memory used by a vector, but not the vector itself if it's on the heap.
#define VECTOR_DELETE(vector) \
  do {                        \
    free((vector).data);     \
    (vector).data = NULL;    \
    (vector).size = 0;      \
    (vector).capacity = 0;   \
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
  for (__typeof__((vector).data) element = (vector).data; \
    element < (vector).data + (vector).size;                \
    element++)

/// Iterate over each element of a vector, and dereference the element.
#define VECTOR_FOREACH_PTR(element, vector)                                          \
  for (__typeof__(*(vector).data) *element##_ptr = (vector).data, element = NULL; \
    element##_ptr < (vector).data + (vector).size && (element = *element##_ptr, 1); /* "=", not "=="! */ \
    element##_ptr++)
#else
#define VECTOR_FOREACH(type, element, vector) \
  for (type *element = (vector).data; element < (vector).data + (vector).size; element++)
#define VECTOR_FOREACH_PTR(type, element, vector) \
  for (type *element##_ptr = (vector).data, *element = NULL; \
      element##_ptr < (vector).data + (vector).size && (element = *element##_ptr, 1); /* "=", not "=="! */ \
      element##_ptr++)
#endif

/// Iterate over each index and element of a vector.
#define VECTOR_FOREACH_INDEX(index, vector) \
  for (size_t index = 0; index < (vector).size; index++)

/// Ensure that there is space for at least (vector->size + elements) many elements.
#define VECTOR_RESERVE(vector, elements)                                                                             \
  do {                                                                                                               \
    if ((vector).capacity < (vector).size + (elements)) {                                                         \
      (vector).capacity += (elements);                                                                              \
      (vector).capacity *= 2;                                                                                       \
      if (!(vector).data) {                                                                                         \
        (vector).data = calloc((vector).capacity, sizeof *(vector).data);                                         \
      } else {                                                                                                       \
        (vector).data = realloc((vector).data, (vector).capacity * sizeof *(vector).data);                       \
        memset((vector).data + (vector).size, 0, ((vector).capacity - (vector).size) * sizeof *(vector).data);\
      }                                                                                                              \
    }                                                                                                                \
  } while (0)

/// Push an element onto the vector.
#define VECTOR_PUSH(vector, element)               \
  do {                                             \
    VECTOR_RESERVE((vector), 1);                   \
    (vector).data[(vector).size++] = (element); \
  } while (0)

/// Pop an element from the vector.
#define VECTOR_POP(vector) ((vector).data[--(vector).size])

/// Remove an element from a vector by index. This may change the order of elements in the vector.
#define VECTOR_REMOVE_UNORDERED(vector, index)                 \
  do {                                                         \
    (vector).data[index] = (vector).data[--(vector).size]; \
  } while (0)

/// Remove an element from a vector. This may change the order of elements in the vector.
#define VECTOR_REMOVE_ELEMENT_UNORDERED(vector, element)                   \
  do {                                                                     \
    size_t _index = 0;                                                     \
    for (; _index < (vector).size; _index++) {                           \
      if (memcmp((vector).data + _index, &(element), sizeof(element)) == 0) { break; }                    \
    }                                                                      \
    if (_index < (vector).size) VECTOR_REMOVE_UNORDERED(vector, _index); \
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
    (vector).size = 0;     \
  } while (0)

/// Get the last element of a vector.
/// TODO: Convert ASSERT() into an expression and insert an ASSERT() here.
#define VECTOR_BACK(vector) ((vector).data[(vector).size - 1])
#define VECTOR_BACK_OR(vector, default) ((vector).size ? VECTOR_BACK(vector) : (default))

/// Get the first element of a vector.
#define VECTOR_FRONT(vector) ((vector).data[0])
#define VECTOR_FRONT_OR(vector, default) ((vector).size ? VECTOR_FRONT(vector) : (default))

/// Insert an element into a vector at before the given index.
#define VECTOR_INSERT(vector, pos, element)                                                      \
  do {                                                                                           \
    if ((pos) >= (vector).data + (vector).size) {                                              \
      VECTOR_PUSH(vector, element);                                                              \
    } else {                                                                                     \
      VECTOR_RESERVE((vector), 1);                                                               \
      memmove((pos) + 1, (pos), ((vector).size - ((pos) - (vector).data)) * sizeof *(vector).data); \
      *(pos) = element;                                                                            \
      (vector).size++;                                                                          \
    }                                                                                            \
  } while (0)

/// Remove an element from the vector.
#define VECTOR_REMOVE_ELEMENT(vector, element)                                                                               \
  do {                                                                                                                       \
    size_t _index = 0;                                                                                                       \
    for (; _index < (vector).size; _index++) {                                                                              \
      if ((vector).data[_index] == element) { break; }                                                                      \
    }                                                                                                                        \
    if (_index < (vector).size) {                                                                                           \
      memmove((vector).data + _index, (vector).data + _index + 1, ((vector).size - _index - 1) * sizeof *(vector).data); \
      (vector).size--;                                                                                                      \
    }                                                                                                                        \
  } while (0)

#define VECTOR_APPEND_ALL(to, from) \
  do {                              \
    VECTOR_RESERVE((to), (from).size); \
    memcpy((to).data + (to).size, (from).data, (from).size * sizeof *(from).data); \
    (to).size += (from).size;     \
  } while (0)

#define VECTOR_INSERT_BEFORE(vector, element, before)       \
  do {                                                      \
    size_t _index = 0;                                      \
    for (; _index < (vector).size; _index++) {              \
      if ((vector).data[_index] == before) { break; }       \
    }                                                       \
    VECTOR_INSERT(vector, (vector).data + _index, element); \
  } while (0)

#define VECTOR_INSERT_AFTER(vector, element, after) VECTOR_INSERT_BEFORE(vector, element, (after + 1))

#define VECTOR_CONTAINS(vector, element, out) \
  do {                                        \
    size_t _index = 0;                        \
    for (; _index < (vector).size; _index++) { \
      if ((vector).data[_index] == element) { \
        (out) = true;                         \
        break;                                \
      }                                       \
    }                                         \
    if (_index == (vector).size) {            \
      (out) = false;                          \
    }                                         \
  } while (0)

#define VECTOR_FIND_IF(vector, out, index, ...) \
    do {                                              \
        size_t index = 0;                             \
        for (; index < (vector).size; index++) {      \
            if (__VA_ARGS__) {                        \
                (out) = (vector).data + index;        \
                break;                                \
            }                                         \
        }                                             \
        if (index == (vector).size) {                 \
            (out) = NULL;                             \
        }                                             \
    } while (0)

#define DLIST_NODE(type) \
  struct {               \
    type *prev;          \
    type *next;          \
  }

#define DLIST(type) \
  struct {          \
    type *first;    \
    type *last;     \
  }

#define DLIST_PUSH_BACK(list, element) \
  do {                                 \
    if (!(list).first) {               \
      (list).first = (element);        \
    } else {                           \
      (list).last->next = (element);   \
      (element)->prev = (list).last;   \
    }                                  \
    (list).last = (element);           \
  } while (0)

#define DLIST_PUSH_FRONT(list, element) \
  do {                                  \
    if (!(list).last) {                 \
      (list).last = (element);          \
    } else {                            \
      (list).first->prev = (element);   \
      (element)->next = (list).first;   \
    }                                   \
    (list).first = (element);           \
  } while (0)

#define DLIST_REMOVE(list, element)            \
  do {                                         \
    if ((element)->prev) {                     \
      (element)->prev->next = (element)->next; \
    } else {                                   \
      (list).first = (element)->next;          \
    }                                          \
    if ((element)->next) {                     \
      (element)->next->prev = (element)->prev; \
    } else {                                   \
      (list).last = (element)->prev;           \
    }                                          \
  } while (0)

#define DLIST_POP_BACK(list)           \
  do {                                 \
    if ((list).last) {                 \
      DLIST_REMOVE(list, (list).last); \
    }                                  \
  } while (0)

#define DLIST_POP_FRONT(list)           \
  do {                                  \
    if ((list).first) {                 \
      DLIST_REMOVE(list, (list).first); \
    }                                   \
  } while (0)

#define DLIST_INSERT_BEFORE(list, element, before) \
  do {                                             \
    (element)->prev = (before)->prev;              \
    (element)->next = (before);                    \
    if ((before)->prev) {                          \
      (before)->prev->next = (element);            \
    } else {                                       \
      (list).first = (element);                    \
    }                                              \
    (before)->prev = (element);                    \
  } while (0)

#define DLIST_INSERT_AFTER(list, element, after) \
  do {                                           \
    (element)->prev = (after);                   \
    (element)->next = (after)->next;             \
    if ((after)->next) {                         \
      (after)->next->prev = (element);           \
    } else {                                     \
      (list).last = (element);                   \
    }                                            \
    (after)->next = (element);                   \
  } while (0)

#define DLIST_SIZE(type, size_out, list)                  \
  do {                                                    \
    size_t _size = 0;                                     \
    for (type _it = (list).first; _it; _it = _it->next) { \
      _size++;                                            \
    }                                                     \
    size_out = _size;                                     \
  } while (0)

#define DLIST_FOREACH(type, it, list) \
  for (type it = (list).first; it; it = it->next)

#endif // VECTOR_H