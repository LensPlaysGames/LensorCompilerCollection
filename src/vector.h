#ifndef VECTOR_H
#define VECTOR_H

#ifndef _MSVC_VER
#define VECTOR_TYPEOF __typeof__
#elif defined(__cplusplus)
#define VECTOR_TYPEOF decltype
#else
#error This program requires either __typeof__ or decltype to work properly. \
       If you are using MSVC, please compile this in C++ mode.
#endif

/// Iterate from 0 to until (exclusive).
#define FOR(i, until) for (size_t i = 0; i < (until); ++i)

/// Define a vector of `type`.
#define VECTOR(type) \
  struct {           \
    type *data;      \
    size_t count;    \
    size_t capacity; \
  }

/// Free the memory used by a vector, but not the vector itself if it's on the heap.
#define VECTOR_DELETE(vector) \
  do {                        \
    free((vector)->data);     \
    (vector)->data = NULL;    \
    (vector)->count = 0;      \
    (vector)->capacity = 0;   \
  } while (0)

/// Define a vector on the heap.
#define MAKE_VECTOR_HEAP(type) \
  calloc(1, sizeof(struct {    \
    type *data;                \
    size_t count;              \
    size_t capacity;           \
  }))

/// Iterate over each element of a vector.
#define VECTOR_FOREACH(element, vector)                        \
  for (VECTOR_TYPEOF((vector)->data) element = (vector)->data; \
    element < (vector)->data + (vector)->count;                \
    element++)

/// Iterate over each element of a vector, and dereference the element.
#define VECTOR_FOREACH_PTR(element, vector)                                          \
  for (VECTOR_TYPEOF(*(vector)->data) *element##_ptr = (vector)->data, element = NULL; \
    element##_ptr < (vector)->data + (vector)->count && (element = *element##_ptr); /* "=", not "=="! */ \
    element##_ptr++)

/// Iterate over each index and element of a vector.
#define VECTOR_FOREACH_INDEX(index, vector) \
  for (size_t index = 0; index < (vector)->count; index++)

/// Ensure that there is space for at least (vector->count + elements) many elements.
#define VECTOR_RESERVE(vector, elements)                                                     \
  do {                                                                                       \
    if ((vector)->capacity < (vector)->count + (elements)) {                                 \
      (vector)->capacity += (elements);                                                      \
      (vector)->capacity *= 2;                                                               \
      (vector)->data = realloc((vector)->data, (vector)->capacity * sizeof *(vector)->data); \
    }                                                                                        \
  } while (0)

/// Push an element onto the vector.
#define VECTOR_PUSH(vector, element)               \
  do {                                             \
    VECTOR_RESERVE((vector), 1);                   \
    (vector)->data[(vector)->count++] = (element); \
  } while (0)

/// Pop an element from the vector.
#define VECTOR_POP(vector) ((vector)->data[--(vector)->count])

/// Remove an element from a vector by index. This may change the order of elements in the vector.
#define VECTOR_REMOVE_UNORDERED(vector, index)                 \
  do {                                                         \
    (vector)->data[index] = (vector)->data[--(vector)->count]; \
  } while (0)

/// Remove an element from a vector. This may change the order of elements in the vector.
#define VECTOR_REMOVE_ELEMENT_UNORDERED(vector, element)                   \
  do {                                                                     \
    size_t _index = 0;                                                     \
    for (; _index < (vector)->count; _index++) {                           \
      if ((vector)->data[_index] == element) { break; }                    \
    }                                                                      \
    if (_index < (vector)->count) VECTOR_REMOVE_UNORDERED(vector, _index); \
  } while (0)

/// Append a vector to another vector
#define VECTOR_APPEND(to, from) \
  do {                          \
    VECTOR_RESERVE((to), (from)->count); \
    memcpy((to)->data + (to)->count, (from)->data, (from)->count * sizeof *(from)->data); \
    (to)->count += (from)->count; \
  } while (0)

/// Remove all elements from a vector.
#define VECTOR_CLEAR(vector) \
  do {                       \
    (vector)->count = 0;     \
  } while (0)

/// Get the last element of a vector.
/// TODO: Convert ASSERT() into an expression and insert an ASSERT() here.
#define VECTOR_BACK(vector) ((vector)->data[(vector)->count - 1])

#endif // VECTOR_H
