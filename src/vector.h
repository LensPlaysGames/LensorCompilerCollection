#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>
#include <string.h>
#include <utils.h>

/// Iterate from 0 to until (exclusive).
/*#define FOR(i, until) for (size_t i = 0; i < (until); ++i)*/

/// Define a vector of `type`.
#define Vector(type) \
  struct {           \
    type *data;      \
    size_t size;     \
    size_t capacity; \
  }

/// Free the memory used by a vector, but not the vector itself if it's on the heap.
#define vector_delete(vector) \
  do {                        \
    free((vector).data);      \
    (vector).data = NULL;     \
    (vector).size = 0;        \
    (vector).capacity = 0;    \
  } while (0)

/// Iterate over a vector by reference.
#define foreach(element, vector) \
  for (__typeof__(*(vector).data) *element = (vector).data; element < (vector).data + (vector).size; element++)

/// Iterate over a vector of pointers by value.
#define foreach_val(element, vector)                                                                           \
  for (__typeof__(*(vector).data) *element##_ptr = (vector).data, element = NULL;                              \
       element##_ptr < (vector).data + (vector).size && (element = *element##_ptr, true); /* "=", not "=="! */ \
       element##_ptr++)

#define foreach_rev(element, vector) \
  if ((vector).size)                 \
    for (__typeof__(*(vector).data) *element = (vector).data + (vector).size - 1; element >= (vector).data; --element)

#define foreach_ptr_rev(element, vector)                                                                  \
  if ((vector).size)                                                                                      \
    for (__typeof__(*(vector).data) *element##_ptr = (vector).data + ((vector).size - 1), element = NULL; \
         element##_ptr >= (vector).data && (element = *element##_ptr, true); /* "=", not "=="! */         \
         element##_ptr--)

/// Iterate over each index and element of a vector.
#define foreach_index(index, vector) \
  for (size_t index = 0; index < (vector).size; index++)

/// Ensure that there is space for at least (vector->size + elements) many elements.
#define vector_reserve(vector, elements)                                                                       \
  do {                                                                                                         \
    if ((vector).capacity < (vector).size + (elements)) {                                                      \
      (vector).capacity += (elements);                                                                         \
      (vector).capacity *= 2;                                                                                  \
      if (!(vector).data) {                                                                                    \
        (vector).data = calloc((vector).capacity, sizeof *(vector).data);                                      \
      } else {                                                                                                 \
        (vector).data = realloc((vector).data, (vector).capacity * sizeof *(vector).data);                     \
        memset((vector).data + (vector).size, 0, ((vector).capacity - (vector).size) * sizeof *(vector).data); \
      }                                                                                                        \
    }                                                                                                          \
  } while (0)

/// Push an element onto the vector.
#define vector_push(vector, element)            \
  do {                                          \
    vector_reserve((vector), 1);                \
    (vector).data[(vector).size++] = (element); \
  } while (0)

/// Pop an element from the vector.
#define vector_pop(vector) ((vector).data[--(vector).size])

/// Remove an element from a vector by index. This may change the order of elements in the vector.
#define vector_remove_unordered(vector, index) ((void)((vector).data[index] = (vector).data[--(vector).size]))

/// Remove an element from a vector. This may change the order of elements in the vector.
#define vector_remove_element_unordered(vector, element)                               \
  do {                                                                                 \
    size_t _index = 0;                                                                 \
    for (; _index < (vector).size; _index++) {                                         \
      if (memcmp((vector).data + _index, &(element), sizeof(element)) == 0) { break; } \
    }                                                                                  \
    if (_index < (vector).size) vector_remove_unordered(vector, _index);               \
  } while (0)

/// Remove all elements from a vector that are in another vector.
#define vector_remove_elements_unordered(vector, elements)            \
  do {                                                                \
    for (size_t _i = 0; _i < (elements).size; _i++) {                 \
      vector_remove_element_unordered((vector), (elements).data[_i]); \
    }                                                                 \
    vector_clear(elements);                                           \
  } while (0)

/// Remove all elements from a vector.
#define vector_clear(vector) ((void) ((vector).size = 0))

/// Get the last element of a vector.
#define vector_back_or(vector, default) ((vector).size ? vector_back(vector) : (default))
#define vector_back(vector) (*({                                       \
    ASSERT((vector).size > 0, "vector_back() called on empty vector"); \
    &((vector).data[(vector).size - 1]);                               \
}))

/// Get the first element of a vector.
#define vector_front_or(vector, default) ((vector).size ? vector_front(vector) : (default))
#define vector_front(vector) (*({                                       \
    ASSERT((vector).size > 0, "vector_front() called on empty vector"); \
    &((vector).data[0]);                                                \
}))

/// Insert an element into a vector at before the given index.
#define vector_insert(vector, pos, element)                     \
  do {                                                          \
    if ((pos) >= (vector).data + (vector).size) {               \
      vector_push(vector, element);                             \
    } else {                                                    \
      isz _diff = (pos) - (vector).data;                        \
      ASSERT(_diff >= 0);                                       \
      usz _index = (usz)_diff;                                  \
      vector_reserve((vector), 1);                              \
      memmove((vector).data + _index + 1,                       \
              (vector).data + _index,                           \
              ((vector).size - _index) * sizeof *(vector).data  \
              );                                                \
      (vector).data[_index] = element;                          \
      (vector).size++;                                          \
    }                                                           \
  } while (0)

#define vector_insert_after(vector, element, after) vector_insert(vector, (after + 1), element)

/// Remove an element from a vector by index.
#define vector_remove_index(vector, index)                              \
  do {                                                                  \
    if (index < (vector).size) {                                        \
      memmove((vector).data + (index), (vector).data + (index) + 1, ((vector).size - (index) -1) * sizeof *(vector).data); \
      (vector).size--;                                                  \
    }                                                                   \
  } while (0)

/// Remove an element from the vector.
#define vector_remove_element(vector, element)          \
  do {                                                  \
    size_t _index = 0;                                  \
    for (; _index < (vector).size; _index++) {          \
      if ((vector).data[_index] == element) { break; }  \
    }                                                   \
    vector_remove_index(vector, _index);                \
  } while (0)

/// Append a vector to another vector
#define vector_append(to, from)                                         \
  do {                                                                  \
    vector_reserve((to), (from).size);                                  \
    memcpy((to).data + (to).size, (from).data, (from).size * sizeof *(from).data); \
    (to).size += (from).size;                                           \
  } while (0)

/// Check if a vector contains an element.
#define vector_contains(vector, element) ({              \
    bool _found = false;                                 \
    foreach (_el, (vector)) {                            \
        if (memcmp(_el, &(element), sizeof *_el) == 0) { \
            _found = true;                               \
            break;                                       \
        }                                                \
    }                                                    \
    _found;                                              \
})

/// Find an element in a vector by predicate. Returns
/// a pointer to the element or NULL if not found.
#define vector_find_if(element, vector, ...) ({ \
    __typeof__(*(vector).data) *_ptr = NULL;    \
    foreach (element, (vector)) {               \
        if (__VA_ARGS__) {                      \
            _ptr = element;                     \
            break;                              \
        }                                       \
    }                                           \
    _ptr;                                       \
})

#define list_node(type) \
  struct {              \
    type *prev;         \
    type *next;         \
  }

#define List(type) \
  struct {         \
    type *first;   \
    type *last;    \
  }

#define list_delete(list)                                 \
    do {                                                  \
        __typeof__(*(list).first) *node = (list).first;   \
        while (node) {                                    \
            __typeof__(*(list).first) *next = node->next; \
            free(node);                                   \
            node = next;                                  \
        }                                                 \
    } while (0)

#define list_push_back(list, element) \
  do {                                \
    if (!(list).first) {              \
      (list).first = (element);       \
    } else {                          \
      (list).last->next = (element);  \
      (element)->prev = (list).last;  \
    }                                 \
    (list).last = (element);          \
  } while (0)

#define list_push_front(list, element) \
  do {                                 \
    if (!(list).last) {                \
      (list).last = (element);         \
    } else {                           \
      (list).first->prev = (element);  \
      (element)->next = (list).first;  \
    }                                  \
    (list).first = (element);          \
  } while (0)

#define list_remove(list, element)             \
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

#define list_pop_back(list)           \
  do {                                \
    if ((list).last) {                \
      list_remove(list, (list).last); \
    }                                 \
  } while (0)

#define list_pop_front(list)           \
  do {                                 \
    if ((list).first) {                \
      list_remove(list, (list).first); \
    }                                  \
  } while (0)

#define list_insert_before(list, element, before) \
  do {                                            \
    (element)->prev = (before)->prev;             \
    (element)->next = (before);                   \
    if ((before)->prev) {                         \
      (before)->prev->next = (element);           \
    } else {                                      \
      (list).first = (element);                   \
    }                                             \
    (before)->prev = (element);                   \
  } while (0)

#define list_insert_after(list, element, after) \
  do {                                          \
    (element)->prev = (after);                  \
    (element)->next = (after)->next;            \
    if ((after)->next) {                        \
      (after)->next->prev = (element);          \
    } else {                                    \
      (list).last = (element);                  \
    }                                           \
    (after)->next = (element);                  \
  } while (0)

#define list_size(type, size_out, list)                   \
  do {                                                    \
    size_t _size = 0;                                     \
    for (type _it = (list).first; _it; _it = _it->next) { \
      _size++;                                            \
    }                                                     \
    size_out = _size;                                     \
  } while (0)

#define list_foreach(type, it, list) \
  for (type it = (list).first; it; it = it->next)

#define list_foreach_rev(type, it, list) \
  for (type it = (list).last; it; it = it->prev)

#endif // VECTOR_H
