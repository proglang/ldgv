//
// LDST_gc.h
//

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "LDST.h"

#ifndef GC_INIT_SIZE
#define GC_INIT_SIZE 64
#endif

#ifdef GC_DEBUG
#define GC_LOG(fmt, ...) fprintf(stderr, "gc: " fmt "\n", ##__VA_ARGS__)
#else
#define GC_LOG(fmt, ...)
#endif

typedef struct object {
  /// The lowest bit is set if the object is marked, the size is always a
  /// multiple of two.
  size_t sizeAndMarked;

  /// The user requested space.
  _Alignas(_Alignof(max_align_t))
    char contents[];
} object_t;

/// Calculates the pointer to the object header from the contents pointer.
static object_t *get_header(void *pointer) {
  intptr_t ptrval = (intptr_t)pointer;
  ptrval -= offsetof(object_t, contents);
  return (object_t *)ptrval;
}

typedef struct ptr_set {
  size_t size;
  size_t capacity;
  void **pointers;
  bool unordered;
} ptr_set_t;

/// The set of all allocated pointers.
///
/// These are the pointers returned to the user, not the pointers to the object
/// header.
static ptr_set_t Allocated = { 0 };

/// Compares two pointers.
///
/// This function is used as an argument to `qsort` and `bsearch` to sort the
/// stored pointers in a `ptr_set` and has therefore the hidden level of
/// indirection not visible in the function signature.
static int ptr_cmp(const void *a_void, const void *b_void) {
  const void *a = *((const void *const *)a_void);
  const void *b = *((const void *const *)b_void);
  if (a < b) return -1;
  if (a > b) return +1;
  return 0;
}

/// Inserts a pointer into a pointer set.
static bool pointers_add(ptr_set_t *set, void *p) {
  if (set->size >= set->capacity) {
    size_t new_size = set->size == 0 ? GC_INIT_SIZE : set->size * 2;
    void **resized = realloc(set->pointers, new_size * sizeof(void*));
    if (resized == 0)
      return false;

    set->capacity = new_size;
    set->pointers = resized;
  }

  set->pointers[set->size++] = p;
  set->unordered = true;
  return true;
}

/// Sorts a pointer set if it isn't already sorted.
static void pointers_sort(ptr_set_t *set) {
  if (!set->unordered)
    return;

  qsort(set->pointers, set->size, sizeof(void*), ptr_cmp);
  set->unordered = false;
}

/// Searches for a pointer in a pointer set and returns a pointer to the
/// pointers header if exists.
static object_t *pointers_search(ptr_set_t *set, void *p) {
  pointers_sort(set);
  return bsearch(&p, set->pointers, set->size, sizeof(void*), ptr_cmp) ? get_header(p) : 0;
}

/// Checks that the size is small enough that there is enough space left to
/// allocate the object header and rounds it up to the next even number.
static bool adjust_size(size_t *size) {
  size_t sz = *size;
  bool ok =
    sz > 0                // No need to allocate anything for size=0
      && sz != SIZE_MAX   // There is no space left for the object header
      && (sz += sz % 2) <= SIZE_MAX - sizeof(object_t);
                          // Check space left after rounding up to next even number.
  if (ok) *size = sz;
  return ok;
}

void *ldst__gc_alloc(size_t size) {
  GC_LOG("alloc %zu", size);
  object_t *obj = 0;

  if (!adjust_size(&size))
    goto fail;

  if (!(obj = malloc(size + sizeof(object_t))))
    goto fail;

  // Initialize the object. Since we will traverse the the contents for known
  // pointers, we initialize it zeroes.
  obj->sizeAndMarked = size;
  void *p = &obj->contents;
  memset(p, 0, size);

  if (!pointers_add(&Allocated, p))
    goto fail;

  GC_LOG("   %p | %p", (void*)obj, (void*)p);
  return p;

fail:
  GC_LOG("   alloc failed");
  free(obj);
  return 0;
}

static size_t mark(object_t *obj) {
  size_t size = obj->sizeAndMarked;
  obj->sizeAndMarked |= 1;
  return size & 1 ? 0 : size;
}

static void mark_objects(object_t *root) {
  size_t content_size = mark(root);
  size_t pointer_size = sizeof(object_t *);
  GC_LOG("  mark %p", (void*)root);

  for (size_t off = 0; off + pointer_size <= content_size; off += pointer_size) {
    // Use memcpy to work around C's strict aliasing rule.
    void *p;
    memcpy(&p, root->contents + off, pointer_size);

    // Check if it is a known pointer.
    object_t *object;
    if ((object = pointers_search(&Allocated, p))) {
      GC_LOG("  -> %p | %p", (void*)object, (void*)p);
      mark_objects(object);
      GC_LOG("  <-");
    } else {
      GC_LOG("  x  %p", (void*)p);
    }
  }
}

static void sweep_list(ptr_set_t *set) {
  GC_LOG("sweeping");

  size_t offset = 0;
  size_t size = set->size;
  void **ptrs = set->pointers;

  for (size_t i = 0; i < size; ++i) {
    void *p = ptrs[i];
    object_t *o = get_header(p);

    if (o->sizeAndMarked & 1) {
      GC_LOG("  %p | %p *", (void*)o, p);
      o->sizeAndMarked &= ~(size_t)1;
      ptrs[i - offset] = p;
    } else {
      GC_LOG("  %p | %p", (void*)o, p);
      free(o);
      offset++;
    }
  }

  set->size -= offset;
  GC_LOG("  (%zu sweeped)", offset);
}

void ldst__gc(gc_root_visitor_t visitor) {
  GC_LOG("begin gc");

#ifdef GC_DEBUG
  pointers_sort(&Allocated);
  for (size_t i = 0; i < Allocated.size; ++i) {
    void *p = Allocated.pointers[i];
    object_t *o = get_header(p);
    GC_LOG("  %p | %p  (%zu)", (void*)o, p, o->sizeAndMarked & ~1);
  }
#endif   // GC_DEBUG

  void *root = 0;
  void *visitor_context = 0;

  while (visitor && (root = visitor(&visitor_context))) {
    object_t *root_object = get_header(root);
    GC_LOG("root: %p | %p", (void*)root_object, (void*)root);
    assert(pointers_search(&Allocated, root) && "root object not allocated via GC");
    mark_objects(root_object);
  }

  sweep_list(&Allocated);

  GC_LOG("end gc");
}
