/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * All ML objects (including ML execution stacks) are allocated in a
 * contiguous heap.  The heap has the following general layout:
 *
 *  -------------------------------------------------------------------------
 *  |    old generation    |               |  nursery  | cardMap | crossMap |
 *  -------------------------------------------------------------------------
 *  |------oldGenSize------|
 *  |-----------------------size-----------------------|
 *  ^                                      ^
 *  start                                  nursery
 *  |------------------------------withMapsSize-----------------------------|
*/

typedef enum {
    LOCAL_HEAP,
    SHARED_HEAP
} GC_heapKind;

typedef struct GC_heap {
  GC_heapKind kind;
  pointer nursery; /* start of nursery */
  size_t oldGenSize; /* size of old generation */
  size_t size; /* size of heap */
  pointer start; /* start of heap (and old generation) */
  size_t withMapsSize; /* size of heap with card/cross maps */

  //The following structures are only used in shared heaps
  size_t availableSize; /* may be smaller than size if we are limiting
                           allocation for profiling purposes */
  pointer frontier; /* next (globally) unallocated space */
} *GC_heap;


typedef struct GC_objectSharingInfo {
  //Location of the object in the shared heap
  void* objectLocation;
  //If the object is exclusively pointed from a single local heap, then this is
  //the core id of the local heap. Otherwise, this is -1.
  int32_t coreId;
  UT_hash_handle hh;
} *GC_objectSharingInfo;

#define GC_HEAP_LIMIT_SLOP 512

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isPointerInHeap (GC_state s, GC_heap h, pointer p);
static inline bool isPointerInAnyLocalHeap (GC_state s, pointer p);
static inline bool isPointerInOldGen (GC_state s, GC_heap h, pointer p);
static inline bool isPointerInNursery (GC_state s, GC_heap h, pointer p);
static inline bool isPointerInFromSpace (GC_state s, GC_heap h, pointer p);
static inline bool isObjptrInHeap (GC_state s, GC_heap h, objptr op);
static inline bool isObjptrInOldGen (GC_state s, GC_heap h, objptr op);
static inline bool isObjptrInNursery (GC_state s, GC_heap h, objptr op);
static inline bool isObjptrInFromSpace (GC_state s, GC_heap h, objptr op);
static inline bool hasHeapBytesFree (GC_state s, GC_heap h, size_t oldGen, size_t nursery);
static inline bool isHeapInit (GC_heap h);

static void displayHeap (GC_state s, GC_heap heap, FILE *stream);

static inline void initHeap (GC_state s, GC_heap h, GC_heapKind);
static inline size_t sizeofHeapDesired (GC_state s, size_t live, size_t currentSize, GC_heapKind kind);

static inline void releaseHeap (GC_state s, GC_heap h);
static void shrinkHeap (GC_state s, GC_heap h, size_t keepSize);
static bool createHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize);
static bool createHeapSecondary (GC_state s, size_t desiredSize);
static bool createSharedHeapSecondary (GC_state s, size_t desiredSize);
static bool remapHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize);
static void growHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize);
static void resizeHeap (GC_state s, GC_heap h, size_t minSize);
static void resizeLocalHeapSecondary (GC_state s);
static void resizeSharedHeapSecondary (GC_state s, size_t primarySize);

static inline bool isObjectLifted (GC_header header);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
