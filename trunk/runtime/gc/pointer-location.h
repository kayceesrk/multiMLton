/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


/* Since we are dealing with processes in different OSes on the SCC, we might
 * encounter situations where the virtual addresses of local heaps on different
 * cores might overlap. This will not be a problem during the execution of ML
 * code, since our GC design forbids pointers to other cores. So it is safe to
 * dereference any pointer without having to worry about the pointer pointing
 * another local heap.
 *
 * But during shared heap collection, we do face situations where, given a
 * pointer, we will need to find out which heap it belongs to. Due to the above
 * mentioned problem of overlapping virtual address spaces, using
 * isPointerToHeap, which relies on range checks, will not work on the SCC.
 * Hence, before we enter shared collection, we create a PointerToCoreMap of
 * all pointers p, pointed to by opp (objptr*), where opp is in the shared heap
 * and p is in some local heap. We associate the coreId with p. This will help
 * to unambiguously determine to which core (and thereby local heap), p belongs
 * to. */
#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct {
  pointer p;
  uint32_t core;
  bool isDanglingStack;
} PointerToCore;

typedef struct {
  size_t size;
  size_t maxSize;
  PointerToCore elem[];
} PointerToCoreMap;


#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void initPointerToCore (GC_state s, size_t size);
static void finalizePointerToCore (GC_state s);
static void insertPointerToCore (GC_state s, pointer p, uint32_t core, bool isDanglingStack);
static uint32_t getCoreIdFromPointer (GC_state s, pointer p);
static int32_t getCoreIdFromPointer_safe (GC_state s, pointer p);
static bool isPointerInAnotherCore (GC_state s, objptr* opp, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

