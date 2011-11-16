/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_pointerSet {
  void* p;
  UT_hash_handle hh;
};

typedef enum {
  ZERO=0,
  ONE=1,
  MANY
} GC_numReferences;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void isObjectPointerVirginMark (GC_state s, pointer current, pointer parent);
static inline void isObjectPointerVirginUnmark (GC_state s, pointer current, pointer parent);
static inline void doesPointToMarkedObject (GC_state s, objptr* opp);
PRIVATE bool GC_isObjectClosureClean (GC_state s, pointer p);
PRIVATE bool GC_isThreadClosureClean (GC_state s, pointer p);
bool __GC_isThreadClosureClean (GC_state s, pointer p, size_t* size);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
