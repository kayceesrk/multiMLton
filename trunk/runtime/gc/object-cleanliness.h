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
  LOCAL_MANY=2,
  GLOBAL_MANY=3
} GC_numReferences;

#define ONE_REF (ONE << VIRGIN_SHIFT)
#define LOCAL_MANY_REF (LOCAL_MANY << VIRGIN_SHIFT)
#define GLOBAL_MANY_REF (GLOBAL_MANY << VIRGIN_SHIFT)

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_numReferences getNumReferences (const GC_header header);
void setNumReferences (GC_header* headerp, GC_numReferences count);
static inline bool isWriteCleanMark (GC_state s, pointer current, pointer parent);
static inline bool isWriteCleanUnmark (GC_state s, pointer current, pointer parent);
static inline bool isSpawnCleanMark (GC_state s, pointer current, pointer parent);
static inline bool isSpawnCleanUnmark (GC_state s, pointer current, pointer parent);
static inline void doesPointToMarkedObject (GC_state s, objptr* opp);
static inline void doesCurrentStackPointToMarkedObject (GC_state s, objptr* opp);
PRIVATE bool GC_isObjectClosureClean (GC_state s, pointer p);
PRIVATE bool GC_isThreadClosureClean (GC_state s, pointer p);
bool __GC_isThreadClosureClean (GC_state s, pointer p, size_t* size);
bool foreachObjptrInUnmarkedObject (GC_state s, pointer p);
const char* numReferencesToString (GC_numReferences n);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
