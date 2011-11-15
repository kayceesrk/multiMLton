/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct __CopyObjectMap {
  pointer oldP;
  pointer newP;
  UT_hash_handle hh;
} CopyObjectMap;

typedef struct __SkipRange SkipRange;

struct __SkipRange {
  pointer start;
  pointer end;
  SkipRange* next;
};

struct GC_forwardState {
  bool amInMinorGC;
  pointer back;
  pointer toStart;
  pointer toLimit;
  /* Used when moving objects to shared heap to represent the parts
   * of the sharedHeap allocated to other mutators.
   */
  SkipRange* rangeListFirst;
  SkipRange* rangeListCurrent;
  SkipRange* rangeListLast;

  /* Stacks are usually not moved to the shared heap and stack pointers are
   * added to danglingLists (remembered sets) for local collection. But at
   * certain instances, stack lifting is infact required (preThread in
   * basis-library/mlton/thread). The following boolean forces stack lifting
   */
  bool forceStackForwarding;

  /* This indicates the object being lifted if forwardState is being used in an
   * transitive closure lift operation. If the shared heap GC finishes the
   * lifting, the thread performing the lifting process, aborts lifting.
   */
  objptr liftingObject;

  /* When aborting transitive closure lifting, the following location is used
   * for a non-local goto operation. Similar to throwing an exception.
   */
  jmp_buf returnLocation;
  bool isReturnLocationSet;

  /* used to indicate if a GC would be required to fix the heap */
};

#define GC_FORWARDED ~((GC_header)0)

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isPointerInToSpace (GC_state s, pointer p);
static inline bool isObjptrInToSpace (GC_state s, objptr op);

static inline void forwardObjptr (GC_state s, objptr *opp);
static void forwardObjptrToSharedHeap (GC_state s, objptr *opp);
static void copyObjptr (GC_state s, objptr *opp);
static inline void forwardObjptrIfInNursery (GC_state s, objptr *opp);
static inline void forwardObjptrIfInLocalHeap (GC_state s, objptr *opp);
static inline void forwardObjptrForSharedCheneyCopy (GC_state s, objptr *opp);
static inline void forwardObjptrForSharedMarkCompact (GC_state s, objptr *opp);
static inline void forwardInterGenerationalObjptrs (GC_state s);

static inline void fixFwdObjptr (GC_state s, objptr *opp);
static inline objptr fixFwdObjptrAndFetch (GC_state s, objptr *opp);

static inline void saveForwardState (GC_state s, struct GC_forwardState* fwd);
static inline void restoreForwardState (GC_state s, struct GC_forwardState* fwd);
static void clearRangeList (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
