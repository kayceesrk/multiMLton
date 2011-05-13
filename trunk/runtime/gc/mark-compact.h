/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void copyForThreadInternal (pointer dst, pointer src);
static inline void threadInternalObjptr (GC_state s, objptr *opp);
static inline void threadInternalObjptrIfInLocalHeap (GC_state s, objptr *opp);
static inline void threadInternalObjptrIfInSharedHeap (GC_state s, objptr *opp);
static inline void updateWeaksForMarkCompact (GC_state s);
static void updateForwardPointersForMarkCompact (GC_state s, GC_heap h, pointer front, pointer back, GC_stack* currentStack, bool sharedCollection);
static void updateBackwardPointersAndSlideForMarkCompact (GC_state s, GC_heap h, GC_stack* currentStack, bool sharedCollection);
static void majorMarkCompactGC (GC_state s);
static void majorMarkCompactSharedGC (GC_state s);
static inline void headerCheck (GC_state s, pointer front, pointer back);
static inline void headerCheckInternalObjptr (GC_state s, objptr* opp);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
