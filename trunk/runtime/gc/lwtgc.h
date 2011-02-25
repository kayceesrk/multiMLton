/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE pointer GC_move (GC_state s, pointer object,
                         bool forceStackForwarding,
                         bool skipFixForwardingPointers);
PRIVATE void GC_addToMoveOnWBA (GC_state s, pointer p);

//Kind is 0 -- HOST or 1 -- PARASITE
PRIVATE void GC_addToPreemptOnWBA (GC_state s, pointer p, int kind);
PRIVATE bool GC_isInSharedOrForwarded (GC_state s, pointer p);
PRIVATE void GC_addToSpawnOnWBA (GC_state s, pointer p, int proc);

static inline void liftObjptr (GC_state s, objptr *opp);
void forceLocalGC (GC_state s);
void moveTransitiveClosure (GC_state s, objptr *opp,
                            bool forceStackForwarding,
                            bool fillOrig);
void moveEachObjptrInObject (GC_state s, pointer object);
void liftAllObjectsDuringInit (GC_state s);
void liftAllObjptrsInMoveOnWBA (GC_state s);
static inline void assertLiftedObjptr (GC_state s, objptr *opp);
void jumpToReturnLocation (GC_state s) __attribute__((noreturn));


#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */


#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void foreachObjptrInWBAs (GC_state s, GC_state fromState,
                                        GC_foreachObjptrFun f);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
