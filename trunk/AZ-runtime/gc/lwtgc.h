/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE pointer GC_move (GC_state s, pointer object,
                         bool forceStackForwarding,
                         bool skipFixForwardingPointers);
PRIVATE pointer GC_moveFromWB (GC_state s, pointer object,
                               bool forceStackForwarding,
                               bool skipFixForwardingPointers);
PRIVATE pointer GC_moveWithCopyType (GC_state s, pointer object,
                                     bool forceStackForwarding,
                                     bool skipFixForwardingPointers,
                                     bool copyImmutable);
PRIVATE bool GC_isInSharedOrForwarded (GC_state s, pointer p);

static inline void liftObjptr (GC_state s, objptr *opp);
void forceLocalGC (GC_state s);
void moveTransitiveClosure (GC_state s, objptr *opp,
                            bool forceStackForwarding,
                            bool fillOrig);
void moveEachObjptrInObject (GC_state s, pointer object);
void liftAllObjectsDuringInit (GC_state s);
static inline void assertLiftedObjptr (GC_state s, objptr *opp);
void jumpToReturnLocation (GC_state s) __attribute__((noreturn));
pointer GC_copyToBuffer (GC_state s, pointer p, size_t size);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
