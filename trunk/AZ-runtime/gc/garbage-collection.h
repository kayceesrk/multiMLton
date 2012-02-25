/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void minorGC (GC_state s);
static void majorGC (GC_state s, size_t bytesRequested, bool mayResize, bool liftWBAs);
static inline void growStackCurrent (GC_state s, bool allocInOldGen, size_t reservedNew);
static inline void enterGC (GC_state s);
static inline void leaveGC (GC_state s);
static void performGC (GC_state s,
                       size_t oldGenBytesRequested,
                       size_t nurseryBytesRequested,
                       bool forceMajor,
                       bool mayResize,
                       size_t forceStackGrowthBytes);
static void performSharedGC (GC_state s, size_t bytesRequested);
static size_t fillGap (GC_state s, pointer start, pointer end);
static void ensureHasHeapBytesFreeAndOrInvariantForMutator (GC_state s,
                                                            bool forceGC,
                                                            bool ensureFrontier,
                                                            bool ensureStack,
                                                            size_t oldGenBytesRequested,
                                                            size_t nurseryBytesRequested,
                                                            bool fromGCCollect,
                                                            size_t forceStackGrowthBytes);
static bool allocChunkInSharedHeap (GC_state s,
                                    size_t nurseryBytesRequested);
static void fixForwardingPointers (GC_state s, bool mayResize);


#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_collect (GC_state s, size_t bytesRequested, bool force,
                         char *file, int line);
/* XX spoons should probably go somewhere else... or just get removed */
pointer FFI_getOpArgsResPtr (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
