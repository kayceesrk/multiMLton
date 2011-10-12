/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void addToObjectSharingInfoIfObjptrInSharedHeap (GC_state s, objptr* opp);
void addToObjectSharingInfoWalkingShared (GC_state s, objptr* opp);
static void reclaimObjects (GC_state s);
void reclaim (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
