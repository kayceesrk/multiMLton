/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void addToObjectSharingInfoWalkingShared (GC_state s, objptr* opp);
static void reclaimObjects (GC_state s, GC_objectSharingInfo map);
void reclaim (GC_state s);


static void addToReachableArray (GC_state s, pointer p);
static void dfsMarkReachable (GC_state s, objptr* opp);
static void dfsUnmarkReachable (GC_state s, objptr* opp);
static GC_objectSharingInfo addToHashTable (GC_state s, GC_objectSharingInfo map, pointer p, int coreId);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
