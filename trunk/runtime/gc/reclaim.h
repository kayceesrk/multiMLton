/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

UT_icd icd = {sizeof(pointer), NULL, NULL, NULL};

void addToObjectSharingInfoWalkingShared (GC_state s, objptr* opp);
void reclaimObjects (GC_state s);
void computeExclusivityInformation (GC_state s);


static bool addToReachableArray (GC_state s, pointer current, pointer parent);
static void dfsMarkReachable (GC_state s, objptr* opp);
static void dfsUnmarkReachable (GC_state s, objptr* opp);
static GC_objectSharingInfo addToHashTable (GC_state s, GC_objectSharingInfo map, pointer p, int coreId);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
