/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void reclaimObjects (GC_state s, GC_objectSharingInfo globalHashTable) {
  GC_objectSharingInfo globalE = NULL;
  size_t totalSize = 0;
  assert (globalHashTable);
  for (globalE = globalHashTable; globalE != NULL; globalE = globalE->hh.next) {
    if ((uint32_t)globalE->coreId == s->procId) {
      pointer p = (pointer)globalE->objectLocation;
      totalSize += sizeofObject (s, p);
    }
  }

  if (DEBUG_RECLAIM || TRUE)
    fprintf (stderr, "totalSize = %s [%d]\n", uintmaxToCommaString (totalSize), s->procId);
  if (totalSize) {
    if (s->forwardState.liftingObject == BOGUS_OBJPTR) {
      //Ensure space in the localheap old gen
      ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE, FALSE, FALSE, totalSize, 0, FALSE, 0);

      //Move to oldGen
      //--------------

      //setup forwarding state
      s->forwardState.toStart = s->heap->start + s->heap->oldGenSize;
      s->forwardState.toLimit = s->heap->start + s->heap->oldGenSize + totalSize;
      s->forwardState.back = s->forwardState.toStart;
      s->forwardState.forceStackForwarding = TRUE;

      for (globalE = globalHashTable; globalE != NULL; globalE = globalE->hh.next) {
        if ((uint32_t)globalE->coreId == s->procId) {
          pointer p = (pointer)globalE->objectLocation;
          GC_header* hp = getHeaderp (p);
          GC_header h = getHeader (p);
          *hp = h & ~LIFT_MASK;
          objptr op = pointerToObjptr (p, s->sharedHeap->start);
          if (DEBUG_RECLAIM)
            fprintf (stderr, "RECLAIMING: p="FMTPTR" ", (uintptr_t)p);
          forwardObjptr (s, &op);
          if (DEBUG_RECLAIM)
            fprintf (stderr, "newP="FMTPTR"\n", (uintptr_t)op);
        }
      }
      assert (totalSize == (size_t)(s->forwardState.back - s->forwardState.toStart));
      s->heap->oldGenSize += (s->forwardState.back - s->forwardState.toStart);

      //Fix the forwarding pointers
      foreachGlobalObjptrInScope (s, fixFwdObjptr);
      refreshDanglingStackList (s);
      if (s->heap->start + s->heap->oldGenSize == s->heap->nursery) {
        foreachObjptrInRange (s, s->heap->start, &s->frontier, fixFwdObjptr, FALSE);
      }
      else {
        pointer end = s->heap->start + s->heap->oldGenSize;
        foreachObjptrInRange (s, s->heap->start, &end, fixFwdObjptr, FALSE);
        foreachObjptrInRange (s, s->heap->nursery, &s->frontier, fixFwdObjptr, FALSE);
      }

      //Fill reclaimedObjects
      for (globalE = globalHashTable; globalE != NULL; globalE = globalE->hh.next) {
        if ((uint32_t)globalE->coreId == s->procId) {
          pointer p = (pointer)globalE->objectLocation;
          pointer front = getBeginningOfObject (s, p);
          assert (front != BOGUS_POINTER);
          fillGap (s, front, front + sizeofObject (s, p));
        }
      }
    }
  }
}

void addToReachableArray (GC_state s, pointer p) {
  if (isPointerInHeap (s, s->sharedHeap, p)) {
    utarray_push_back (s->reachable, &p);
  }
}

void dfsMarkReachable (GC_state s, objptr* opp) {
  pointer p;
  fixFwdObjptr (s, opp);
  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, addToReachableArray, MARK_MODE, FALSE, TRUE, FALSE, FALSE);
}

void dfsUnmarkReachable (GC_state s, objptr* opp) {
  pointer p;
  fixFwdObjptr (s, opp);
  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, emptyForeachObjectFun, UNMARK_MODE, FALSE, TRUE, FALSE, FALSE);
}

GC_objectSharingInfo addToHashTable (GC_state s, GC_objectSharingInfo map, pointer p, int coreId) {
  GC_objectSharingInfo found = NULL;
  HASH_FIND_PTR (map, &p, found);
  //If element present in globalHashTable and shared heap object is not exclusive to a single core
  if (found && found->coreId != coreId) {
    if (DEBUG_RECLAIM)
      fprintf (stderr, "HASH_MODIFY: "FMTPTR" coreId=%d [%d]\n", (uintptr_t)p, -1, s->procId);
    found->coreId = -1;
  }
  //If the element is not in globalHashTable
  else if (not found) {
    found = (GC_objectSharingInfo) malloc (sizeof (struct GC_objectSharingInfo));
    found->objectLocation = p;
    found->coreId = coreId;
    if (DEBUG_RECLAIM)
      fprintf (stderr, "HASH_ADD: "FMTPTR" coreId=%d [%d]\n", (uintptr_t)p, coreId, s->procId);
    HASH_ADD_PTR (map, objectLocation, found);
  }
  return map;
}

void reclaim (GC_state s) {
  s->syncReason = SYNC_MISC;
  ENTER0 (s);

  UT_icd icd = {sizeof(pointer), NULL, NULL, NULL};

  if (Proc_processorNumber (s) == 0) {
    //Globals
    UT_array* globalReachable = NULL;
    utarray_new (s->reachable, &icd);

    for (unsigned int i = 0; i < s->globalsLength; ++i)
      dfsMarkReachable (s, &s->globals[i]);
    for (unsigned int i = 0; i < s->globalsLength; ++i)
      dfsUnmarkReachable (s, &s->globals[i]);
    globalReachable = s->reachable;

    //For local heaps
    for (int proc=0; proc < s->numberOfProcs; proc++) {
      GC_state r = &s->procStates[proc];
      r->reachable = NULL;
      utarray_new (r->reachable, &icd);
      ENTER_LOCAL0 (r);
      foreachGlobalObjptrInScope (r, dfsMarkReachable);
      foreachGlobalObjptrInScope (r, dfsUnmarkReachable);
      LEAVE_LOCAL0 (r);
    }

    GC_objectSharingInfo globalMap = NULL;
    //Globals
    while (utarray_len (globalReachable) != 0) {
      pointer p = *(pointer*)utarray_back (globalReachable);
      utarray_pop_back (globalReachable);
      globalMap = addToHashTable (s, globalMap, p, -1);
    }
    utarray_free (globalReachable);

    //For local heaps
    for (int proc=0; proc < s->numberOfProcs; proc++) {
      GC_state r = &s->procStates[proc];
      if (DEBUG_RECLAIM)
        fprintf (stderr, "Processing array of size %d [%d]\n", utarray_len (r->reachable), r->procId);
      while (utarray_len (r->reachable) != 0) {
        pointer p = *(pointer*)utarray_back (r->reachable);
        utarray_pop_back (r->reachable);
        globalMap = addToHashTable (r, globalMap, p, r->procId);
      }
      utarray_free (r->reachable);
      r->reachable = NULL;
    }

    size_t totalObjects = HASH_COUNT (globalMap);
    size_t totalExclusive = 0;
    for (GC_objectSharingInfo globalE = globalMap; globalE != NULL; globalE = globalE->hh.next) {
      if (globalE->coreId != -1)
        totalExclusive++;
    }

    if (DEBUG_RECLAIM || TRUE)
      fprintf (stderr, "Exclusive objects are %.1f%% of total shared heap objects [%zu]\n",
              (100.0 * ((double)totalExclusive/(double)totalObjects)), totalObjects);

    //Reclaim
    for (int proc=0; proc < s->numberOfProcs; proc++)
      reclaimObjects (&s->procStates[proc], globalMap);

    //FREE
    {
      GC_objectSharingInfo e, tmp;
      HASH_ITER (hh, globalMap, e, tmp) {
        HASH_DEL (globalMap, e);
        free (e);
      }
    }
  }

  LEAVE0 (s);
}
