/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void reclaimObjects (GC_state s) {
  assert (s->reachable);

  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s);

  size_t totalSize = 0;
  size_t totalObjects = utarray_len (s->reachable);

  if (DEBUG_RECLAIM)
    fprintf (stderr, "totalObjects = %zu [%d]\n", totalObjects, s->procId);
  pointer* iter = NULL;

  for (iter = (pointer*)utarray_front (s->reachable);
        iter != NULL;
        iter = (pointer*)utarray_next (s->reachable, iter)) {
    pointer p = *iter;
    totalSize += sizeofObject (s, p);
  }

  if (DEBUG_RECLAIM)
    fprintf (stderr, "totalSize = %s [%d]\n", uintmaxToCommaString (totalSize), s->procId);
  if (totalSize) {
    //Move to oldGen
    //--------------
    //setup forwarding state
    s->forwardState.toStart = s->heap->start + s->heap->oldGenSize;

    //XXX see gc_state.c:setGCStateCurrentLocalHeap
    s->forwardState.toLimit = s->heap->start + s->heap->size - GC_HEAP_LIMIT_SLOP;

    s->forwardState.back = s->forwardState.toStart;
    s->forwardState.forceStackForwarding = FALSE;

    //Set nursery to toLimit. Nursery is broken at this point anyway. This is
    //needed to prevent infinitely triggering recursive forwarding.
    s->heap->nursery = s->forwardState.toLimit;

    size_t numReclaimed = 0;
    for (iter = (pointer*)utarray_front (s->reachable);
         iter != NULL;
         iter = (pointer*)utarray_next (s->reachable, iter)) {
      pointer p = *iter;
      size_t size = sizeofObject (s, p);

      if ((size_t) (s->forwardState.toLimit - s->forwardState.back) <= size)
        break;

      numReclaimed++;
      objptr op = pointerToObjptr (p, s->sharedHeap->start);
      GC_header* hp = getHeaderp (p);
      GC_header h = getHeader (p);
      *hp = h & ~(LIFT_MASK);
      forwardObjptr (s, &op);

      if (DEBUG_RECLAIM)
        fprintf (stderr, "RECLAIMING: p="FMTPTR" newP="FMTPTR" [%d]\n", (uintptr_t)p, (uintptr_t)op, s->procId);
    }
    s->heap->oldGenSize += (s->forwardState.back - s->forwardState.toStart);

    //Fix the forwarding pointers
    foreachGlobalObjptrInScope (s, fixFwdObjptr);
    refreshDanglingStackList (s);
    pointer end = s->heap->start + s->heap->oldGenSize;
    foreachObjptrInRange (s, s->heap->start, &end, fixFwdObjptr, FALSE);

    //Fill reclaimedObjects
    for (size_t i=0; i < numReclaimed; i++) {
      pointer p = *(pointer*) utarray_eltptr (s->reachable, i);
      pointer front = getBeginningOfObject (s, p);
      fillGap (s, front, front + sizeofObject (s, p));
    }

    if (numReclaimed == totalObjects) {
      utarray_free (s->reachable);
      s->reachable = NULL;
    }
    else
      utarray_erase (s->reachable, 0, numReclaimed);
  }

  ENTER_LOCAL0 (s);
}

bool addToReachableArray (GC_state s, pointer p, __attribute__((unused)) pointer prev) {
  bool toPush = TRUE;
  assert (s->reachable);
  if (isPointerInHeap (s, s->sharedHeap, p)) {
    if (DEBUG_RECLAIM)
      fprintf (stderr, "addToReachableArray "FMTPTR"\n", (uintptr_t)p);

    //If we are adding a thread make sure the stack is not located in another
    //core's local heap.
    if ((getHeader(p) & ~(LIFT_MASK | VIRGIN_MASK)) == (GC_header)0x3) {
      if (DEBUG_RECLAIM)
        fprintf (stderr, "\taddToReachableArray "FMTPTR" ignoring\n", (uintptr_t)p);
      toPush = FALSE;
    }

    if (toPush)
      utarray_push_back (s->reachable, &p);
  }
  return TRUE;
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

void computeExclusivityInformation (GC_state s) {
  s->syncReason = SYNC_MISC;
  ENTER0 (s);

  if (Proc_processorNumber (s) == 0) {
    for (int proc=0; proc < s->numberOfProcs; proc++) {
      GC_state r = &s->procStates[proc];
      r->reachable = NULL;
    }

    //Globals
    UT_array* globalReachable = NULL;
    utarray_new (s->reachable, &icd);

    //Globals -- MARK
    if (DEBUG_RECLAIM)
      fprintf (stderr, "reclaim: MARK GLOBALS\n");
    for (unsigned int i = 0; i < s->globalsLength; ++i)
      dfsMarkReachable (s, &s->globals[i]);
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      GC_state r = &s->procStates[proc];
      if (DEBUG_RECLAIM)
        fprintf (stderr, "reclaim: MARK foreachObjptrInWBAs [%d]\n", r->procId);
      foreachObjptrInExportableWBAs (s, r, dfsMarkReachable);
      if (DEBUG_RECLAIM)
        fprintf (stderr, "reclaim: MARK danglingStackList [%d]\n", r->procId);
      foreachObjptrInDanglingStackList (s, r, dfsMarkReachable);
    }
    //Globals -- UNMARK
    if (DEBUG_RECLAIM)
      fprintf (stderr, "reclaim: UNMARK GLOBALS\n");
    for (unsigned int i = 0; i < s->globalsLength; ++i)
      dfsUnmarkReachable (s, &s->globals[i]);
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      GC_state r = &s->procStates[proc];
      if (DEBUG_RECLAIM)
        fprintf (stderr, "reclaim: UNMARK foreachObjptrInWBAs [%d]\n", r->procId);
      foreachObjptrInExportableWBAs (s, r, dfsUnmarkReachable);
      if (DEBUG_RECLAIM)
        fprintf (stderr, "reclaim: UNMARK danglingStackList [%d]\n", r->procId);
      foreachObjptrInDanglingStackList (s, r, dfsUnmarkReachable);
    }
    globalReachable = s->reachable;

    //For local heaps
    for (int proc=0; proc < s->numberOfProcs; proc++) {
      GC_state r = &s->procStates[proc];
      r->reachable = NULL;
      utarray_new (r->reachable, &icd);
      r->syncReason = SYNC_MISC;
      ENTER_LOCAL0 (r);
      if (DEBUG_RECLAIM)
        fprintf (stderr, "reclaim: MARK locals [%d]\n", r->procId);
      foreachGlobalObjptrInScope (r, dfsMarkReachable);
      if (DEBUG_RECLAIM)
        fprintf (stderr, "reclaim: UNMARK locals [%d]\n", r->procId);
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

    //Add to local reachable lists and free hash table elements
    {
      GC_objectSharingInfo e, tmp;
      HASH_ITER (hh, globalMap, e, tmp) {
        if (e->coreId != -1) {
          GC_state r = &s->procStates[e->coreId];
          if (!(r->reachable))
            utarray_new (r->reachable, &icd);
          pointer p = (pointer)e->objectLocation;
          utarray_push_back (r->reachable, &p);
          if (DEBUG_RECLAIM)
            fprintf (stderr, "PUSH_BACK: p="FMTPTR" coreId=%d\n", (uintptr_t)p, e->coreId);
        }
        HASH_DEL (globalMap, e);
        free (e);
      }
    }
  }

  LEAVE0 (s);
}
