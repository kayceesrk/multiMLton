/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

volatile GC_objectSharingInfo globalHashTable;
volatile int32_t currentCoreId;

void addToObjectSharingInfoIfObjptrInSharedHeap (GC_state s, objptr* opp) {
  if (not isObjptr(*opp))
    return;
  fixFwdObjptr (s, opp);
  pointer object = objptrToPointer (*opp, s->heap->start);
  if (isPointerInHeap (s, s->sharedHeap, object)) {
    GC_objectSharingInfo e;
    HASH_FIND_PTR (s->objectSharingInfo, &object, e);
    if (not e) {
      //We did not find the object in the hash table. Hence, insert it.
      e = (GC_objectSharingInfo) malloc (sizeof (struct GC_objectSharingInfo));
      e->objectLocation = (void*)object;
      e->coreId = (int32_t) s->procId;
      e->front = BOGUS_POINTER;
      fprintf (stderr, "HASH_ADD: "FMTPTR" coreId=%d [%d]\n", (uintptr_t)object, s->procId, s->procId);
      HASH_ADD_PTR (s->objectSharingInfo, objectLocation, e);
    }
  }
}


void addToObjectSharingInfoWalkingShared (GC_state s, objptr* opp) {
  if (not isObjptr(*opp))
    return;
  fixFwdObjptr (s, opp);
  pointer object = objptrToPointer (*opp, s->heap->start);
  if (isPointerInHeap (s, s->sharedHeap, object)) {
    GC_objectSharingInfo e;
    HASH_FIND_PTR (globalHashTable, &object, e);
    if (not e) {
      //We did not find the object in the hash table. Hence, insert it.
      e = (GC_objectSharingInfo) malloc (sizeof (struct GC_objectSharingInfo));
      e->objectLocation = (void*)object;
      e->coreId = currentCoreId;
      e->front = BOGUS_POINTER;
      fprintf (stderr, "HASH_ADD: "FMTPTR" coreId=%d [%d]\n", (uintptr_t)object, currentCoreId, s->procId);
      HASH_ADD_PTR (globalHashTable, objectLocation, e);
    }
    else if (e->coreId != currentCoreId) {
      //the shared heap object is not exlusive to a single core
      fprintf (stderr, "HASH_MODIFY(1): "FMTPTR" coreId=%d [%d]\n", (uintptr_t)object, -1, s->procId);
      e->coreId = -1;
    }
  }
}

void reclaimObjects (GC_state s) {
  GC_objectSharingInfo globalE = NULL;
  size_t totalSize = 0;
  assert (globalHashTable);
  if (s->forwardState.liftingObject == BOGUS_OBJPTR) {
    for (globalE = globalHashTable; globalE != NULL; globalE = globalE->hh.next) {
      if ((uint32_t)globalE->coreId == s->procId) {
        pointer p = (pointer)globalE->objectLocation;
        totalSize += sizeofObject (s, p);
      }
    }

    fprintf (stderr, "totalSize = %s [%d]\n", uintmaxToCommaString (totalSize), s->procId);
    if (totalSize) {
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
          fprintf (stderr, "RECLAIMING: p="FMTPTR" ", (uintptr_t)p);
          forwardObjptr (s, &op);
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

      for (globalE = globalHashTable; globalE != NULL; globalE = globalE->hh.next) {
        if ((uint32_t)globalE->coreId == s->procId) {
          pointer p = (pointer)globalE->objectLocation;
          pointer front = (pointer)globalE->front;
          assert (front != BOGUS_POINTER);
          fillGap (s, front, front + sizeofObject (s, p));
        }
      }
      s->controls->selectiveDebug = TRUE;
    }
  }
}


void reclaim (GC_state s) {
  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s);
  assert (s->objectSharingInfo == NULL);
  foreachGlobalObjptrInScope (s, addToObjectSharingInfoIfObjptrInSharedHeap);
  if (s->heap->start + s->heap->oldGenSize == s->heap->nursery) {
    foreachObjptrInRange (s, s->heap->start, &s->frontier, addToObjectSharingInfoIfObjptrInSharedHeap, FALSE);
  }
  else {
    pointer end = s->heap->start + s->heap->oldGenSize;
    foreachObjptrInRange (s, s->heap->start, &end, addToObjectSharingInfoIfObjptrInSharedHeap, FALSE);
    foreachObjptrInRange (s, s->heap->nursery, &s->frontier, addToObjectSharingInfoIfObjptrInSharedHeap, FALSE);
  }
  LEAVE_LOCAL0 (s);

  s->syncReason = SYNC_MISC;
  ENTER0 (s);
  if (Proc_processorNumber (s) == 0) {

    GC_objectSharingInfo globalE = NULL;
    globalHashTable = NULL;

    //For globals
    currentCoreId = -1;
    for (unsigned int i = 0; i < s->globalsLength; ++i)
      addToObjectSharingInfoWalkingShared (s, &s->globals[i]);

    //For each processor
    for (int proc=0; proc < s->numberOfProcs; proc++) {
      GC_objectSharingInfo e, tmp;
      HASH_ITER (hh, s->procStates[proc].objectSharingInfo, e, tmp) {
        void* location = e->objectLocation;
        HASH_FIND_PTR (globalHashTable, &location, globalE);
        //If element present in globalHashTable and shared heap object is not exclusive to a single core
        if (globalE && globalE->coreId != e->coreId) {
          fprintf (stderr, "HASH_MODIFY(2): "FMTPTR" coreId=%d [%d]\n", (uintptr_t)location, -1, s->procId);
          globalE->coreId = -1;
        }
        //If the element is not in globalHashTable
        else if (not globalE) {
          globalE = (GC_objectSharingInfo) malloc (sizeof (struct GC_objectSharingInfo));
          globalE->objectLocation = location;
          globalE->coreId = proc;
          globalE->front = BOGUS_POINTER;
          fprintf (stderr, "HASH_ADD: "FMTPTR" coreId=%d [%d]\n", (uintptr_t)location, proc, s->procId);
          HASH_ADD_PTR (globalHashTable, objectLocation, globalE);
        }
        HASH_DEL (s->procStates[proc].objectSharingInfo, e);
        free (e);
      }
      s->procStates[proc].objectSharingInfo = NULL;
    }

    //Walking the shared heap
    {
      pointer front = s->sharedHeap->start;
      pointer back = s->sharedFrontier;

      assert (front <= back);
      while (front < back) {
        pointer p = advanceToObjectData (s, front);
        HASH_FIND_PTR (globalHashTable, &p, globalE);
        if (not globalE) {
          currentCoreId = -1;
          objptr o = pointerToObjptr (p, s->sharedHeap->start);
          addToObjectSharingInfoWalkingShared (s, &o);
        }
        else {
          currentCoreId = globalE->coreId;
          globalE->front = front;
        }
        front = foreachObjptrInObject (s, p, addToObjectSharingInfoWalkingShared, FALSE);
      }
    }

    size_t totalObjects = HASH_COUNT (globalHashTable);
    size_t totalExclusive = 0;
    for (globalE = globalHashTable; globalE != NULL; globalE = globalE->hh.next) {
      if (globalE->coreId != -1)
        totalExclusive++;
    }
    fprintf (stderr, "Exclusive objects are %.1f%% of total shared heap objects [%zu]\n",
            (100.0 * ((double)totalExclusive/(double)totalObjects)), totalObjects);
  }
  LEAVE0 (s);

  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s);
  reclaimObjects (s);
  LEAVE_LOCAL0 (s);

  s->syncReason = SYNC_MISC;
  ENTER0 (s);
  if (Proc_processorNumber (s) == 0) {
      GC_objectSharingInfo e, tmp;
      HASH_ITER (hh, globalHashTable, e, tmp) {
        HASH_DEL (globalHashTable, e);
        free (e);
      }
  }
  LEAVE0 (s);

  performSharedGC (s, 0, TRUE);

}
