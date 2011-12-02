/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* Move an object from local heap to the shared heap */
static inline void liftObjptr (GC_state s, objptr *opp) {

  fixFwdObjptr (s, opp);
  /* If pointer has already been forwarded, skip setting lift bit */
  if (isObjptrInHeap (s, s->sharedHeap, *opp)) {
    if (DEBUG_LWTGC) {
      fprintf (stderr, "\t object in shared heap\n");
    }
    return;
  }
  forwardObjptrToSharedHeap (s, opp);

  objptr new_op = *opp;
  pointer new_p = objptrToPointer (new_op, s->heap->start);
  GC_header new_header = getHeader(new_p);
  GC_header* new_headerp = getHeaderp (new_p);

  if (isPointerInHeap (s, s->sharedHeap, new_p)) {
    /* Set lift mask */
    if (DEBUG_LWTGC)
      fprintf (stderr, "\t pointer "FMTPTR" headerp "FMTPTR" : setting header "FMTHDR" to "FMTHDR"\n",
               (uintptr_t)new_p, (uintptr_t)new_headerp, new_header, new_header | LIFT_MASK);
    *new_headerp = new_header | LIFT_MASK;
  }
  else {
    if (DEBUG_LWTGC)
      fprintf (stderr, "\t pointer "FMTPTR" was not lifted\n", (uintptr_t)new_p);
  }
}

/* Move an object from local heap to the shared heap */
static inline void liftObjptrAndFillOrig (GC_state s, objptr *opp) {

  if (not isObjptrInNursery (s, s->heap, *opp)) {
    if (DEBUG_LWTGC) {
      fprintf (stderr, "\t is not in nursery\n");
    }
  }
  /* If pointer has already been forwarded, skip setting lift bit */
  if (isObjptrInHeap (s, s->sharedHeap, *opp)) {
    if (DEBUG_LWTGC) {
      fprintf (stderr, "\t object in shared heap\n");
    }
    return;
  }
  pointer old_p = objptrToPointer (*opp, s->heap->start);
  forwardObjptrToSharedHeap (s, opp);

  objptr new_op = *opp;
  pointer new_p = objptrToPointer (new_op, s->sharedHeap->start);

  uint32_t typeIndex = (getHeader(new_p) & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  uint32_t threadTypeIndex = 1;

  if (isPointerInHeap (s, s->sharedHeap, new_p) && (typeIndex != threadTypeIndex)) {
    size_t objSize = sizeofObject (s, new_p);
    old_p -= sizeofObjectHeader (s, getHeader (new_p));
    if (DEBUG_DETAILED)
      fprintf (stderr, "\t filling Gap between "FMTPTR" and "FMTPTR" of size %ld [%d]\n",
               (uintptr_t)old_p, (uintptr_t)(old_p + objSize), objSize, s->procId);
    fillGap (s, old_p, old_p + objSize);
  }
  else if (DEBUG_LWTGC) {
    fprintf (stderr, "\t pointer "FMTPTR" was not lifted\n", (uintptr_t)new_p);
  }
}

static inline void assertLiftedObjptr (GC_state s, objptr *opp) {
  objptr op = *opp;
  bool stackType = FALSE;

  if (DEBUG_DETAILED)
    fprintf (stderr, "assertLiftedObjptr ("FMTOBJPTR") [%d]\n", *opp, s->procId);

  GC_header h = getHeader (objptrToPointer (op, s->heap->start));
  if (h == GC_FORWARDED) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "assertLiftedObjptr: forwarding [%d]\n", s->procId);
    fixFwdObjptr (s, opp);
    op = *opp;
  }

  bool res = isObjptrInHeap (s, s->sharedHeap, op);
  if (!res) {
    GC_header* hp = getHeaderp (objptrToPointer (op, s->heap->start));
    GC_objectTypeTag tag;
    splitHeader (s, h, hp, &tag, NULL, NULL, NULL, NULL, NULL);
    stackType = (tag == STACK_TAG);
  }

  //To keep gcc unused check happy
  res = !(!res);
  stackType = !(!stackType);

  assert (res || stackType);
}


/* Lifts object pointed to be the thread, but not the thread itself */
static inline void liftThreadDuringInit (GC_state s, objptr op) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  if (isObjptr (op)) {
    pointer p = objptrToPointer (op, s->heap->start);
    header = getHeader (p);
    splitHeader(s, header, getHeaderp (p), &tag, NULL, &bytesNonObjptrs, &numObjptrs, NULL, NULL);

    /* We know that the object is a GC_thread */
    assert (tag == NORMAL_TAG);
    assert (numObjptrs == 1);
    p += bytesNonObjptrs;

    pointer stack = objptrToPointer (*(objptr*)p, s->heap->start);
    assert (stack);
    foreachObjptrInObject (s, stack, liftObjptr, TRUE);
  }
}

void liftAllObjectsDuringInit (GC_state s) {
  s->syncReason = SYNC_FORCE;
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: \n");

  //Set up the forwarding state
  pointer toStart = alignFrontier (s, s->sharedFrontier);
  s->forwardState.toStart = s->sharedFrontier;
  s->forwardState.toLimit = s->sharedHeap->start + s->sharedHeap->size;
  s->forwardState.back = toStart;
  s->forwardState.rangeListCurrent = NULL;
  s->forwardState.rangeListLast = NULL;
  s->forwardState.rangeListFirst = NULL;
  s->forwardState.forceStackForwarding = FALSE;

  //Forward
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: foreachGlobalObjptr\n");
  for (unsigned int i = 0; i < s->globalsLength; ++i) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "foreachGlobal %u [%d]\n", i, s->procId);
    callIfIsObjptr (s, liftObjptr, &s->globals [i]);
  }

  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: foreachObjptrInRange\n");
  foreachObjptrInRange (s, toStart, &s->forwardState.back, liftObjptr, TRUE);
  clearRangeList (s);

  //Check
  if (DEBUG_LWTGC) {
    fprintf (stderr, "liftAllObjectsDuringInit: check(1)\n");
    foreachObjptrInRange (s, toStart, &s->forwardState.back, assertLiftedObjptr, TRUE);
    fprintf (stderr, "liftAllObjectsDuringInit: check(2)\n");
  }

  /* Force a major GC to clean up the local heap. */
  fixForwardingPointers (s, TRUE);
  s->lastSharedMajorStatistics->bytesLive = s->sharedHeap->size;
  endAtomic (s);
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: Exiting\n");

}

/* This lifts the transitive closure to the shared heap */
void moveTransitiveClosure (GC_state s, objptr* opp,
                            bool forceStackForwarding,
                            bool fillOrig) {
  struct timeval tv_rt;
  volatile bool statValid = TRUE;
  bool done = FALSE;

  startWallTiming (&tv_rt);
  s->forwardState.liftingObject = *opp;

  while (!done) {
    //Set up the forwarding state
    s->forwardState.forceStackForwarding = forceStackForwarding;
    s->forwardState.toStart = s->sharedFrontier;
    s->forwardState.toLimit = s->sharedHeap->start + s->sharedHeap->size;
    s->forwardState.back = s->forwardState.toStart;
    s->forwardState.rangeListCurrent = NULL;
    s->forwardState.rangeListLast = NULL;
    s->forwardState.rangeListFirst = NULL;
    s->forwardState.amInMinorGC = TRUE;
    s->forwardState.isReturnLocationSet = TRUE;

    GC_foreachObjptrFun f = liftObjptr;
    if (fillOrig)
      f = liftObjptrAndFillOrig;

    if (setjmp (s->forwardState.returnLocation) == 0) {
      /* Original call: Forward the given object to sharedHeap */
      f (s, &s->forwardState.liftingObject);
      foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, f, TRUE);
      done = true;
    }
    else {
      /* retry -- this time it should work since we have perfomed a shared GC
       * and hence should not run out of space */
      if (DEBUG_LWTGC)
        fprintf (stderr, "retry with "FMTPTR" [%d]\n",
                 (uintptr_t)s->forwardState.liftingObject, s->procId);
      statValid = FALSE;
    }
  }


  *opp = s->forwardState.liftingObject;
  s->forwardState.isReturnLocationSet = FALSE;
  s->forwardState.amInMinorGC = FALSE;
  s->forwardState.forceStackForwarding = FALSE;
  s->forwardState.liftingObject = BOGUS_OBJPTR;
  clearRangeList (s);

  if (statValid && needGCTime (s))
    stopWallTiming (&tv_rt, &s->cumulativeStatistics->tv_rt);

}

pointer GC_moveFromWB (GC_state s, pointer p,
                       bool forceStackForwarding,
                       bool skipFixForwardingPointers) {
  return GC_move (s, p, forceStackForwarding, skipFixForwardingPointers);
}


pointer GC_move (GC_state s, pointer p,
                 bool forceStackForwarding,
                 bool skipFixForwardingPointers) {
  return GC_moveWithCopyType (s, p, forceStackForwarding, skipFixForwardingPointers, TRUE);

}

pointer GC_moveWithCopyType (GC_state s, pointer p,
                             bool forceStackForwarding,
                             bool skipFixForwardingPointers,
                             bool copyImmutable) {

  size_t gcID = s->lastSharedMajorStatistics->bytesLive;
  assert (s);
  assert (s->heap);
  if (!(s->heap->start <= p and p < s->heap->start + s->heap->size)) {
    if (DEBUG_LWTGC)
      fprintf (stderr, "GC_move: pointer "FMTPTR" not in heap\n", (uintptr_t)p);
    return p;
  }

  /* If objct has already been lifted, return */
  if (isObjectLifted (getHeader (p))) {
    return p;
  }

  if (Proc_isInitialized (s))
    skipFixForwardingPointers = TRUE;

  if (skipFixForwardingPointers) {
    s->syncReason = SYNC_LIFT_NO_GC;
    s->copyImmutable = copyImmutable;
    s->copyObjectMap = NULL;
  }
  else
    s->syncReason = SYNC_LIFT;

  ENTER_LOCAL0 (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "GC_move "FMTPTR" [%d]\n", (uintptr_t)p, s->procId);


  objptr op = pointerToObjptr (p, s->heap->start);
  objptr* pOp = &op;

  moveTransitiveClosure (s, pOp, forceStackForwarding, FALSE);

  if (DEBUG_LWTGC)
    fprintf (stderr, "GC_move: After move transitive closure [%d]\n", s->procId);

  assert (isObjptrInHeap (s, s->sharedHeap, op));

  if (skipFixForwardingPointers) {
    //Free the objects in copyObjectMap
    CopyObjectMap *e, *tmp;
    HASH_ITER (hh, s->copyObjectMap, e, tmp) {
      HASH_DEL (s->copyObjectMap, e);

      //Only fix forwarding pointers if we haven't done a shared GC
      if (gcID == s->lastSharedMajorStatistics->bytesLive)
        foreachObjptrInObject (s, e->oldP, fixFwdObjptr, TRUE);
      free (e);
    }
    s->copyObjectMap = NULL;
    s->copyImmutable = FALSE;

    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    foreachObjptrInObject (s, (pointer)getStackCurrent(s), fixFwdObjptr, TRUE);
  }
  else {
    /* Force a garbage collection. Essential to fix the forwarding pointers from
     * the previous step.
     * NOTE: Major GC needs to be forced only if moving objects from the major heap. */
    if (not s->canMinor || TRUE /* Force Major */)
      fixForwardingPointers (s, TRUE);
    else
      fixForwardingPointers (s, TRUE);
  }

  LEAVE_LOCAL0 (s);

  pointer res = objptrToPointer (*pOp, s->sharedHeap->start);
  assert (res != BOGUS_POINTER);

  if (DEBUG_LWTGC)
    fprintf (stderr, "GC_move: Exiting with "FMTPTR" [%d]\n",
             (uintptr_t)res, s->procId);
  return res;
}

void forceLocalGC (GC_state s) {
  if (DEBUG_LWTGC)
    fprintf (stderr, "forceLocalGC [%d]\n", s->procId);

  s->syncReason = SYNC_FORCE;
  ENTER_LOCAL0 (s);

  fixForwardingPointers (s, TRUE);

  LEAVE_LOCAL0 (s);
}

void moveEachObjptrInObject (GC_state s, pointer p) {
  assert (p != BOGUS_POINTER);

  /* If objct has already been lifted, return */
  if (isObjectLifted (getHeader (p))) {
    return;
  }

  s->syncReason = SYNC_LIFT;
  ENTER_LOCAL0 (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "moveEachObjptrInObject: \n");

  //Set up the forwarding state
  s->forwardState.toStart = s->sharedFrontier;
  s->forwardState.toLimit = s->sharedHeap->start + s->sharedHeap->size;
  s->forwardState.back = s->forwardState.toStart;
  s->forwardState.rangeListCurrent = NULL;
  s->forwardState.rangeListLast = NULL;
  s->forwardState.rangeListFirst = NULL;
  s->forwardState.amInMinorGC = TRUE;
  s->forwardState.forceStackForwarding = FALSE;

  /* Forward objptrs in the given object to sharedHeap */
  foreachObjptrInObject (s, p, liftObjptr, TRUE);
  foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, liftObjptr, TRUE);
  s->forwardState.amInMinorGC = FALSE;
  s->forwardState.liftingObject = BOGUS_OBJPTR;
  clearRangeList (s);

  if (not s->canMinor || TRUE /* Force Major */)
    fixForwardingPointers (s, TRUE);
  else
    fixForwardingPointers (s, TRUE);

  LEAVE_LOCAL0 (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "moveEachObjptrInObject: Exiting\n");

  return;
}

void jumpToReturnLocation (GC_state s) {
  assert (s->forwardState.isReturnLocationSet);
  s->forwardState.isReturnLocationSet = FALSE;
  longjmp (s->forwardState.returnLocation, 1);
}

bool GC_isInSharedOrForwarded (GC_state s, pointer p) {
  if (getHeader (p) == GC_FORWARDED || isPointerInHeap (s, s->sharedHeap,p))
    return TRUE;
  return FALSE;
}

static inline void assertNotInLocalHeap (GC_state s, objptr* opp) {
  assert (not isPointerInHeap (s, s->heap, (pointer)*opp));
  s=s;
  opp = opp;
}

static inline void assertThreadCleanliness (GC_state s, objptr* opp) {
  CopyObjectMap *parentFound, *currentFound;
  pointer p = *(pointer*)opp;
  void* current = (void*)p;
  void* parent = (pointer)s->tmpPointer;
  HASH_FIND_PTR (s->copyObjectMap, &current, currentFound);
  HASH_FIND_PTR (s->copyObjectMap, &parent, parentFound);
  if (objectHasIdentity (s, getHeader (p))) {
    if (parentFound && (!currentFound))
      assert ("Closure points to local heap" && 0);
    else if (currentFound && (not parentFound))
      assert ("Local heap points to mutable closure object" && 0);
  }
}

pointer GC_copyToBuffer (GC_state s, pointer p, size_t size) {
  objptr op = pointerToObjptr (p, s->heap->start);
  void* buffer = malloc_safe (size);
  s->forwardState.toStart = s->forwardState.back = buffer;
  s->forwardState.toLimit = (pointer)((char*)buffer + size);
  assert (s->copyObjectMap == NULL);
  s->copyObjectMap = NULL;

  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s);
  copyObjptr (s, &op);
  foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, copyObjptr, TRUE);
  LEAVE_LOCAL0 (s);

  CopyObjectMap *e, *tmp;
  HASH_ITER (hh, s->copyObjectMap, e, tmp) {
    HASH_DEL (s->copyObjectMap, e);
    free (e);
  }
  s->copyObjectMap = NULL;

  return (pointer)buffer;
}
