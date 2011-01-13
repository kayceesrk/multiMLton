/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#define FACT 4

static inline void liftObjptr (GC_state s, objptr *opp);

/* Move an object from local heap to the shared heap */
static inline void liftObjptr (GC_state s, objptr *opp) {

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
    else if (DEBUG_LWTGC) {
            fprintf (stderr, "\t pointer "FMTPTR" was not lifted\n", (uintptr_t)new_p);
    }
}


static inline void assertLiftedObjptr (GC_state s, objptr *opp) {
    objptr op = *opp;
    bool res = isObjptrInHeap (s, s->sharedHeap, op);
    GC_header h = getHeader(objptrToPointer (op, s->heap->start));
    GC_objectTypeTag tag;
    if (DEBUG_DETAILED)
        fprintf (stderr, "assertLiftedObjptr ("FMTOBJPTR")\n", *opp);
    splitHeader (s, h, &tag, NULL, NULL, NULL);
    bool stackType = (tag == STACK_TAG);
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
    splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

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
  s->forwardState.rangeListFirst = s->forwardState.rangeListLast = NULL;

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
  assert (!s->forwardState.rangeListFirst);
  assert (!s->forwardState.rangeListLast);

  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: updateWeaksForCheneyCopy\n");
  //updateWeaksForCheneyCopy (s);
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: resizeHeap\n");
  //resizeHeap (s, s->heap->oldGenSize);

  //Check
  if (DEBUG_LWTGC) {
    fprintf (stderr, "liftAllObjectsDuringInit: check(1)\n");
    foreachObjptrInRange (s, toStart, &s->forwardState.back, assertLiftedObjptr, TRUE);
    fprintf (stderr, "liftAllObjectsDuringInit: check(2)\n");
  }

  /* Force a major GC to clean up the local heap. */
  fixForwardingPointers (s, TRUE);
  endAtomic (s);
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: Exiting\n");

}

pointer GC_move (GC_state s, pointer p) {
  if (!(s->heap->start <= p and p < s->heap->start + s->heap->size)) {
      if (DEBUG_LWTGC)
          fprintf (stderr, "GC_move: pointer "FMTPTR" not in heap\n", (uintptr_t)p);
      return p;
  }

  printf ("GC_move\n");

  /* ENTER (0) */
  s->syncReason = SYNC_FORCE;
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "GC_move: \n");

  /* If objct has already been lifted, return */
  if (isObjectLifted (getHeader (p))) {
    /* LEAVE (0) */
    s->syncReason = SYNC_NONE;
    endAtomic (s);
    return p;
  }

  //Set up the forwarding state
  s->forwardState.toStart = s->sharedFrontier;
  s->forwardState.toLimit = s->sharedHeap->start + s->sharedHeap->size;
  s->forwardState.back = s->forwardState.toStart;
  s->forwardState.rangeListFirst = s->forwardState.rangeListLast = NULL;
  s->forwardState.amInMinorGC = TRUE;

  /* Forward the given object to sharedHeap */
  objptr op = pointerToObjptr (p, s->heap->start);
  objptr* pOp = &op;
  liftObjptr (s, pOp);
  foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, liftObjptr, TRUE);
  s->forwardState.amInMinorGC = FALSE;
  assert (!s->forwardState.rangeListFirst);
  assert (!s->forwardState.rangeListLast);

  /* Force a garbage collection. Essential to fix the forwarding pointers from
   * the previous step.
   * NOTE: Major GC needs to be forced only if moving objects from the major heap.
   * ENTER0 (s) -- atomicState is atomic */
  if (not s->canMinor || TRUE /* Force Major */)
    fixForwardingPointers (s, TRUE);
  else
    fixForwardingPointers (s, TRUE);

  /* LEAVE0 (s) */
  endAtomic (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "GC_move: Exiting\n");

  return objptrToPointer (*pOp, s->sharedHeap->start);
}

void moveEachObjptrInObject (GC_state s, pointer p) {
  assert (p != BOGUS_POINTER);

  /* ENTER (0) */
  s->syncReason = SYNC_FORCE;
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "moveEachObjptrInObject: \n");

  /* If objct has already been lifted, return */
  if (isObjectLifted (getHeader (p))) {
    /* LEAVE (0) */
    s->syncReason = SYNC_NONE;
    endAtomic (s);
    return;
  }

  //Set up the forwarding state
  s->forwardState.toStart = s->sharedFrontier;
  s->forwardState.toLimit = s->sharedHeap->start + s->sharedHeap->size;
  s->forwardState.back = s->forwardState.toStart;
  s->forwardState.rangeListFirst = s->forwardState.rangeListLast = NULL;
  s->forwardState.amInMinorGC = TRUE;

  /* Forward objptrs in the given object to sharedHeap */
  foreachObjptrInObject (s, p, liftObjptr, TRUE);
  foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, liftObjptr, TRUE);
  s->forwardState.amInMinorGC = FALSE;
  assert (!s->forwardState.rangeListFirst);
  assert (!s->forwardState.rangeListLast);

  if (not s->canMinor || TRUE /* Force Major */)
    fixForwardingPointers (s, TRUE);
  else
    fixForwardingPointers (s, TRUE);

  /* LEAVE0 (s) */
  endAtomic (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "moveEachObjptrInObject: Exiting\n");

  return;
}

