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
    if (isObjptrInHeap (s, s->heap, *opp)) {
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

    /* Set lift mask */
    if (DEBUG_LWTGC)
        fprintf (stderr, "\t pointer "FMTPTR" headerp "FMTPTR" : setting header "FMTHDR" to "FMTHDR"\n",
                 (uintptr_t)new_p, (uintptr_t)new_headerp, new_header, new_header | LIFT_MASK);
    *new_headerp = new_header | LIFT_MASK;
}


static inline void assertLiftedObjptr (GC_state s, objptr *opp) {
    objptr op = *opp;
    bool res = isObjptrInHeap (s, s->sharedHeap, op);
    assert (res);
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
  s->forwardState.toLimit = s->sharedLimit;
  s->forwardState.back = toStart;

  //Forward
  //Foreach global
  for (unsigned int i = 0; i < s->globalsLength; ++i) {
    if (DEBUG_LWTGC)
      fprintf (stderr, "liftAllObjectsDuringInit: foreachGlobal %u\n", i);
    callIfIsObjptr (s, liftObjptr, &s->globals [i]);
  }
  //Foreach thread
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: foreachGlobal threads\n");
  if (s->procStates) {
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      liftThreadDuringInit (s, s->procStates[proc].callFromCHandlerThread);
      liftThreadDuringInit (s, s->procStates[proc].currentThread);
      liftThreadDuringInit (s, s->procStates[proc].savedThread);
      liftThreadDuringInit (s, s->procStates[proc].signalHandlerThread);

      if (s->procStates[proc].roots) {
        for (uint32_t i = 0; i < s->procStates[proc].rootsLength; i++) {
          callIfIsObjptr (s, liftObjptr, &s->procStates[proc].roots[i]);
        }
      }
    }
  }
  else {
    liftThreadDuringInit (s, s->callFromCHandlerThread);
    liftThreadDuringInit (s, s->currentThread);
    liftThreadDuringInit (s, s->savedThread);
    liftThreadDuringInit (s, s->signalHandlerThread);
  }

  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: foreachObjptrInRange\n");
  foreachObjptrInRange (s, toStart, &s->forwardState.back, liftObjptr, TRUE);
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: updateWeaksForCheneyCopy\n");
  //updateWeaksForCheneyCopy (s);
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: resizeHeap\n");
  //resizeHeap (s, s->heap->oldGenSize);

  //Check
  if (DEBUG_LWTGC) {
    fprintf (stderr, "liftAllObjectsDuringInit: check\n");
    foreachObjptrInRange (s, toStart, &s->forwardState.back, assertLiftedObjptr, TRUE);
  }

  /* Force a major GC to clean up the local heap. */

  performGC (s, 0, 0, TRUE, TRUE, TRUE);
  endAtomic (s);
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjectsDuringInit: Exiting\n");

}

void GC_move (GC_state s, pointer p) {

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
    return;
  }

  //Set up the forwarding state
  s->forwardState.toStart = s->sharedFrontier;
  s->forwardState.toLimit = s->sharedLimit;
  s->forwardState.back = s->forwardState.toStart;
  s->forwardState.amInMinorGC = TRUE;

  /* Forward the given object to sharedHeap */
  objptr op = pointerToObjptr (p, s->heap->start);
  objptr* pOp = &op;
  liftObjptr (s, pOp);
  foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, liftObjptr, TRUE);
  s->forwardState.amInMinorGC = FALSE;

  //Check
  if (DEBUG_LWTGC) {
    fprintf (stderr, "GC_move: check\n");
    foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, assertLiftedObjptr, TRUE);
  }

  /* Force a garbage collection. Essential to fix the forwarding pointers from
   * the previous step.
   * ENTER0 (s) -- atomicState is atomic */
  if (not s->canMinor || TRUE /* Force Major */)
    performGC (s, 0, 0, TRUE, TRUE, TRUE);
  else
    performGC (s, 0, 0, FALSE, TRUE, TRUE);

  /* LEAVE0 (s) */
  endAtomic (s);

  if (DEBUG_LWTGC)
    fprintf (stderr, "GC_move: Exiting\n");

  return;
}



