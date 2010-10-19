/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void forwardObjptrIfInNurseryToAuxHeap (GC_state s, objptr *opp);

void forwardObjptrIfInNurseryToAuxHeap (GC_state s, objptr *opp) {

    if (not isObjptrInNursery (s, *opp)) {
        if (DEBUG_LWTGC) {
            fprintf (stderr, "\t is not in nursery\n");
        }
    }
    objptr op = *opp;
    pointer p = objptrToPointer (op, s->heap->start);
    GC_header* headerp = getHeaderp (p);
    GC_header header = getHeader (p);

    /* If pointer has already been forwarded, skip setting lift bit */
    if (getHeader (p) == 1) {
        if (DEBUG_LWTGC) {
            fprintf (stderr, "\t skipping lift bit setting\n");
        }
        return;
    }

    forwardObjptr (s, opp);
    assert (*headerp == GC_FORWARDED);

    op = *opp;
    p = objptrToPointer (op, s->heap->start);
    assert (header == getHeader (p));
    headerp = getHeaderp (p);
    assert (getHeader (p) != 1);

    /* Set lift mask */
    if (DEBUG_LWTGC)
        fprintf (stderr, "\t pointer "FMTPTR" headerp "FMTPTR" : setting header "FMTHDR" to "FMTHDR"\n",
                 (uintptr_t)p, (uintptr_t)headerp, header, header | LIFT_MASK);
    *headerp = header | LIFT_MASK;
}

void GC_move (GC_state s, pointer p) {

    /* ENTER (0) */
    s->syncReason = SYNC_FORCE;
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    beginAtomic (s);
    Proc_beginCriticalSection(s);

    if (DEBUG_LWTGC)
        fprintf (stderr, "GC_move: \n");

    /* If objct has already been lifted, return */
    if (isObjectLifted (getHeader (p))) {
        /* LEAVE (0) */
        s->syncReason = SYNC_NONE;
        Proc_endCriticalSection(s);
        endAtomic (s);
        return;
    }

    //create auxHeap if you haven't already done so
    if (s->auxHeap->size == 0) {
        if (DEBUG_LWTGC)
            fprintf (stderr, "\tCreating auxheap of size = %ld bytes\n", s->heap->size);
        createHeap (s, s->auxHeap, s->heap->size, s->heap->size);

        /* set up new offsets in gc_state */
        for (int proc = 0; proc < s->numberOfProcs; proc ++) {
            s->procStates[proc].sharedHeapStart = s->auxHeap->start;
            s->procStates[proc].sharedHeapEnd = s->auxHeap->start + s->auxHeap->size;
        }

    }

    //Set up the forwarding state
    s->forwardState.toStart = s->auxHeap->start + s->auxHeap->oldGenSize;
    s->forwardState.toLimit = s->auxHeap->start + s->auxHeap->size;
    s->forwardState.back = s->forwardState.toStart;
    s->forwardState.amInMinorGC = TRUE;

    /* Forward the given object to auxHeap */
    objptr op = pointerToObjptr (p, s->heap->start);
    objptr* pOp = &op;
    forwardObjptrIfInNurseryToAuxHeap (s, pOp);
    foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, forwardObjptrIfInNurseryToAuxHeap, TRUE);
    s->auxHeap->oldGenSize = s->forwardState.back - s->auxHeap->start;
    s->forwardState.amInMinorGC = FALSE;


    /* Force a garbage collection. Essential to fix the forwarding pointers from
     * the previous step.
     * ENTER0 (s) -- atomicState is atomic */
    if (not s->canMinor || TRUE /* Force Major */)
        performGC (s, 0, 0, TRUE, TRUE, TRUE);
    else
        performGC (s, 0, 0, FALSE, TRUE, TRUE);

    /* LEAVE0 (s) */
    Proc_endCriticalSection(s);
    endAtomic (s);

    return;
}



