/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void forwardObjptrToAuxHeap (GC_state s, objptr *opp) {
    forwardObjptr (s, opp);
    objptr op = *opp;
    pointer p = objptrToPointer (op, s->heap->start);
    GC_header header = getHeader (p);
    GC_header* headerp = getHeaderp (p);

    *headerp = header & LIFT_MASK;
}

void GC_move (GC_state s, pointer p) {

    s->syncReason = SYNC_FORCE;
    ENTER0 (s);

    //create auxHeap if you haven't already done so
    if (not isHeapInit (s->auxHeap))
        createHeap (s, s->auxHeap, s->heap->size * 2, s->heap->size);

    //Set up the forwarding state
    s->forwardState.toStart = s->auxHeap->start + s->auxHeap->oldGenSize;
    s->forwardState.toLimit = s->auxHeap->start + s->auxHeap->size;
    s->forwardState.back = s->forwardState.toStart;
    s->forwardState.amInMinorGC = TRUE;

    /* Forward the given object to auxHeap */
    objptr op = pointerToObjptr (p, s->heap->start);
    objptr* pOp = &op;
    forwardObjptrToAuxHeap (s, pOp);
    foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, forwardObjptrToAuxHeap, TRUE);
    s->auxHeap->oldGenSize = s->forwardState.back - s->auxHeap->start;
    s->forwardState.amInMinorGC = TRUE;

    /* Force a minor collection. Essential to fix the forwarding pointers from
     * the previous step.
     */
    minorCheneyCopyGC (s);

    LEAVE0 (s);
    return;
}
