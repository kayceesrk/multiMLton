/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void foo (__attribute__ ((unused)) GC_state s, __attribute__ ((unused)) GC_frameIndex f) {
}

void GC_noop (void) {}

void GC_printFrames (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    foreachStackFrame (s, foo);
}

pointer GC_getFrameBottom (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    /* TODO : get frameBottom by fixed offset */
    GC_returnAddress returnAddress;
    GC_frameIndex findex;
    GC_frameLayout layout;
    pointer start = s->stackTop;
    if (DEBUG_SPLICE) {
        fprintf (stderr, "FrameBottom [%d]\n", Proc_processorNumber (s));
        fprintf (stderr, "\ttop = "FMTPTR"\n", (uintptr_t)start);
        fflush (stderr);
    }

    returnAddress = *((GC_returnAddress*)(start - GC_RETURNADDRESS_SIZE));
    findex = getFrameIndexFromReturnAddress (s, returnAddress);
    unless (findex < s->frameLayoutsLength)
      die ("top = "FMTPTR"  returnAddress = "FMTRA"  findex = "FMTFI"\n",
           (uintptr_t)start, (uintptr_t)returnAddress, findex);
    layout = &(s->frameLayouts[findex]);
    start -= layout->size; /* This points to the bottom of the recur function */
    if (DEBUG_SPLICE) {
        fprintf (stderr, "\tframeBottom = "FMTPTR"\n", (uintptr_t)start);
        fflush (stderr);
    }

    return start;
}


int GC_getFrameBottomAsOffset (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    pointer p = GC_getFrameBottom ();
    return p - s->stackBottom;
}

int GC_getCopiedSize (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    return s->copiedSize;
}

GC_thread GC_copyFrames (int startOffset) {
    /* TODO : Avoid this computation and subtract fixed offset
     * from stackTop to get end pointer */
    GC_state s = pthread_getspecific (gcstate_key);

    pointer start = s->stackBottom + startOffset;
    pointer end = GC_getFrameBottom ();

    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;

    GC_thread th = newThread (s, end-start);
    GC_stack stk = (GC_stack) objptrToPointer (th->stack, s->heap->start);

    if (DEBUG_SPLICE) {
        fprintf (stderr, "\ncopyFrames [%d]\n", Proc_processorNumber (s));
    }

    start = s->stackBottom + startOffset;
    end = GC_getFrameBottom ();
    assert (start < end);

    long int numBytes = end-start;
    s->copiedSize = numBytes;
    if (DEBUG_SPLICE) {
        fprintf (stderr, "\tnumBytes = %ld\n", numBytes);
        GC_printFrames ();
        fflush (stderr);
    }

    pointer dest = getStackBottom (s, stk);
    stk->used = numBytes;

    memcpy (dest, start, numBytes);
    return th;
}

void GC_jumpDown (GC_state s, int offset) {
    pointer p = s->stackBottom + offset;
    if (DEBUG_SPLICE) {
        fprintf (stderr, "\njumpingDown = "FMTPTR" [%d]",
                            (uintptr_t)p,
                            Proc_processorNumber (s));
        fflush (stderr);
    }
    s->stackTop = p;
    s->atomicState --;
}

void GC_printPointer (pointer p) {
    printf ("["FMTPTR"]", (uintptr_t)p);
    fflush(stdout);
}

void GC_printPointerAtOffset (int offset) {
    GC_state s = pthread_getspecific (gcstate_key);
    printf ("["FMTPTR"]", (uintptr_t)(s->stackBottom + offset));
    fflush(stdout);
}

void GC_printStackTop (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    fprintf (stderr, "\nstackTop ["FMTPTR"] [%d]", (uintptr_t)s->stackTop, Proc_processorNumber (s));
    fflush (stderr);
}

GC_thread GC_preemptAsync (GC_thread oriThrd, int startOffset) {
    assert (isObjptr ((objptr)(oriThrd)));
    GC_state s = pthread_getspecific (gcstate_key);

    /* Find out the size needed for newThread */
    long int size = -1;
    {
        GC_stack oriStk = (GC_stack) objptrToPointer (oriThrd->stack, s->heap->start);
        size = oriStk->used - startOffset;
    }
    assert (size > 0);

    /* newThread () might do GC, so backup stuff */
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    assert (s->savedThread == BOGUS_OBJPTR);
    s->savedThread = pointerToObjptr ((pointer)oriThrd, s->heap->start);

    GC_thread th = newThread (s, size);
    /* restore */
    oriThrd = (GC_thread)(objptrToPointer(s->savedThread, s->heap->start));
    s->savedThread = BOGUS_OBJPTR;

    GC_stack stk = (GC_stack) objptrToPointer (th->stack, s->heap->start);

    /* Find the limits of async */
    GC_stack oriStk = (GC_stack) objptrToPointer (oriThrd->stack, s->heap->start);
    pointer start = getStackBottom (s, oriStk) + startOffset;
    pointer end = getStackBottom (s, oriStk) + oriStk->used;
    assert (start < end);

    long int numBytes = end-start;
    assert (size == numBytes);
    s->copiedSize = numBytes;
    if (DEBUG_SPLICE) {
        fprintf (stderr, "\tnumBytes = %ld\n", numBytes);
        GC_printFrames ();
        fflush (stderr);
    }

    pointer dest = getStackBottom (s, stk);
    stk->used = numBytes;
    memcpy (dest, start, numBytes);

    /* set control to threadlet sitting below */
    oriStk->used = startOffset;

    return th;
}

void GC_prefixAndSwitchTo (GC_state s, GC_thread thrd) {
    /* TODO : get frameBottom by fixed offset */

    assert (s->atomicState > 0);

    if (DEBUG_SPLICE)
        fprintf (stderr, "\nprefixAndSwitchTo [%d]\n", Proc_processorNumber (s));

    GC_stack stk = (GC_stack) objptrToPointer (thrd->stack, s->heap->start);
    pointer p = getStackBottom (s, stk);
    int size = stk->used;

    assert (s->stackLimit > s->stackTop + size);

    pointer start = GC_getFrameBottom ();
    memcpy (start, p, size);
    s->stackTop = start + size;
    if (DEBUG_SPLICE) {
        fprintf (stderr, "\tprefixing frame of size %d\n", size);
        fprintf (stderr, "\tnewStackTop = "FMTPTR"\n", (uintptr_t) s->stackTop);
        fflush (stderr);
    }

    s->atomicState --;
    return;
}
