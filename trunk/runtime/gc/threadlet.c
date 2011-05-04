/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_noop (void) {}

void GC_printFrames (void) {
    //GC_state s = pthread_getspecific (gcstate_key);
    //foreachStackFrame (s, foo);
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
    int res = p - s->stackBottom;
    assert (res >= 0 && res < (s->stackTop - s->stackBottom));
    return res;
}

int GC_getCopiedSize (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    return s->copiedSize;
}

GC_thread GC_copyParasite (int startOffset) {
    /* TODO : Avoid this computation and subtract fixed offset
     * from stackTop to get end pointer */
    GC_state s = pthread_getspecific (gcstate_key);
    s->cumulativeStatistics->numParasitesReified++;

    assert (startOffset >=0);

    pointer start = s->stackBottom + startOffset;
    pointer end = GC_getFrameBottom ();

    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;

    assert (end-start > 0);

    GC_thread th = newThread (s, end-start);
    GC_stack stk = (GC_stack) objptrToPointer (th->stack, s->heap->start);
    stk->isParasitic = TRUE;

    //Save the exception stack offset relative to parasite bottom. This will be
    //used to get to the correct exception state when the parasite is prefixed
    //to a host.
    th->exnStack = s->exnStack - (size_t)startOffset;
    //All parasites have a default handler. Assert this indirectly
    assert (s->exnStack > (size_t)startOffset);

    if (DEBUG_SPLICE) {
        fprintf (stderr, "\ncopyParasite [%d]\n", Proc_processorNumber (s));
    }

    start = s->stackBottom + startOffset;
    end = GC_getFrameBottom ();
    assert (start < end);

    long int numBytes = end-start;
    s->cumulativeStatistics->bytesParasiteStack += numBytes;

    s->copiedSize = numBytes;
    if (DEBUG_SPLICE) {
        fprintf (stderr, "\tnumBytes = %ld\n", numBytes);
        GC_printFrames ();
        fflush (stderr);
    }

    pointer dest = getStackBottom (s, stk);
    stk->used = numBytes;

    s->amInGC = TRUE; //For profiler safety
    memcpy (dest, start, numBytes);
    s->amInGC = FALSE;

    if (MEASURE_PARASITE_CLOSURE) {
      s->cumulativeStatistics->bytesParasiteClosure +=
        GC_sizeInLocalHeap (s, (pointer)th);
    }

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
    assert (s->stackBottom <= s->stackTop);
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

bool GC_proceedToExtract (pointer p, int startOffset) {
    GC_state s = pthread_getspecific (gcstate_key);
    GC_thread oriThrd = (GC_thread) (p + offsetofThread (s));

    /* Find out the size needed for newThread */
    long int size = -1;
    GC_stack oriStk = (GC_stack) objptrToPointer (oriThrd->stack, s->heap->start);
    size = oriStk->used - startOffset;
    if (size <= 0) return false;
    return true;
}

GC_thread GC_extractParasite (pointer p, int startOffset) {
    GC_state s = pthread_getspecific (gcstate_key);
    GC_thread oriThrd = (GC_thread) (p + offsetofThread (s));

    /* Find out the size needed for newThread */
    long int size = -1;
    GC_stack oriStk = (GC_stack) objptrToPointer (oriThrd->stack, s->heap->start);
    size = oriStk->used - startOffset;
    assert (size >=0);

    if (size == 0)
        printf("--------------------------SIZE IS 0---------------------------------\n");

    /* newThread () might do GC, so backup stuff */
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    assert (s->savedThread == BOGUS_OBJPTR);
    s->savedThread = pointerToObjptr((pointer)oriThrd - offsetofThread (s), s->heap->start);

    GC_thread th = newThread (s, size);
    assert (th);
    /* restore */
    oriThrd = (GC_thread)(objptrToPointer(s->savedThread, s->heap->start) + offsetofThread (s));
    s->savedThread = BOGUS_OBJPTR;

    GC_stack stk = (GC_stack) objptrToPointer (th->stack, s->heap->start);
    stk->isParasitic = TRUE;

    /* Find the limits of async */
    oriStk = (GC_stack) objptrToPointer (oriThrd->stack, s->heap->start);
    pointer start = getStackBottom (s, oriStk) + startOffset;
    pointer end = getStackBottom (s, oriStk) + oriStk->used;
    assert (start <= end);

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

    s->amInGC = TRUE; //For profiler safety
    memcpy (dest, start, numBytes);
    s->amInGC = FALSE;

    /* set control to threadlet sitting below */
    oriStk->used = startOffset;

    return th;
}

void GC_prefixAndSwitchTo (GC_state s, pointer p) {
    /* TODO : get frameBottom by fixed offset */

    assert (s->atomicState > 0);
    assert (p);


    GC_thread thrd = (GC_thread)(p + offsetofThread (s));

    if (DEBUG_SPLICE)
        fprintf (stderr, "\nprefixAndSwitchTo [%d]\n", Proc_processorNumber (s));

    GC_stack stk = (GC_stack) objptrToPointer (thrd->stack, s->heap->start);

    int i=0;
    while (s->stackLimit < s->stackTop + stk->used) {
        s->cumulativeStatistics->numForceStackGrowth++;
        if (DEBUG_SPLICE) {
            fprintf (stderr, "\tGrowingStack\n");
            fprintf (stderr, "\t\tstackTop = "FMTPTR"\n", (uintptr_t)s->stackTop);
            fprintf (stderr, "\t\tstackLimit = "FMTPTR"\n", (uintptr_t)s->stackLimit);
            fprintf (stderr, "\t\tparasiteSize = %ld\n", stk->used);
        }

        /* XXX KC This is a temporary fix. Might break if parasite was
         * sufficiently large enough that target stack's reserved space is not
         * enough
         */
        if (i>0) {
          fprintf (stderr, "PREMATURE BREAKING\n");
          break;
        }

        /* sort of copied from sizeofStackGrowReserved */
        size_t reservedNew =
            (s->stackTop - s->stackBottom) //Currently used
            + stk->used //Parasite size
            + sizeofStackSlop (s); //Space for slop
        {
            const size_t RESERVED_MAX = (SIZE_MAX >> 2);
            double reservedD = (double)reservedNew;
            double reservedGrowD =
                (double)s->controls->ratios.stackCurrentGrow * reservedD;
            size_t reservedGrow =
                reservedGrowD > (double)RESERVED_MAX ?
                RESERVED_MAX : (size_t)reservedGrowD;
            reservedNew = alignStackReserved (s, reservedGrow);
            assert (isStackReservedAligned (s, reservedNew));
        }

        /* grow stack if needed */
        getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
        getThreadCurrent(s)->exnStack = s->exnStack;
        getThreadCurrent(s)->bytesNeeded = 0;

        assert (s->savedThread == BOGUS_OBJPTR);
        s->savedThread = pointerToObjptr((pointer)thrd - offsetofThread (s), s->heap->start);
        ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE,
                                                        TRUE, TRUE,
                                                        0, 0, FALSE, reservedNew);

        thrd = (GC_thread)(objptrToPointer(s->savedThread, s->heap->start) + offsetofThread (s));
        s->savedThread = BOGUS_OBJPTR;

        /* Assertions */
        stk = (GC_stack) objptrToPointer (thrd->stack, s->heap->start);
        i++;
    }

    pointer parasiteBottom = getStackBottom (s, stk);
    pointer start = GC_getFrameBottom ();

    s->amInGC = TRUE; //For profiler safety
    memcpy (start, parasiteBottom, stk->used);
    s->amInGC = FALSE;

    s->stackTop = start + stk->used;

    GC_thread curThread = getThreadCurrent (s);
    //Restore exception state
    curThread->exnStack = (start - s->stackBottom) + thrd->exnStack;
    s->exnStack = curThread->exnStack;

    if (DEBUG_SPLICE) {
        fprintf (stderr, "\tprefixing frame of size %ld\n", stk->used);
        fprintf (stderr, "\tnewStackTop = "FMTPTR"\n", (uintptr_t) s->stackTop);
        fflush (stderr);
    }


    s->atomicState--;
    return;
}

void GC_parasiteCreatedEvent (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->cumulativeStatistics->numParasitesCreated++;
}
