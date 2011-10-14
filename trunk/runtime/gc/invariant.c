/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void assertIsObjptrInFromSpaceOrLifted (GC_state s, objptr *opp) {
  objptr op = *opp;

  GC_header h = getHeader (objptrToPointer (op, s->heap->start));
  if (h == GC_FORWARDED) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "assertIsObjptrInFromSpaceOrLifted: forwarding [%d]\n", s->procId);
    fixFwdObjptr (s, opp);
    op = *opp;
  }

  assert (isObjptrInFromSpace (s, s->heap, *opp) || isObjptrInHeap (s, s->sharedHeap, *opp));
  unless (isObjptrInFromSpace (s, s->heap, *opp) || isObjptrInHeap (s, s->sharedHeap, *opp))
    die ("gc.c: assertIsObjptrInFromSpaceOrLifted "
         "opp = "FMTPTR"  "
         "*opp = "FMTOBJPTR"\n",
         (uintptr_t)opp, *opp);
}

void assertIsObjptrInFromSpace (GC_state s, objptr *opp) {
  assert (isObjptrInFromSpace (s, s->heap, *opp));
  unless (isObjptrInFromSpace (s, s->heap, *opp))
    die ("gc.c: assertIsObjptrInFromSpace "
         "opp = "FMTPTR"  "
         "*opp = "FMTOBJPTR"\n",
         (uintptr_t)opp, *opp);
  /* The following checks that intergenerational pointers have the
   * appropriate card marked.  Unfortunately, it doesn't work because
   * for stacks, the card containing the beginning of the stack is
   * marked, but any remaining cards aren't.
   */
  if (FALSE and s->mutatorMarksCards
      and isPointerInOldGen (s, s->heap, (pointer)opp)
      and isObjptrInNursery (s, s->heap, *opp)
      and not isCardMarked (s, (pointer)opp)) {
    displayGCState (s, stderr);
    die ("gc.c: intergenerational pointer from "FMTPTR" to "FMTOBJPTR" with unmarked card.\n",
         (uintptr_t)opp, *opp);
  }
}

#if ASSERT
bool invariantForGC (GC_state s) {
  if (DEBUG)
    fprintf (stderr, "invariantForGC [%d]\n",
             s->procId);
  /* Frame layouts */
  for (unsigned int i = 0; i < s->frameLayoutsLength; ++i) {
    GC_frameLayout layout;

    layout = &(s->frameLayouts[i]);
    if (layout->size > 0) {
      GC_frameOffsets offsets;

      assert (layout->size <= s->maxFrameSize);
      offsets = layout->offsets;
    }
  }
  /* Generational */
  if (s->mutatorMarksCards) {
    assert (s->generationalMaps.cardMap ==
            &(s->generationalMaps.cardMapAbsolute
              [pointerToCardMapIndexAbsolute(s->heap->start)]));
    assert (&(s->generationalMaps.cardMapAbsolute
              [pointerToCardMapIndexAbsolute(s->heap->start + s->heap->size - 1)])
            < (s->generationalMaps.cardMap
               + (s->generationalMaps.cardMapLength * CARD_MAP_ELEM_SIZE)));
  }
  assert (isAligned (s->heap->size, s->sysvals.pageSize));
  assert (isAligned ((size_t)s->heap->start, CARD_SIZE));
  assert (isFrontierAligned (s, s->heap->start + s->heap->oldGenSize));
  assert (isFrontierAligned (s, s->heap->nursery));
  assert (isFrontierAligned (s, s->frontier));
  assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
  assert (s->heap->nursery <= s->heap->start + s->heap->size);
  assert (s->heap->nursery <= s->heap->start + s->heap->availableSize);
  assert (s->heap->nursery <= s->frontier or 0 == s->frontier);
  assert (s->heap->availableSize <= s->heap->size);
  assert (s->start <= s->frontier);
  unless (0 == s->heap->size or 0 == s->frontier) {
    assert (s->frontier <= s->limitPlusSlop);
    assert ((s->limit == 0) or (s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP));
    assert (hasHeapBytesFree (s, s->heap, 0, 0));
  }
  assert (s->secondaryLocalHeap->start == NULL
          or s->heap->size == s->secondaryLocalHeap->size);
  /* Check that all pointers are into from space. */
  foreachGlobalObjptrInScope (s, assertIsObjptrInFromSpaceOrLifted);
  pointer back = s->heap->start + s->heap->oldGenSize;
  if (DEBUG)
    fprintf (stderr, "Checking old generation. [%d]\n", s->procId);
  foreachObjptrInRange (s, alignFrontier (s, s->heap->start), &back,
                        assertIsObjptrInFromSpaceOrLifted, FALSE);
  if (DEBUG)
      fprintf (stderr, "Checking nursery(1). nursery="FMTPTR" frontier="FMTPTR" [%d]\n",
                (uintptr_t)s->heap->nursery, (uintptr_t)s->frontier, s->procId);
  foreachObjptrInRange (s, s->heap->nursery, &s->frontier,
                        assertIsObjptrInFromSpaceOrLifted, FALSE);
  if (DEBUG)
      fprintf (stderr, "Checking nursery(2). [%d]\n", s->procId);
  if (DEBUG)
    fprintf (stderr, "Checking sharedHeap(1). sharedStart = "FMTPTR" sharedFrontier = "FMTPTR" [%d]\n",
              (uintptr_t)s->sharedStart, (uintptr_t)s->sharedFrontier, s->procId);
  foreachObjptrInRange (s, s->sharedStart, &s->sharedFrontier, assertLiftedObjptr, FALSE);
  if (DEBUG)
    fprintf (stderr, "Checking sharedHeap(2). [%d]\n", s->procId);
 /* Current thread. */
  GC_stack stack = getStackCurrent(s);
  assert (isStackReservedAligned (s, stack->reserved));
  assert (s->stackBottom == getStackBottom (s, stack));
  assert (s->stackTop == getStackTop (s, stack));
  assert (s->stackLimit == getStackLimit (s, stack));
  assert (s->stackBottom <= s->stackTop);
  assert (stack->used == sizeofGCStateCurrentStackUsed (s));
  assert (stack->used <= stack->reserved);
  if (DEBUG)
    fprintf (stderr, "invariantForGC passed [%d]\n",
             s->procId);
  return TRUE;
}
#endif

bool invariantForMutatorFrontier (GC_state s) {
  GC_thread thread = getThreadCurrent(s);
  return (thread->bytesNeeded
          <= (size_t)(s->limitPlusSlop - s->frontier));
}

bool invariantForMutatorStack (GC_state s) {
  GC_stack stack = getStackCurrent(s);
  return (getStackTop (s, stack)
          <= getStackLimit (s, stack) + getStackTopFrameSize (s, stack));
}

#if ASSERT
bool invariantForMutator (GC_state s, bool frontier, bool stack) {
  if (DEBUG)
    displayGCState (s, stderr);
  if (frontier)
    assert (invariantForMutatorFrontier(s));
  if (stack)
    assert (invariantForMutatorStack(s));
  assert (invariantForGC (s));
  return TRUE;
}
#endif
