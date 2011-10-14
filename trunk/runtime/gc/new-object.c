/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* newObject (s, header, bytesRequested, allocInOldGen)
 *
 * Allocate a new object in the local heap->
 * bytesRequested includes the size of the header.
 */
pointer newObject (GC_state s,
                   GC_header header,
                   size_t bytesRequested,
                   bool allocInOldGen,
                   bool allocInSharedHeap) {
  pointer frontier;
  pointer result;

  assert (!allocInSharedHeap);
  assert (isAligned (bytesRequested, s->alignment));
  assert (allocInOldGen
          ? hasHeapBytesFree (s, s->heap, bytesRequested, 0)
          : hasHeapBytesFree (s, s->heap, 0, bytesRequested));
  if (allocInOldGen) {
    frontier = s->heap->start + s->heap->oldGenSize;
    s->heap->oldGenSize += bytesRequested;
    s->cumulativeStatistics->bytesAllocated += bytesRequested;
  }
  else if (allocInSharedHeap) {
    allocChunkInSharedHeap (s, bytesRequested);
    if (DEBUG_DETAILED)
      fprintf (stderr, "sharedFrontier changed from "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)s->sharedFrontier,
               (uintptr_t)(s->sharedFrontier + bytesRequested));
    frontier = s->sharedFrontier;
    s->sharedFrontier += bytesRequested;
  }
  else {
    if (DEBUG_DETAILED)
      fprintf (stderr, "frontier changed from "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)s->frontier,
               (uintptr_t)(s->frontier + bytesRequested));
    frontier = s->frontier;
    s->frontier += bytesRequested;
  }
  GC_profileAllocInc (s, bytesRequested);
  *((GC_header*)frontier) = header;
  result = frontier + GC_NORMAL_HEADER_SIZE;
  assert (isAligned ((size_t)result, s->alignment));
  if (DEBUG)
    fprintf (stderr, FMTPTR " = newObject ("FMTHDR", %"PRIuMAX", %s)\n",
             (uintptr_t)result,
             header,
             (uintmax_t)bytesRequested,
             boolToString (allocInOldGen));
  return result;
}

GC_stack newStack (GC_state s,
                   size_t reserved,
                   bool allocInOldGen,
                   bool allocInSharedHeap) {
  GC_stack stack;

  assert (!(allocInOldGen & allocInSharedHeap));

  assert (isStackReservedAligned (s, reserved));
  /* XXX unsafe concurrent access */
  if (reserved > s->cumulativeStatistics->maxStackSize)
    s->cumulativeStatistics->maxStackSize = reserved;

  /* Statistics */
  s->cumulativeStatistics->bytesThreadReserved += reserved;
  s->cumulativeStatistics->countThreadReserved++;
  s->cumulativeStatistics->bytesThreadUsed += reserved;
  s->cumulativeStatistics->countThreadUsed++;

  stack = (GC_stack)(newObject (s, GC_STACK_HEADER,
                                sizeofStackWithHeader (s, reserved),
                                allocInOldGen, allocInSharedHeap));
  stack->reserved = reserved;
  stack->used = 0;
  stack->thread = BOGUS_OBJPTR;
  stack->isParasitic = FALSE;
  if (DEBUG_STACKS)
    fprintf (stderr, FMTPTR " = newStack (%"PRIuMAX")\n",
             (uintptr_t)stack,
             (uintmax_t)reserved);
  return stack;
}

GC_thread newThread (GC_state s, size_t reserved) {
  GC_stack stack;
  GC_thread thread;
  pointer res;

  assert (isStackReservedAligned (s, reserved));
  ensureHasHeapBytesFreeAndOrInvariantForMutator
    (s, FALSE, FALSE, FALSE, 0,
     sizeofStackWithHeader (s, alignStackReserved (s, reserved)) + sizeofThread (s),
     FALSE, 0);
  stack = newStack (s, reserved, FALSE, FALSE);
  res = newObject (s, GC_THREAD_HEADER,
                   sizeofThread (s),
                   FALSE, FALSE);
  thread = (GC_thread)(res + offsetofThread (s));
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->stack = pointerToObjptr((pointer)stack, s->heap->start);
  stack->thread = pointerToObjptr ((pointer)thread, s->heap->start);
  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = newThreadOfSize (%"PRIuMAX")\n",
             (uintptr_t)thread, (uintmax_t)reserved);;
  return thread;
}

static inline void setFrontier (GC_state s, pointer p,
                                __attribute__ ((unused)) size_t bytes) {
  p = alignFrontier (s, p);
  assert ((size_t)(p - s->frontier) <= bytes);
  GC_profileAllocInc (s, p - s->frontier);
  /* XXX unsafe concurrent access */
  s->cumulativeStatistics->bytesAllocated += p - s->frontier;
  s->frontier = p;
  assert (s->frontier <= s->limitPlusSlop);
  assert (s->start <= s->frontier);
}

static inline void setSharedFrontier (GC_state s, pointer p,
                                __attribute__ ((unused)) size_t bytes) {
  p = alignFrontier (s, p);
  assert ((size_t)(p - s->sharedFrontier) <= bytes);
  GC_profileAllocInc (s, p - s->sharedFrontier);
  /* XXX unsafe concurrent access */
  s->cumulativeStatistics->bytesAllocated += p - s->sharedFrontier;
  s->sharedFrontier = p;
  assert (s->sharedFrontier <= s->sharedLimitPlusSlop);
  assert (s->sharedStart <= s->sharedFrontier);
}
