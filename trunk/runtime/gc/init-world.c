/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

size_t sizeofIntInfFromString (GC_state s, const char *str) {
  size_t slen = strlen (str);

  /* A slight overestimate. */
  double bytesPerChar = 0.415241011861 /* = ((log(10.0) / log(2.0)) / 8.0) */ ;
  double bytes = ceil((double)slen * bytesPerChar);
  return align (GC_ARRAY_HEADER_SIZE
                + sizeof(mp_limb_t) // for the sign
                + align((size_t)bytes, sizeof(mp_limb_t)),
                s->alignment);
}

size_t sizeofInitialBytesLive (GC_state s) {
  uint32_t i;
  size_t dataBytes;
  size_t total;

  total = 0;
  for (i = 0; i < s->intInfInitsLength; ++i) {
    total += sizeofIntInfFromString (s, s->intInfInits[i].mlstr);
  }
  for (i = 0; i < s->vectorInitsLength; ++i) {
    dataBytes =
      s->vectorInits[i].bytesPerElement
      * s->vectorInits[i].numElements;
    total += align (GC_ARRAY_HEADER_SIZE
                    + ((dataBytes < OBJPTR_SIZE)
                       ? OBJPTR_SIZE
                       : dataBytes),
                    s->alignment);
  }
  return total;
}

void initIntInfs (GC_state s) {
  struct GC_intInfInit *inits;
  uint32_t i;
  const char *str;
  size_t bytes;
  bool neg;
  __mpz_struct resmpz;
  int ans;

  assert (isFrontierAligned (s, s->frontier));
  for (i = 0; i < s->intInfInitsLength; i++) {
    inits = &(s->intInfInits[i]);
    assert (inits->globalIndex < s->globalsLength);
    str = inits->mlstr;
    bytes = sizeofIntInfFromString (s, str);
    neg = *str == '~';
    if (neg)
      str++;
    initIntInfRes (s, &resmpz, bytes);
    ans = mpz_set_str (&resmpz, str, 10);
    assert (ans == 0);
    if (neg)
      resmpz._mp_size = - resmpz._mp_size;
    s->globals[inits->globalIndex] = finiIntInfRes (s, &resmpz, bytes);
  }
  assert (isFrontierAligned (s, s->frontier));
}

void initVectors (GC_state s) {
  struct GC_vectorInit *inits;
  pointer frontier;
  uint32_t i;

  assert (isFrontierAligned (s, s->frontier));
  inits = s->vectorInits;
  frontier = s->frontier;
  for (i = 0; i < s->vectorInitsLength; i++) {
    size_t bytesPerElement;
    size_t dataBytes;
    size_t objectSize;
    uint32_t typeIndex;

    bytesPerElement = inits[i].bytesPerElement;
    dataBytes = bytesPerElement * inits[i].numElements;
    objectSize = align (GC_ARRAY_HEADER_SIZE
                        + ((dataBytes < OBJPTR_SIZE)
                           ? OBJPTR_SIZE
                           : dataBytes),
                        s->alignment);
    assert (objectSize <= (size_t)(s->heap->start + s->heap->size - frontier));
    *((GC_arrayCounter*)(frontier)) = 0;
    frontier = frontier + GC_ARRAY_COUNTER_SIZE;
    *((GC_arrayLength*)(frontier)) = inits[i].numElements;
    frontier = frontier + GC_ARRAY_LENGTH_SIZE;
    switch (bytesPerElement) {
    case 1:
      typeIndex = WORD8_VECTOR_TYPE_INDEX;
      break;
    case 2:
      typeIndex = WORD16_VECTOR_TYPE_INDEX;
      break;
    case 4:
      typeIndex = WORD32_VECTOR_TYPE_INDEX;
      break;
    case 8:
      typeIndex = WORD64_VECTOR_TYPE_INDEX;
      break;
    default:
      die ("unknown bytes per element in vectorInit: %"PRIuMAX"",
           (uintmax_t)bytesPerElement);
    }
    *((GC_header*)(frontier)) = buildHeaderFromTypeIndex (typeIndex);
    frontier = frontier + GC_HEADER_SIZE;
    s->globals[inits[i].globalIndex] = pointerToObjptr(frontier, s->heap->start);
    if (DEBUG_DETAILED)
      fprintf (stderr, "allocated vector at "FMTPTR"\n",
               (uintptr_t)(s->globals[inits[i].globalIndex]));
    memcpy (frontier, inits[i].bytes, dataBytes);
    frontier += objectSize - GC_ARRAY_HEADER_SIZE;
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "frontier after string allocation is "FMTPTR"\n",
             (uintptr_t)frontier);
  GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->cumulativeStatistics->bytesAllocated += (size_t)(frontier - s->frontier);
  assert (isFrontierAligned (s, frontier));
  s->frontier = frontier;
}

void initWorld (GC_state s) {
  uint32_t i;
  pointer start;
  GC_thread thread;
  size_t minSize;

  for (i = 0; i < s->globalsLength; ++i)
    s->globals[i] = BOGUS_OBJPTR;
  s->lastMajorStatistics->bytesLive = sizeofInitialBytesLive (s);
  minSize = s->lastMajorStatistics->bytesLive
    + ((GC_HEAP_LIMIT_SLOP + GC_BONUS_SLOP) * s->numberOfProcs);
  createHeap (s, s->heap,
              sizeofHeapDesired (s, minSize, 0),
              minSize);

  //set up local heap
  setCardMapAndCrossMap (s);
  start = alignFrontier (s, s->heap->start);
  s->start = s->frontier = start;
  s->limitPlusSlop = s->heap->start + s->heap->size - GC_BONUS_SLOP;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  initIntInfs (s);
  initVectors (s);
  assert ((size_t)(s->frontier - start) <= s->lastMajorStatistics->bytesLive);
  s->heap->oldGenSize = s->frontier - s->heap->start;
  setGCStateCurrentLocalHeap (s, 0, 0);

  //set up shared heap
  //Create an initial heap of size 10M
  createHeap (s, s->sharedHeap, 1024 * 1024 * 10, 1024 * 1024 * 10);
  start = alignFrontier (s, s->sharedHeap->start);
  s->start = s->frontier = start;
  s->limitPlusSlop = s->sharedHeap->start + s->sharedHeap->size - GC_BONUS_SLOP;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  s->sharedHeap->oldGenSize = s->frontier - s->sharedHeap->start;
  setGCStateCurrentSharedHeap (s, 0, 0, true);

  thread = newThread (s, sizeofStackInitialReserved (s));
  switchToThread (s, pointerToObjptr((pointer)thread - offsetofThread (s), s->heap->start));
}

void duplicateWorld (GC_state d, GC_state s) {
  GC_thread thread;
  pointer start;

  d->lastMajorStatistics->bytesLive = 0;
  //set up local heap
  d->heap = (GC_heap) malloc (sizeof (struct GC_heap));
  initHeap (d, d->heap);
  createHeap (d, d->heap, 8192, 8192);
  start = alignFrontier (d, d->heap->start);
  d->start = d->frontier = start;
  d->limitPlusSlop = d->heap->start + d->heap->size - GC_BONUS_SLOP;
  d->limit = d->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  d->heap->oldGenSize = d->frontier - d->heap->start;
  setGCStateCurrentLocalHeap (d, 0, 0);
  setCardMapAndCrossMap (d);

  //set up shared local heap
  d->secondaryLocalHeap = (GC_heap) malloc (sizeof (struct GC_heap));
  initHeap (d, d->secondaryLocalHeap);


  /* Use the original to allocate */
  //XXX KC SPH does this violate GC invariants
  thread = newThread (d, sizeofStackInitialReserved (d));

  /* Now copy stats, heap data from original */
  d->cumulativeStatistics->maxHeapSize = s->cumulativeStatistics->maxHeapSize;
  d->secondaryLocalHeap = s->secondaryLocalHeap;
  d->sharedHeap = s->sharedHeap;
  d->generationalMaps = s->generationalMaps;

  /* Allocation handled in setGCStateCurrentSharedHeap when called from initWorld */

  switchToThread (d, pointerToObjptr((pointer)thread - offsetofThread (d), d->heap->start));
}

