/* Copyright (C) 2005-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void displayHeap (__attribute__ ((unused)) GC_state s,
                  GC_heap heap,
                  FILE *stream) {
  fprintf(stream,
          "\t\tnursery = "FMTPTR"\n"
          "\t\toldGenSize = %"PRIuMAX"\n"
          "\t\tsize = %"PRIuMAX"\n"
          "\t\tavailableSize = %zu\n"
          "\t\tstart = "FMTPTR"\n"
          "\t\tfrontier = "FMTPTR"\n"
          "\t\twithMapsSize = %"PRIuMAX"\n",
          (uintptr_t)heap->nursery,
          (uintmax_t)heap->oldGenSize,
          (uintmax_t)heap->size,
          (uintmax_t)heap->availableSize,
          (uintptr_t)heap->start,
          (uintptr_t)heap->frontier,
          (uintmax_t)heap->withMapsSize);
}


void initHeap (__attribute__ ((unused)) GC_state s,
               GC_heap h, GC_heapKind kind) {
  h->kind = kind;
  h->nursery = NULL;
  h->oldGenSize = 0;
  h->size = 0;
  h->availableSize = h->size;
  h->start = NULL;
  h->frontier = NULL;
  h->withMapsSize = 0;
}

/* sizeofHeapDesired (s, l, cs, kind)
 *
 * returns the desired heap size for a heap with l bytes live,
 * given that the current heap size is cs and the gc kind is kind.
 */
size_t sizeofHeapDesired (GC_state s, size_t liveSize, size_t currentSize, GC_heapKind kind) {
  size_t liveWithMapsSize;
  size_t res;
  float withMapsRatio;
  uintmax_t maxHeap;

  if (kind == LOCAL_HEAP)
    maxHeap = s->controls->maxHeapLocal;
  else
    maxHeap = s->controls->maxHeapShared;

  liveSize = align (liveSize, s->sysvals.pageSize);
  liveWithMapsSize = liveSize + sizeofCardMapAndCrossMap (s, liveSize);

  withMapsRatio = (float)s->sysvals.ram / (float)liveWithMapsSize;
  if (withMapsRatio >= s->controls->ratios.live + s->controls->ratios.grow) {
    /* Cheney copying fits in RAM with desired ratios.live. */
    res = liveSize * s->controls->ratios.live;
    /* If the heap is currently close in size to what we want, leave
     * it alone.  Favor growing over shrinking.
     */
    unless (1.1 * currentSize <= res
            or res <= .5 * currentSize)
      res = currentSize;
  } else if (s->controls->ratios.grow >= s->controls->ratios.copy
             and withMapsRatio >= 2 * s->controls->ratios.copy) {
    /* Split RAM in half.  Round down by pageSize so that the total
     * amount of space taken isn't greater than RAM once rounding
     * happens.  This is so resizeLocalHeapSecondary doesn't get confused
     * and free a semispace in a misguided attempt to avoid paging.
     */
    res = alignDown (s->sysvals.ram / 2, s->sysvals.pageSize);
  } else if (withMapsRatio >= s->controls->ratios.copy + s->controls->ratios.grow) {
    /* Cheney copying fits in RAM. */
    res = s->sysvals.ram - s->controls->ratios.grow * liveSize;
    /* If the heap isn't too much smaller than what we want, leave it
     * alone.  On the other hand, if it is bigger we want to leave res
     * as is so that the heap is shrunk, to try to avoid paging.
     */
    if (currentSize <= res
        and res <= 1.1 * currentSize)
      res = currentSize;
  } else if (withMapsRatio >= s->controls->ratios.markCompact) {
    /* Mark compact fits in RAM.  It doesn't matter what the current
     * size is.  If the heap is currently smaller, we are using
     * copying and should switch to mark-compact.  If the heap is
     * currently bigger, we want to shrink back to RAM to avoid
     * paging.
     */
    res = s->sysvals.ram;
  } else { /* Required live ratio. */
    res = liveSize * s->controls->ratios.markCompact;
    /* If the current heap is bigger than res, then shrinking always
     * sounds like a good idea.  However, depending on what pages the
     * VM keeps around, growing could be very expensive, if it
     * involves paging the entire heap.  Hopefully the copy loop in
     * growHeap will make the right thing happen.
     */
  }
  if (s->controls->fixedHeap > 0) {
    if (res > s->controls->fixedHeap / 2)
      res = s->controls->fixedHeap;
    else
      res = s->controls->fixedHeap / 2;
    if (res < liveSize) {
      assert (0 and "Out of memory with fixed heap.");
      die ("Out of memory with fixed heap size %s.",
           uintmaxToCommaString(s->controls->fixedHeap));
    }
  } else if (maxHeap > 0) {
    if (res > maxHeap)
      res = maxHeap;
    if (res < liveSize)
      die ("Out of memory with max heap size %s.", uintmaxToCommaString(maxHeap));
  }

  char kindStr[25];
  if (kind == LOCAL_HEAP)
    strcpy (kindStr, "LOCAL_HEAP");
  else
    strcpy (kindStr, "SHARED_HEAP");

  if (DEBUG_RESIZING)
    fprintf (stderr, "%s = sizeofHeapDesired (%s, %s, %s)\n",
             uintmaxToCommaString(res),
             uintmaxToCommaString(liveSize),
             uintmaxToCommaString(currentSize),
             kindStr);
  assert (res >= liveSize);
  return res;
}

void releaseHeap (GC_state s, GC_heap h) {
  if (NULL == h->start)
    return;
  if (DEBUG or s->controls->messages)
    fprintf (stderr,
             "[GC: Releasing heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map).]\n",
             (uintptr_t)(h->start),
             uintmaxToCommaString(h->size),
             uintmaxToCommaString(h->withMapsSize - h->size));
  GC_release (h->start, h->withMapsSize);
  initHeap (s, h, h->kind);
}

/* shrinkHeap (s, h, keepSize)
 */
void shrinkHeap (GC_state s, GC_heap h, size_t keepSize) {
  assert (keepSize <= h->size);
  if (0 == keepSize) {
    releaseHeap (s, h);
    return;
  }
  keepSize = align (keepSize, s->sysvals.pageSize);
  if (keepSize < h->size) {
    size_t keepWithMapsSize;
    keepWithMapsSize = keepSize + sizeofCardMapAndCrossMap (s, keepSize);
    if (DEBUG or s->controls->messages) {
      fprintf (stderr,
               "[GC: Shrinking heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map),]\n",
               (uintptr_t)(h->start),
               uintmaxToCommaString(h->size),
               uintmaxToCommaString(h->withMapsSize - h->size));
      fprintf (stderr,
               "[GC:\tto size %s bytes (+ %s bytes card/cross map).]\n",
               uintmaxToCommaString(keepSize),
               uintmaxToCommaString(keepWithMapsSize - keepSize));
    }
    assert (keepWithMapsSize <= h->withMapsSize);
    GC_release (h->start + keepWithMapsSize, h->withMapsSize - keepWithMapsSize);
    h->size = keepSize;
    h->withMapsSize = keepWithMapsSize;
  }
}

/* createHeap (s, h, desiredSize, minSize)
 *
 * allocates a heap of the size necessary to work with desiredSize
 * live data, and ensures that at least minSize is available.  It
 * returns TRUE if it is able to allocate the space, and returns FALSE
 * if it is unable.
 */
bool createHeap (GC_state s, GC_heap h,
                 size_t desiredSize,
                 size_t minSize) {
  size_t backoff;
  size_t newSize;
  size_t newWithMapsSize;
  bool isShared = FALSE;

  if (h == s->sharedHeap || h == s->secondarySharedHeap)
    isShared = TRUE;
  else
    isShared = FALSE;

  if (DEBUG_MEM)
    fprintf (stderr, "createHeap  desired size = %s  min size = %s\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(minSize));
  assert (isHeapInit (h));
  if (desiredSize < minSize)
    desiredSize = minSize;
  minSize = align (minSize, s->sysvals.pageSize);
  desiredSize = align (desiredSize, s->sysvals.pageSize);
  assert (0 == h->size and NULL == h->start);
  backoff = (desiredSize - minSize) / 16;
  if (0 == backoff)
    backoff = 1; /* enough to terminate the loop below */
  backoff = align (backoff, s->sysvals.pageSize);
  /* mmap toggling back and forth between high and low addresses to
   * decrease the chance of virtual memory fragmentation causing an mmap
   * to fail.  This is important for large heaps.
   * Note that the loop always trys a NULL address last.
   */
  newSize = desiredSize;
  do {
    const unsigned int countLog2 = 5;
    const unsigned int count = 0x1 << countLog2;
    const size_t step = (size_t)0x1 << (ADDRESS_BITS - countLog2);
#if ADDRESS_BITS == POINTER_BITS
    const size_t address_end = 0;
#else
    const size_t address_end = (size_t)0x1 << ADDRESS_BITS;
#endif

    bool direction = TRUE;
    unsigned int i;

    newWithMapsSize = newSize + sizeofCardMapAndCrossMap (s, newSize);

    assert (isAligned (newWithMapsSize, s->sysvals.pageSize));

    for (i = 1; i <= count; i++) {
      size_t address;
      pointer newStart;

      address = (size_t)i * step;
      if (direction)
        address = address_end - address;
      /* Always use 0 in the last step. */
      if (i == count)
        address = 0;

      newStart = GC_mmapAnon ((pointer)address, newWithMapsSize);
      unless ((void*)-1 == newStart) {
        direction = not direction;
        h->start = newStart;
        h->size = newSize;
        h->withMapsSize = newWithMapsSize;
        if (!isShared && h->size > s->cumulativeStatistics->maxHeapSize)
          s->cumulativeStatistics->maxHeapSize = h->size;
        if (isShared && h->size > s->cumulativeStatistics->maxSharedHeapSize)
          s->cumulativeStatistics->maxSharedHeapSize = h->size;
        assert (minSize <= h->size and h->size <= desiredSize);
        if (DEBUG or s->controls->messages)
          fprintf (stderr,
                   "[GC: Created heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map).]\n",
                   (uintptr_t)(h->start),
                   uintmaxToCommaString(h->size),
                   uintmaxToCommaString(h->withMapsSize - h->size));
        return TRUE;
      }
    }
    if (s->controls->messages) {
      fprintf (stderr,
               "[GC: Creating heap of size %s bytes (+ %s bytes card/cross map) cannot be satisfied,]\n",
               uintmaxToCommaString (newSize),
               uintmaxToCommaString (newWithMapsSize - newSize));
      fprintf (stderr,
               "[GC:\tbacking off by %s bytes with minimum size of %s bytes.]\n",
               uintmaxToCommaString (backoff),
               uintmaxToCommaString (minSize));
    }
    size_t nextSize = newSize - backoff;
    if (nextSize < minSize and minSize < newSize) {
      newSize = minSize;
    } else {
      newSize = nextSize;
    }
  } while (newSize >= minSize);
  return FALSE;
}

/* createHeapSecondary (s, desiredSize)
 */
bool createHeapSecondary (GC_state s, size_t desiredSize) {
  if ((s->controls->fixedHeap > 0
       and s->heap->size + desiredSize > s->controls->fixedHeap)
      or (s->controls->maxHeapLocal > 0
          and s->heap->size + desiredSize > s->controls->maxHeapLocal))
    return FALSE;
  return createHeap (s, s->secondaryLocalHeap, desiredSize, s->heap->oldGenSize);
}

bool createSharedHeapSecondary (GC_state s, size_t desiredSize) {
  if ((s->controls->fixedHeap > 0
       and s->sharedHeap->size + desiredSize > s->controls->fixedHeap)
      or (s->controls->maxHeapShared > 0
          and s->sharedHeap->size + desiredSize > s->controls->maxHeapShared))
    return FALSE;
  return createHeap (s, s->secondarySharedHeap,
                     desiredSize, s->sharedHeap->oldGenSize);
}

/* remapHeap (s, h, desiredSize, minSize)
 */
bool remapHeap (GC_state s, GC_heap h,
                size_t desiredSize,
                size_t minSize) {
  size_t backoff;
  size_t newSize;
  size_t newWithMapsSize;
  size_t origSize;
  size_t origWithMapsSize;
  bool isShared = FALSE;

  if (h == s->sharedHeap || h == s->secondarySharedHeap)
    isShared = TRUE;

#if not HAS_REMAP
  return FALSE;
#endif
  if (DEBUG_MEM)
    fprintf (stderr, "remapHeap  desired size = %s  min size = %s\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(minSize));
  assert (minSize <= desiredSize);
  assert (desiredSize >= h->size);
  minSize = align (minSize, s->sysvals.pageSize);
  desiredSize = align (desiredSize, s->sysvals.pageSize);
  backoff = (desiredSize - minSize) / 16;
  if (0 == backoff)
    backoff = 1; /* enough to terminate the loop below */
  backoff = align (backoff, s->sysvals.pageSize);
  origSize = h->size;
  origWithMapsSize = origSize + sizeofCardMapAndCrossMap (s, origSize);
  newSize = desiredSize;
  do {
    pointer newStart;

    newWithMapsSize = newSize + sizeofCardMapAndCrossMap (s, newSize);

    assert (isAligned (newWithMapsSize, s->sysvals.pageSize));

    newStart = GC_mremap (h->start, origWithMapsSize, newWithMapsSize);
    unless ((void*)-1 == newStart) {
      h->start = newStart;
      h->size = newSize;
      h->withMapsSize = newWithMapsSize;
      if (!isShared && h->size > s->cumulativeStatistics->maxHeapSize)
        s->cumulativeStatistics->maxHeapSize = h->size;
      if (isShared && h->size > s->cumulativeStatistics->maxSharedHeapSize)
        s->cumulativeStatistics->maxSharedHeapSize = h->size;
      assert (minSize <= h->size and h->size <= desiredSize);
      if (DEBUG or s->controls->messages)
        fprintf (stderr,
                 "[GC: Remapped heap at "FMTPTR" to size %s bytes (+ %s bytes card/cross map).]\n",
                 (uintptr_t)(h->start),
                 uintmaxToCommaString(h->size),
                 uintmaxToCommaString(h->withMapsSize - h->size));
      return TRUE;
    }
    if (s->controls->messages) {
      fprintf (stderr,
               "[GC: Remapping heap to size %s bytes (+ %s bytes card/cross map) cannot be satisfied,]\n",
               uintmaxToCommaString (newSize),
               uintmaxToCommaString (newWithMapsSize - newSize));
      fprintf (stderr,
               "[GC:\tbacking off by %s bytes with minimum size of %s bytes.]\n",
               uintmaxToCommaString (backoff),
               uintmaxToCommaString (minSize));
    }
    size_t nextSize = newSize - backoff;
    if (nextSize < minSize and minSize < newSize) {
      newSize = minSize;
    } else {
      newSize = nextSize;
    }
  } while (newSize >= minSize);
  return FALSE;
}

enum {
  COPY_CHUNK_SIZE = 0x2000000, /* 32M */
};

/* growHeap (s, desiredSize, minSize)
 */
void growHeap (GC_state s, GC_heap h, size_t desiredSize, size_t minSize) {
  GC_heap curHeapp;
  struct GC_heap newHeap;
  GC_heap newHeapp;

  bool useCurrent;
  pointer origStart;
  size_t liveSize;

  bool isShared = FALSE;

  char str[20];
  sprintf (str, "local");
  if (h == s->sharedHeap) {
    isShared = TRUE;
    sprintf (str, "shared");
  }

  assert (desiredSize >= h->size);
  if (DEBUG_RESIZING or s->controls->messages) {
    fprintf (stderr,
             "[GC: Growing %s heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map),]\n",
             str,
             (uintptr_t)h->start,
             uintmaxToCommaString(h->size),
             uintmaxToCommaString(h->withMapsSize - h->size));
    fprintf (stderr,
             "[GC:\tto desired size of %s bytes and minimum size of %s bytes.]\n",
             uintmaxToCommaString(desiredSize),
             uintmaxToCommaString(minSize));
  }
  if (minSize <= h->size) {
    useCurrent = TRUE;
    /* Demand real growth from remapHeap and/or createHeap. */
    minSize = (desiredSize / 2) + (h->size / 2);
  } else {
    useCurrent = FALSE;
  }
  curHeapp = h;
  newHeapp = &newHeap;
  origStart = curHeapp->start;
  liveSize = curHeapp->oldGenSize;
  assert (liveSize <= curHeapp->size);
  if (remapHeap (s, curHeapp, desiredSize, minSize)) {
    goto done;
  }
  if (!useCurrent)
    shrinkHeap (s, curHeapp, liveSize);
  initHeap (s, newHeapp, curHeapp->kind);
  /* Allocate a space of the desired size. */
  if (createHeap (s, newHeapp, desiredSize, minSize)) {
    pointer from;
    pointer to;
    size_t remaining;

    from = curHeapp->start + liveSize;
    to = newHeapp->start + liveSize;
    remaining = liveSize;
    shrinkHeap (s, curHeapp, remaining);
copy:
    assert (remaining == (size_t)(from - curHeapp->start)
            and from >= curHeapp->start
            and to >= newHeapp->start);
    if (remaining < COPY_CHUNK_SIZE) {
      GC_memcpy (curHeapp->start, newHeapp->start, remaining);
      releaseHeap (s, curHeapp);
    } else {
      remaining -= COPY_CHUNK_SIZE;
      from -= COPY_CHUNK_SIZE;
      to -= COPY_CHUNK_SIZE;
      GC_memcpy (from, to, COPY_CHUNK_SIZE);
      shrinkHeap (s, curHeapp, remaining);
      goto copy;
    }
    newHeapp->oldGenSize = liveSize;
    *curHeapp = *newHeapp;
  } else if (useCurrent) {
    if (DEBUG_RESIZING or s->controls->messages) {
      fprintf (stderr,
               "[GC: Using heap at "FMTPTR" of size %s bytes (+ %s bytes card/cross map).]\n",
               (uintptr_t)h->start,
               uintmaxToCommaString(h->size),
               uintmaxToCommaString(h->withMapsSize - h->size));
    }
  } else if (s->controls->mayPageHeap) {
    /* Page the heap to disk and try again. */
    void *data;

    if (DEBUG or s->controls->messages) {
      fprintf (stderr,
               "[GC: Writing %s bytes of heap at "FMTPTR" to disk.]\n",
               uintmaxToCommaString(liveSize),
               (uintptr_t)curHeapp->start);
    }
    data = GC_diskBack_write (curHeapp->start, liveSize);
    releaseHeap (s, curHeapp);
    if (createHeap (s, curHeapp, desiredSize, minSize)) {
      if (DEBUG or s->controls->messages) {
        fprintf (stderr,
                 "[GC: Reading %s bytes of heap to "FMTPTR" from disk.]\n",
                 uintmaxToCommaString(liveSize),
                 (uintptr_t)(curHeapp->start));
      }
      GC_diskBack_read (data, curHeapp->start, liveSize);
      GC_diskBack_close (data);
      curHeapp->oldGenSize = liveSize;
    } else {
      GC_diskBack_close (data);
      if (s->controls->messages)
        GC_displayMem ();
      die ("Out of memory.  Unable to allocate heap with %s bytes.\n",
           uintmaxToCommaString(minSize));
    }
  } else {
    if (s->controls->messages)
      GC_displayMem ();
    die ("Out of memory.  Unable to allocate heap with %s bytes.\n",
         uintmaxToCommaString(minSize));
  }
done:
  unless (origStart == h->start) {
    if (!isShared)
      translateHeap (s, origStart, h->start, h->oldGenSize);
    else
      translateSharedHeap (s, origStart, h->start, h->oldGenSize);
  }
}

/* resizeHeap (s, h, minSize)
 */
void resizeHeap (GC_state s, GC_heap h, size_t minSize) {
  size_t desiredSize;
  bool isShared = FALSE;

  if (h == s->sharedHeap)
    isShared = TRUE;

  if (DEBUG_RESIZING)
    fprintf (stderr, "resizeHeap  minSize = %s  size = %s isShared=%d\n",
             uintmaxToCommaString(minSize),
             uintmaxToCommaString(h->size),
             isShared);
  if (isShared)
    desiredSize = sizeofHeapDesired (s, minSize, h->size, SHARED_HEAP);
  else
    desiredSize = sizeofHeapDesired (s, minSize, h->size, LOCAL_HEAP);

  assert (minSize <= desiredSize);
  if (desiredSize <= h->size) {
    shrinkHeap (s, h, desiredSize);
  } else {
    if (isShared)
      releaseHeap (s, s->secondarySharedHeap);
    else
      releaseHeap (s, s->secondaryLocalHeap);
    growHeap (s, h, desiredSize, minSize);
  }
  assert (h->size >= minSize);
}

/* resizeLocalHeapSecondary (s)
 */
void resizeLocalHeapSecondary (GC_state s) {
  size_t primarySize;
  size_t secondarySize;

  primarySize = s->heap->size;
  secondarySize = s->secondaryLocalHeap->size;
  if (DEBUG_RESIZING)
    fprintf (stderr, "secondaryLocalHeapResize\n");
  if (0 == secondarySize)
    return;
  if (2 * primarySize > s->sysvals.ram)
    /* Holding on to secondaryLocalHeap might cause paging.  So don't. */
    releaseHeap (s, s->secondaryLocalHeap);
  else if (secondarySize < primarySize) {
    unless (remapHeap (s, s->secondaryLocalHeap, primarySize, primarySize))
      releaseHeap (s, s->secondaryLocalHeap);
  } else if (secondarySize > primarySize)
    shrinkHeap (s, s->secondaryLocalHeap, primarySize);
  assert (0 == s->secondaryLocalHeap->size
          or s->heap->size == s->secondaryLocalHeap->size);
}

/* resizeSharedHeapSecondary (s)
 */
void resizeSharedHeapSecondary (GC_state s, size_t primarySize) {
  size_t secondarySize;

  secondarySize = s->secondarySharedHeap->size;
  if (DEBUG_RESIZING)
    fprintf (stderr, "secondarySharedHeapResize\n");
  if (0 == secondarySize)
    return;
  if (2 * primarySize > s->sysvals.ram)
    /* Holding on to secondarySharedHeap might cause paging.  So don't. */
    releaseHeap (s, s->secondarySharedHeap);
  else if (secondarySize < primarySize) {
    unless (remapHeap (s, s->secondarySharedHeap, primarySize, primarySize))
      releaseHeap (s, s->secondarySharedHeap);
  } else if (secondarySize > primarySize)
    shrinkHeap (s, s->secondarySharedHeap, primarySize);
  assert (0 == s->secondarySharedHeap->size
          or primarySize == s->secondarySharedHeap->size);
}
