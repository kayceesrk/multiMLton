/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool isPointerInHeap (__attribute__ ((unused)) GC_state s, GC_heap h, pointer p) {
  return (not (isPointer (p))
          or (h->start <= p
              and p < h->start + h->size));
}

bool isPointerInOldGen (GC_state s, GC_heap h, pointer p) {
  return (not (isPointer (p))
          or (h->kind==SHARED_HEAP ?
              (s->sharedHeap->start <= p and
                    p < s->sharedHeap->start + s->sharedHeap->oldGenSize) :
              (s->heap->start <= p and
                    p < s->heap->start + s->heap->oldGenSize)));
}

bool isPointerInNursery (GC_state s, GC_heap h, pointer p) {
  return (not (isPointer (p))
          or (h->kind==SHARED_HEAP ?
             (s->sharedHeap->nursery <= p and p < s->sharedHeap->frontier) :
             (s->heap->nursery <= p and p < s->heap->frontier)));
}

bool isPointerInFromSpace (GC_state s, GC_heap h, pointer p) {
  return (isPointerInOldGen (s, h, p)
          or isPointerInNursery (s, h, p));
}

bool isObjptrInHeap (GC_state s, GC_heap h, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, h->start);
  return isPointerInHeap (s, h, p);
}

bool isObjptrInOldGen (GC_state s, GC_heap h, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap->start);
  return isPointerInOldGen (s, h, p);
}

bool isObjptrInNursery (GC_state s, GC_heap h, objptr op) {
  pointer p;
  if (not (isObjptr(op)))
    return TRUE;
  p = objptrToPointer (op, s->heap->start);
  return isPointerInNursery (s, h, p);
}

bool isObjptrInFromSpace (GC_state s, GC_heap h, objptr op) {
  return (isObjptrInOldGen (s, h, op)
          or isObjptrInNursery (s, h, op));
}

/* Is there space in the heap for "oldGen" additional bytes;
  also, can "nursery" bytes be allocated by the current thread
  without using/claiming any shared resources */
bool hasHeapBytesFree (GC_state s, GC_heap h, size_t oldGen, size_t nursery) {
  size_t total;
  bool res;

  if (h->kind == LOCAL_HEAP) {
    total = s->heap->oldGenSize + oldGen + (s->canMinor ? 2 : 1) * (s->frontier - s->heap->nursery);
    res = (total <= s->heap->size) and
      (s->heap->start + s->heap->oldGenSize + oldGen <= s->heap->nursery) and
      (nursery <= (size_t)(s->limitPlusSlop - s->frontier));
    if (DEBUG_DETAILED)
      fprintf (stderr, "%s = hasBytesFree in localHeap (%s, %s) [%d]\n",
               boolToString (res),
               uintmaxToCommaString(oldGen),
               uintmaxToCommaString(nursery),
               s->procId);
  }
  else {
    total = s->sharedHeap->oldGenSize + oldGen + (s->canMinor ? 2 : 1) * (s->sharedHeap->frontier - s->sharedHeap->nursery);
    res = (total <= s->sharedHeap->availableSize) and
          (s->sharedHeap->start + s->sharedHeap->oldGenSize + oldGen <= s->sharedHeap->nursery) and
          (nursery <= (size_t)(s->sharedLimitPlusSlop - s->sharedFrontier));
    if (DEBUG_DETAILED)
      fprintf (stderr, "%s = hasBytesFree in sharedHeap (%s, %s)\n",
              boolToString (res),
              uintmaxToCommaString(oldGen),
              uintmaxToCommaString(nursery));
  }
  return res;
}

bool isHeapInit (GC_heap h) {
  return (0 == h->size);
}

/* isObjectLifted (GC_header header)
 *
 * Returns true if the object belongs to shared heap
 */
static inline bool isObjectLifted (GC_header header) {
    return (not (header == GC_FORWARDED)
            && (header & LIFT_MASK));
}


bool isPointerInAnyLocalHeap (GC_state s, pointer p) {
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    GC_state r = &s->procStates[proc];
    if (isPointerInHeap (r, r->heap, p))
      return TRUE;
  }
  return FALSE;
}
