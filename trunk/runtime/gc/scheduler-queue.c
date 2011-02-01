/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


/**< Init Ciruclar Buffer */
static inline CircularBuffer* CircularBufferInit(CircularBuffer** pQue, int size) {
	int sz = size*sizeof(KeyType)+sizeof(CircularBuffer);
	*pQue = (CircularBuffer*) malloc(sz);
	if(*pQue)
	{
		(*pQue)->size=size;
		(*pQue)->writePointer = 0;
		(*pQue)->readPointer  = 0;
	}
	return *pQue;
}

static inline void CircularBufferClean (CircularBuffer* que) {
  que->readPointer = que->writePointer = 0;
}

static inline int CircularBufferIsFull(CircularBuffer* que) {
	return ((que->writePointer + 1) % que->size == que->readPointer);
}

static inline int CircularBufferIsEmpty(CircularBuffer* que) {
	return (que->readPointer == que->writePointer);
}

static inline int CircularBufferEnque(CircularBuffer* que, KeyType k) {
	int isFull = CircularBufferIsFull(que);
	que->keys[que->writePointer] = k;
	que->writePointer++;
	que->writePointer %= que->size;
	return isFull;
}

static inline int CircularBufferDeque(CircularBuffer* que, KeyType* pK) {
	int isEmpty = CircularBufferIsEmpty(que);
  if (isEmpty)
    return TRUE;
	*pK = que->keys[que->readPointer];
	que->readPointer++;
	que->readPointer %= que->size;
	return(FALSE);
}

static inline SchedulerQueue* newSchedulerQueue (void) {
  SchedulerQueue* sq = (SchedulerQueue*) malloc (sizeof (SchedulerQueue));
  CircularBufferInit (&sq->primary, BUFFER_SIZE);
  CircularBufferInit (&sq->secondary, BUFFER_SIZE);
  return sq;
}

static inline CircularBuffer* getSubQ (SchedulerQueue* sq, int i) {
  if (i==0)
    return sq->primary;
  return sq->secondary;
}

void GC_sqCreateQueues (GC_state s) {
  assert (s->procStates);
  Lock* schedulerLocks = (Lock*) malloc (sizeof(Lock) * s->numberOfProcs);
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    schedulerLocks[proc] = -1;
    s->procStates[proc].schedulerQueue = newSchedulerQueue ();
    s->procStates[proc].schedulerLocks = schedulerLocks;
  }
}

void GC_sqEnque (GC_state s, pointer p, int proc, int i) {
  if (DEBUG_SQ)
    fprintf (stderr, "GC_sqEnque p="FMTPTR" proc=%d q=%d [%d]\n",
             (uintptr_t)p, proc, i, s->procId);

  assert (p);
  GC_state fromProc = &s->procStates[proc];
  objptr op = pointerToObjptr (p, fromProc->heap->start);

  /* If I am placing a thread on another core, the thread must reside
   * in the shared heap. So Lift it.
   */
  if (proc != (int)s->procId && !(isObjptrInHeap (s, s->sharedHeap, op))) {
    if (DEBUG_SQ)
      fprintf (stderr, "GC_sqEnque: moving closure to shared heap[%d]\n",
               s->procId);
    moveTransitiveClosure (s, &op, FALSE, TRUE);
    if (DEBUG_SQ)
      fprintf (stderr, "GC_sqEnque: moving closure to shared heap done. "FMTOBJPTR" [%d]\n",
               op, s->procId);

  }

  CircularBuffer* cq = getSubQ (fromProc->schedulerQueue, i);
  assert (!CircularBufferIsFull(cq));
  CircularBufferEnque (cq, op);
  Parallel_wakeUpThread (proc, 1);
}

pointer GC_sqDeque (GC_state s, int i) {
  CircularBuffer* cq = getSubQ (s->schedulerQueue, i);
  objptr op = (objptr)NULL;
  pointer res = (pointer)NULL;
  if (!CircularBufferDeque (cq, &op))
    res = objptrToPointer (op, s->heap->start);

  assert (res);

  if (DEBUG_SQ)
    fprintf (stderr, "GC_sqDeque p="FMTPTR" proc=%d q=%d [%d]\n",
             (uintptr_t)res, s->procId, i, s->procId);

  assert (isPointerInHeap (s, s->heap, res) || isPointerInHeap (s, s->sharedHeap, res));

  return res;
}

bool GC_sqIsEmptyPrio (int i) {
  GC_state s = pthread_getspecific (gcstate_key);
  CircularBuffer* cq = getSubQ (s->schedulerQueue, i);
  return CircularBufferIsEmpty (cq);
}

static inline void moveAllThreadsFromSecToPrim (GC_state s) {
  GC_sqAcquireLock (s, s->procId);

  CircularBuffer* prim = getSubQ (s->schedulerQueue, 0);
  CircularBuffer* sec = getSubQ (s->schedulerQueue, 1);
  objptr op = (objptr)NULL;

  while (!CircularBufferDeque (sec, &op)) {
    assert (!CircularBufferIsFull(prim));
    CircularBufferEnque (prim, op);
  }

  GC_sqReleaseLock (s, s->procId);
}

bool GC_sqIsEmpty (GC_state s) {
  bool resPrim = CircularBufferIsEmpty (s->schedulerQueue->primary);
  bool resSec = CircularBufferIsEmpty (s->schedulerQueue->secondary);
  bool res = resPrim && resSec;
  if (res && (s->preemptOnWBASize != 0 || s->spawnOnWBASize != 0)) {
    /* Force a GC if we find that the primary scheduler queue is empty and
     * preemptOnWBA is not */
    forceLocalGC (s);
    if (!resSec)
      moveAllThreadsFromSecToPrim (s);
    resPrim = FALSE;
  }
  return (resPrim && resSec);
}

void GC_sqClean (GC_state s) {
  assert (s->procStates);
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    CircularBufferClean (s->schedulerQueue->primary);
    CircularBufferClean (s->schedulerQueue->secondary);
  }
}

void GC_sqAcquireLock (GC_state s, int proc) {
    while (not Parallel_compareAndSwap
            ((pointer)(&s->schedulerLocks[proc]), -1, s->procId));
}

void GC_sqReleaseLock (GC_state s, int proc) {
    assert (s->schedulerLocks[proc] == (int32_t)s->procId);
    s->schedulerLocks[proc] = -1;
}

void foreachObjptrInSQ (GC_state s, SchedulerQueue* sq, GC_foreachObjptrFun f) {
  if (!sq)
    return;
  GC_sqAcquireLock (s, s->procId);
  for (int i=0; i<2; i++) {
    CircularBuffer* cq = getSubQ (sq, i);
    uint32_t rp = cq->readPointer;
    uint32_t wp = cq->writePointer;

    if (DEBUG_DETAILED)
      fprintf (stderr, "foreachObjptrInSQ sq="FMTPTR" q=%d rp=%d wp=%d [%d]\n",
                (uintptr_t)sq, i, rp, wp, s->procId);
    while (rp != wp) {
      callIfIsObjptr (s, f, &(cq->keys[rp]));
      rp++;
      rp %= cq->size;
    }
  }
  GC_sqReleaseLock (s, s->procId);
}

int sizeofSchedulerQueue (GC_state s, int i) {
  SchedulerQueue* sq = s->schedulerQueue;
  assert (sq);
  int total = 0;
  CircularBuffer* cq = getSubQ (sq, i);
  uint32_t rp = cq->readPointer;
  uint32_t wp = cq->writePointer;
  total += ((wp - rp + cq->size) % cq->size);
  return total;
}
