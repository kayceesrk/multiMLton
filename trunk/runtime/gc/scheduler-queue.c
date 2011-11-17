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

static inline CircularBuffer* growCircularBuffer (CircularBuffer* que) {
  //Double the circular queue size
  CircularBuffer* newBuf;
  CircularBufferInit (&newBuf, que->size * 2);
  if (que->readPointer < que->writePointer) {
    memcpy ((void*)&(newBuf->keys[0]),
            (void*)&(que->keys[que->readPointer]),
            (que->writePointer - que->readPointer)*sizeof(KeyType));
  }
  else {
    memcpy ((void*)&(newBuf->keys[0]),
            (void*)&(que->keys[que->readPointer]),
            (que->size - que->readPointer)*sizeof(KeyType));
    memcpy ((void*)&(newBuf->keys[que->size - que->readPointer]),
            (void*)&(que->keys[0]),
            (que->writePointer)*sizeof(KeyType));

  }
  newBuf->writePointer =
    (que->writePointer - que->readPointer + que->size)
    % que->size;
  free (que);
  return newBuf;
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
  CircularBufferInit (&sq->parasites, BUFFER_SIZE);
  return sq;
}

static inline CircularBuffer* getSubQ (SchedulerQueue* sq, int i) {
  switch (i) {
    case 0:
      return sq->primary;
    case 1:
      return sq->secondary;
    case 2:
      return sq->parasites;
    default:
      assert (0 or "getSubQ: i > 2");
      die ("getSubQ: i > 2");
  }
}

void GC_sqCreateQueues (GC_state s) {
  Lock* schedulerLocks = (Lock*) malloc (sizeof(Lock) * s->numberOfProcs);
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    schedulerLocks[proc].id = -1;
    schedulerLocks[proc].count = 0;
  }
  if (s->procStates) {
    for (int proc=0; proc < s->numberOfProcs; proc++) {
      s->procStates[proc].schedulerQueue = newSchedulerQueue ();
      s->procStates[proc].schedulerLocks = schedulerLocks;
    }
  }
  else {
    s->schedulerQueue = newSchedulerQueue ();
    s->schedulerLocks = schedulerLocks;
  }
}

void sqEnque (GC_state s, pointer p, int proc, int i) {
  if (DEBUG_SQ)
    fprintf (stderr, "sqEnque p="FMTPTR" proc=%d q=%d [%d]\n",
              (uintptr_t)p, proc, i, s->procId);

  assert (p);
  GC_state fromProc = &s->procStates[proc];
  objptr op = pointerToObjptr (p, fromProc->heap->start);
  assert (isPointerInHeap (s, s->sharedHeap, p) or
          ((int)s->procId == proc));

  CircularBuffer* cq = getSubQ (fromProc->schedulerQueue, i);
  if (CircularBufferIsFull(cq)) {
    if (i == 0)
      fromProc->schedulerQueue->primary = growCircularBuffer (cq);
    else if (i == 1)
      fromProc->schedulerQueue->secondary = growCircularBuffer (cq);
    else
      fromProc->schedulerQueue->parasites = growCircularBuffer (cq);

    cq = getSubQ (fromProc->schedulerQueue, i);
  }
  assert (!CircularBufferIsFull(cq));
  CircularBufferEnque (cq, op);
  Parallel_wakeUpThread (proc, 1);
}

void GC_addToDirectCloXferArray (GC_state s, ClosureToSpawn c, int proc) {
  GC_sqAcquireLock (s, proc);
  utarray_push_back (s->procStates[proc].directCloXferArray, &c);
  GC_sqReleaseLock (s, proc);
  Parallel_wakeUpThread (proc, 1);
}

void GC_sqEnque (GC_state s, pointer p, int proc, int i) {
  GC_sqAcquireLock (s, proc);
  sqEnque (s, p, proc, i);
  GC_sqReleaseLock (s, proc);
}


/* i == 0 -- deque from primary
   i == 1 -- deque from secondary
   i == -1 -- deque from (primary or secondary)
*/

pointer GC_sqDeque (GC_state s, int i) {
  if (i == -1) {
    if (!GC_sqIsEmptyPrio (0)) {
      return GC_sqDeque (s, 0);
    }
    else if (!GC_sqIsEmptyPrio (1)) {
      return GC_sqDeque (s, 1);
    }
    else {
      assert (0 and "GC_sqDeque: All queues empty");
      die ("GC_sqDeque: All queues empty");
    }
  }

  GC_sqAcquireLock (s, s->procId);

  CircularBuffer* cq = getSubQ (s->schedulerQueue, i);
  objptr op = (objptr)NULL;
  pointer res = (pointer)NULL;
  if (!CircularBufferDeque (cq, &op))
    res = objptrToPointer (op, s->heap->start);

  GC_sqReleaseLock (s, s->procId);

  assert (res);

  if (DEBUG_SQ)
    fprintf (stderr, "GC_sqDeque p="FMTPTR" proc=%d q=%d [%d]\n",
             (uintptr_t)res, s->procId, i, s->procId);

  assert (isPointerInHeap (s, s->heap, res) || isPointerInHeap (s, s->sharedHeap, res));

  return res;
}

void flushFromDirectCloXferArray (GC_state s) {
  if (utarray_len (s->directCloXferArray) > 0) {
    UT_array* localArray;

    //Copy to local array
    GC_sqAcquireLock (s, s->procId);
    utarray_new (localArray, &directCloXfer_icd);
    ClosureToSpawn* c;
    for (c=(ClosureToSpawn*)utarray_front (s->directCloXferArray);
         c!=NULL;
         c=(ClosureToSpawn*)utarray_next (s->directCloXferArray, c)) {
      utarray_push_back (localArray, c);
    }
    utarray_clear (s->directCloXferArray);
    GC_sqReleaseLock (s, s->procId);

    //Copy all to local heap and enque
    for (c=(ClosureToSpawn*)utarray_front (localArray);
         c!=NULL;
         c=(ClosureToSpawn*)utarray_next (localArray, c)) {
      s->syncReason = SYNC_MISC;
      ENTER_LOCAL0 (s);
      void* buffer = c->p;
      ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE, TRUE, FALSE, 0,
                                                      c->size, FALSE, 0);
      GC_memcpy (buffer, s->frontier, c->size);
      assert (s->frontier + c->size < s->limit);
      pointer to = s->frontier;
      s->frontier += c->size;
      translateRange (s, c->p, to, c->size);
      LEAVE_LOCAL0 (s);
      free (buffer);

      pointer closure = advanceToObjectData (s, to);
      GC_sqEnque (s, closure, s->procId, 0);
    }
    utarray_clear (localArray);
  }
}

bool GC_sqIsEmptyPrio (int i) {
  GC_state s = pthread_getspecific (gcstate_key);
  flushFromDirectCloXferArray (s);
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
  flushFromDirectCloXferArray (s);
  Parallel_maybeWaitForGC ();
  bool primEmpty = CircularBufferIsEmpty (s->schedulerQueue->primary);
  bool secEmpty = CircularBufferIsEmpty (s->schedulerQueue->secondary);

  if (primEmpty && secEmpty && (s->preemptOnWBASize != 0 || s->spawnOnWBASize != 0)) {
    /* Force a GC if we find that the primary scheduler queue is empty and
     * preemptOnWBA is not */
    forceLocalGC (s);
    if (!secEmpty && FALSE)
      moveAllThreadsFromSecToPrim (s);
    primEmpty = FALSE;
  }

  return (primEmpty && secEmpty);
}

void GC_sqClean (GC_state s) {
  assert (s->procStates);
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    CircularBufferClean (s->schedulerQueue->primary);
    CircularBufferClean (s->schedulerQueue->secondary);
    CircularBufferClean (s->schedulerQueue->parasites);
  }
}

void GC_sqAcquireLock (GC_state s, int proc) {
  assert (proc < s->numberOfProcs);
  Lock* lock = &s->schedulerLocks[proc];

  if (lock->id == (int)s->procId) {
    lock->count++;
    return;
  }

  assert (lock->id != (int)s->procId);

  do {
  AGAIN:
    /* Since GC_sqAcquireLock can be called while doing shared GC (while
     * already in a critical section, try to join a barrier only if we are not
     * already in one */
    if (Proc_threadInSection (s) and
        !Proc_executingInSection (s) and FALSE) {
      s->syncReason = SYNC_HELP;
      performSharedGC (s, 0);
    }

    if (Proc_executingInSection (s) and lock->id >= 0)
      assert (0 and "GC_sqAcquireLock: DEADLOCK!!");

    if (lock->id >= 0)
      goto AGAIN;
  } while (not Parallel_compareAndSwap
           ((pointer)(&lock->id), -1, s->procId));
}

void GC_sqReleaseLock (GC_state s, int proc) {
  Lock* lock = &s->schedulerLocks[proc];
  assert (lock->id == (int32_t)s->procId);
  if (lock->count > 0) {
    --lock->count;
    return;
  }
  assert (lock->count == 0);
  lock->id = -1;
}

void foreachObjptrInSQ (GC_state s, SchedulerQueue* sq, GC_foreachObjptrFun f) {
  if (!sq)
    return;
  GC_sqAcquireLock (s, s->procId);
  for (int i=0; i<3; i++) {
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
