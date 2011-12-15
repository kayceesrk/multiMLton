/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

//XXX KC -- if only a minor GC is performed, then how can you fix up all
//forwarding pointers. So, I guess this can only be done at the end of a major
//GC, which will be triggered if the primary scheduler queue is empty and the
//preemptOnWB queue is not empty
void liftAllObjptrsInMoveOnWBA (GC_state s) {

  s->cumulativeStatistics->numPreemptGC += s->preemptOnWBASize;
  if (s->schedulerQueue) {
    s->cumulativeStatistics->numReadyPrimGC += sizeofSchedulerQueue (s, 0);
    s->cumulativeStatistics->numReadySecGC += sizeofSchedulerQueue (s, 1);
  }


  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjptrsInMoveOnWBA: moveOnWBASize = %d [%d]\n",
             s->moveOnWBASize, s->procId);
  for (int32_t i=0; i < s->moveOnWBASize; i++) {
    objptr op = s->moveOnWBA[i];
    moveTransitiveClosure (s, &op, FALSE, FALSE);
  }
  s->moveOnWBASize = 0;
  for (int32_t i=0; i < s->spawnOnWBASize; i++) {
    moveTransitiveClosure (s, &(s->spawnOnWBA[i].op), FALSE, FALSE);
  }

  /* move the threads from preemptOnWBA to scheduler queue */
  if (DEBUG_LWTGC)
    fprintf (stderr, "liftAllObjptrsInMoveOnWBA: preemptOnWBASize = %d [%d]\n",
             s->preemptOnWBASize, s->procId);
  if (s->preemptOnWBASize > 0) {
    GC_sqAcquireLock (s, s->procId);
    for (int i=0; i < s->preemptOnWBASize; i++) {
      if (s->preemptOnWBA[i].kind == HOST)
        sqEnque (s, objptrToPointer (s->preemptOnWBA[i].op, s->heap->start), s->procId, 0);
      else //(s->preemptOnWBA[i].kind == PARASITE)
        sqEnque (s, objptrToPointer (s->preemptOnWBA[i].op, s->heap->start), s->procId, 2);
    }
    s->preemptOnWBASize = 0;
    GC_sqReleaseLock (s, s->procId);
  }

  int i=0;
  while (i < s->spawnOnWBASize) {
    int proc = s->spawnOnWBA[i].proc;
    objptr op = s->spawnOnWBA[i].op;

    /* If I am placing a thread on another core, the thread must reside
     * in the shared heap. So Lift it.
     */
    if (proc != (int)s->procId && !(isObjptrInHeap (s, s->sharedHeap, op))) {
      if (DEBUG_SQ)
        fprintf (stderr, "moving closure to shared heap[%d]\n",
                 s->procId);
      moveTransitiveClosure (s, &op, FALSE, TRUE);
      if (DEBUG_SQ)
        fprintf (stderr, "moving closure to shared heap done. "FMTOBJPTR" [%d]\n",
                 op, s->procId);

    }

    GC_sqEnque (s, objptrToPointer (op, s->sharedHeap->start), proc, 0);

    i++;
  }
  s->spawnOnWBASize = 0;
}

void GC_addToMoveOnWBA (GC_state s, pointer p) {
  s->cumulativeStatistics->numMoveWB++;
  ++(s->moveOnWBASize);
  if (s->moveOnWBASize > s->moveOnWBAMaxSize) {
    s->moveOnWBAMaxSize *= 2;
    objptr* newMoveOnWBA =
        (objptr*) realloc (s->moveOnWBA, sizeof (objptr) * s->moveOnWBAMaxSize);
    if (newMoveOnWBA == NULL) {
      fprintf (stderr, "newMoveOnWBA s->moveOnWBA="FMTPTR" oldMaxSize=%d [%d]\n",
               (uintptr_t)s->moveOnWBA, s->moveOnWBAMaxSize, s->procId);
    }
    assert (newMoveOnWBA);
    s->moveOnWBA = newMoveOnWBA;
  }
  s->moveOnWBA[s->moveOnWBASize - 1] = pointerToObjptr (p, s->heap->start);
}


static inline bool foreachObjptrInUnforwardedObject (GC_state s, pointer p);
bool foreachObjptrInUnforwardedObject (GC_state s, pointer p) {
  GC_header header = getHeader (p);
  if (header == GC_FORWARDED)
    return TRUE;
  foreachObjptrInObject (s, p, fixFwdObjptr, TRUE);
  return TRUE;
}

void GC_addToSpawnOnWBA (GC_state s, pointer p, int proc) {
  bool isClosureClean = FALSE;
  size_t size = 0;
  isClosureClean = __GC_isThreadClosureClean (s, p, &size);

  if (isClosureClean) {
    pointer newP = GC_moveWithCopyType (s, p, FALSE, TRUE, FALSE);
    foreachObjectInRange (s, s->sessionStart, s->frontier, foreachObjptrInUnforwardedObject, TRUE);
    s->sessionStart = s->frontier;
    GC_sqEnque (s, newP, proc, 0);
    return;
  }
  ++(s->spawnOnWBASize);
  if (s->spawnOnWBASize > s->spawnOnWBAMaxSize) {
    s->spawnOnWBAMaxSize *= 2;
    SpawnThread* newSpawnOnWBA =
        (SpawnThread*) realloc (s->spawnOnWBA, sizeof (objptr) * s->spawnOnWBAMaxSize);
    assert (newSpawnOnWBA);
    s->spawnOnWBA = newSpawnOnWBA;
  }
  s->spawnOnWBA[s->spawnOnWBASize - 1].op = pointerToObjptr (p, s->heap->start);
  s->spawnOnWBA[s->spawnOnWBASize - 1].proc = proc;

  if (s->spawnOnWBASize > 16 && s->controls->useIdentityForCleanliness)
    forceLocalGC (s);
}


void GC_addToPreemptOnWBA (GC_state s, pointer p, int kind) {
  assert (kind == 0 or kind == 1);

  s->cumulativeStatistics->numPreemptWB++;
  s->cumulativeStatistics->numReadyPrimWB += sizeofSchedulerQueue (s, 0);
  s->cumulativeStatistics->numReadySecWB += sizeofSchedulerQueue (s, 1);

  objptr op = pointerToObjptr (p, s->heap->start);
  ++(s->preemptOnWBASize);
  if (s->preemptOnWBASize > s->preemptOnWBAMaxSize) {
    s->preemptOnWBAMaxSize *= 2;
    PreemptThread* newPreemptOnWBA =
        (PreemptThread*) realloc (s->preemptOnWBA, sizeof (PreemptThread) * s->preemptOnWBAMaxSize);
    assert (newPreemptOnWBA);
    s->preemptOnWBA = newPreemptOnWBA;
  }
  s->preemptOnWBA[s->preemptOnWBASize - 1].op = op;

  if (kind == 0)
    s->preemptOnWBA[s->preemptOnWBASize - 1].kind = HOST;
  else
    s->preemptOnWBA[s->preemptOnWBASize - 1].kind = PARASITE;
}

static inline void foreachObjptrInWBAs (GC_state s, GC_state fromState, GC_foreachObjptrFun f) {
  for (int i=0; i < fromState->moveOnWBASize; i++)
    callIfIsObjptr (s, f, &(fromState->moveOnWBA [i]));
  for (int i=0; i < fromState->preemptOnWBASize; i++)
    callIfIsObjptr (s, f, &(fromState->preemptOnWBA[i].op));
  for (int i=0; i < fromState->spawnOnWBASize; i++)
    callIfIsObjptr (s, f, &((fromState->spawnOnWBA [i]).op));
}

static inline void foreachObjptrInExportableWBAs (GC_state s, GC_state fromState, GC_foreachObjptrFun f) {
  for (int i=0; i < fromState->moveOnWBASize; i++)
    callIfIsObjptr (s, f, &(fromState->moveOnWBA [i]));
  for (int i=0; i < fromState->spawnOnWBASize; i++)
    callIfIsObjptr (s, f, &((fromState->spawnOnWBA [i]).op));
}
