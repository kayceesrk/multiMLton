/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void displayThread (GC_state s,
                    GC_thread thread,
                    FILE *stream) {
  fprintf(stream,
          "\t\texnStack = %"PRIuMAX"\n"
          "\t\tbytesNeeded = %"PRIuMAX"\n"
          "\t\tstack = "FMTOBJPTR"\n",
          (uintmax_t)thread->exnStack,
          (uintmax_t)thread->bytesNeeded,
          thread->stack);
  displayStack (s, (GC_stack)(objptrToPointer (thread->stack, s->heap->start)),
                stream);
}

size_t sizeofThread (GC_state s) {
  size_t res;

  res = GC_NORMAL_HEADER_SIZE + sizeof (struct GC_thread);
  res = align (res, s->alignment);
  if (DEBUG) {
    size_t check;
    uint16_t bytesNonObjptrs, numObjptrs;

    splitHeader (s, GC_THREAD_HEADER, NULL, NULL, NULL,
                 &bytesNonObjptrs, &numObjptrs, NULL, NULL);
    check = GC_NORMAL_HEADER_SIZE + (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    if (DEBUG_DETAILED)
      fprintf (stderr,
               "sizeofThread: res = %"PRIuMAX"  check = %"PRIuMAX"\n",
               (uintmax_t)res, (uintmax_t)check);
    assert (check == res);
  }
  assert (isAligned (res, s->alignment));
  return res;
}

size_t offsetofThread (GC_state s) {
  return (sizeofThread (s)) - (GC_NORMAL_HEADER_SIZE + sizeof (struct GC_thread));
}


bool GC_testSavedClosure (GC_state s) {
  return (s->savedClosure != BOGUS_OBJPTR);
}

pointer GC_getSavedClosure (GC_state s) {
  pointer p = objptrToPointer (s->savedClosure, s->heap->start);
  s->savedClosure = BOGUS_OBJPTR;
  return p;
}

void GC_setSavedClosure (GC_state s, pointer p) {
  assert (s->savedClosure == BOGUS_OBJPTR);
  s->savedClosure = pointerToObjptr (p, s->heap->start);
}


bool GC_testThreadId (GC_state s) {
  return (s->pacmlThreadId != BOGUS_OBJPTR);
}

pointer GC_getThreadId (GC_state s) {
  assert (s->pacmlThreadId != BOGUS_OBJPTR);
  fixFwdObjptr (s, &s->pacmlThreadId);
  pointer p = objptrToPointer (s->pacmlThreadId, s->heap->start);
  return p;
}

void GC_setThreadId (GC_state s, pointer p) {
  s->pacmlThreadId = pointerToObjptr (p, s->heap->start);
}
