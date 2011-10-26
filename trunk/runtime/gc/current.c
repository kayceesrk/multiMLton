/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

objptr getThreadCurrentObjptr (GC_state s) {
  objptr op = s->currentThread;
  pointer p = objptrToPointer (op, s->heap->start);
  pointer newP = GC_forwardBase (s, p);
  if (p != newP) {
    s->currentThread = pointerToObjptr (newP, s->heap->start);
    if (DEBUG_THREADS)
      fprintf (stderr, "getThreadCurrentObjptr old="FMTPTR" new="FMTPTR" [%d]\n",
               (uintptr_t)p, (uintptr_t)newP, s->procId);
    GC_thread thrd = (GC_thread)p;
    GC_stack stk = (GC_stack)objptrToPointer (thrd->stack, s->heap->start);
    if (stk->thread == pointerToObjptr (p, s->sharedHeap->start))
      stk->thread = s->currentThread;
  }
  return s->currentThread;
}

GC_thread getThreadCurrent (GC_state s) {
  pointer p = objptrToPointer(getThreadCurrentObjptr(s), s->heap->start);
  assert ((getHeader (p) & ~(LIFT_MASK | VIRGIN_MASK)) == (GC_header)0x3);
  return (GC_thread)(p + offsetofThread (s));
}

objptr getStackCurrentObjptr (GC_state s) {
  GC_thread thread = getThreadCurrent(s);
  return thread->stack;
}

GC_stack getStackCurrent (GC_state s) {
  pointer p = objptrToPointer(getStackCurrentObjptr(s), s->heap->start);
  assert ((getHeader (p) & ~(LIFT_MASK | VIRGIN_MASK)) == (GC_header)0x1);
  return (GC_stack)p;
}
