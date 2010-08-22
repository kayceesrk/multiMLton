/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* enter and leave should be called at the start and end of every GC
 * function that is exported to the outside world.  They make sure
 * that the function is run in a critical section and check the GC
 * invariant.
 */
void enter (GC_state s) {
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "enter [%d]\n", Proc_processorNumber (s));
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);
  Proc_beginCriticalSection(s);
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "enter locked [%d]\n", Proc_processorNumber (s));
  if (DEBUG_ENTER_LEAVE)
    displayGCState (s, stderr);
  assert (invariantForGC (s));
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "enter ok [%d]\n", Proc_processorNumber (s));
}

void leave (GC_state s) {
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave [%d] \n", Proc_processorNumber (s));
  /* The mutator frontier invariant may not hold
   * for functions that don't ensureBytesFree.
   */
  assert (invariantForMutator (s, FALSE, TRUE));
  s->syncReason = SYNC_NONE;
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave ok [%d]\n", Proc_processorNumber (s));
  Proc_endCriticalSection(s);
  endAtomic (s);
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave unlocked [%d]\n", Proc_processorNumber (s));
}
