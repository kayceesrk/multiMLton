/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "enter ok [%d]\n", Proc_processorNumber (s));
}

void leave (GC_state s) {
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave [%d] \n", Proc_processorNumber (s));
  /* The mutator frontier invariant may not hold
   * for functions that don't ensureBytesFree.
   */
  s->syncReason = SYNC_NONE;
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave ok [%d]\n", Proc_processorNumber (s));
  Proc_endCriticalSection(s);
  //assert (invariantForMutator (s, FALSE, TRUE));
  endAtomic (s);
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave unlocked [%d]\n", Proc_processorNumber (s));
}


//Local enter and leave
void enter_local (GC_state s) {
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "enter_local [%d]\n", Proc_processorNumber (s));
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);
  incSync (s);
  if (DEBUG_ENTER_LEAVE)
    displayGCState (s, stderr);
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "enter_local ok [%d]\n", Proc_processorNumber (s));
}

void leave_local (GC_state s) {
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave_local [%d] \n", Proc_processorNumber (s));
  s->syncReason = SYNC_NONE;
  if (DEBUG_ENTER_LEAVE)
    fprintf (stderr, "leave_local ok [%d]\n", Proc_processorNumber (s));
  //Proc_endCriticalSection(s);
  endAtomic (s);
}
