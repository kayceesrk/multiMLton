/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

//XXX unsafe executed in parallel
size_t GC_size (GC_state s, pointer root) {
  size_t res;

  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s); /* update stack in heap, in case it is reached */

  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size marking\n");
  res = dfsMarkByMode (s, root, MARK_MODE, FALSE, FALSE, FALSE);
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size unmarking\n");
  dfsMarkByMode (s, root, UNMARK_MODE, FALSE, FALSE, FALSE);
  LEAVE_LOCAL0 (s);

  return res;
}

size_t GC_sizeInLocalHeap (GC_state s, pointer root) {
  size_t res;

  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s);

  if (DEBUG_SIZE)
    fprintf (stderr, "GC_sizeInLocalHeap marking\n");
  res = dfsMarkByMode (s, root, MARK_MODE, FALSE, FALSE, TRUE);
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_sizeInLocalHeap unmarking\n");
  dfsMarkByMode (s, root, UNMARK_MODE, FALSE, FALSE, TRUE);

  LEAVE_LOCAL0 (s);
  return res;
}
