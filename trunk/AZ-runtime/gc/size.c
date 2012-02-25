/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

//XXX unsafe executed in parallel
size_t GC_size (GC_state s, pointer root) {
  return GC_sizeInLocalHeap (s, root);
}

size_t GC_sizeInLocalHeap (GC_state s, pointer root) {
  size_t res;

  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s);

  if (DEBUG_SIZE)
    fprintf (stderr, "GC_sizeInLocalHeap marking "FMTPTR"\n", (uintptr_t)root);
  res = dfsMarkByMode (s, root, emptyForeachObjectFun, MARK_MODE,
                       FALSE, FALSE, TRUE, FALSE);
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_sizeInLocalHeap unmarking\n");
  dfsMarkByMode (s, root, emptyForeachObjectFun, UNMARK_MODE,
                 FALSE, FALSE, TRUE, FALSE);

  LEAVE_LOCAL0 (s);
  return res;
}

/* This is a special estimate of size of objects that will end up in the shared
 * heap if the root were lifted. In particular, this will only return the size
 * of closures that,
 * (1) is in the local heap, even if parts of the closure are in the shared heap
 * (2) only walks non-parasitic stacks if s->forwardState.forceStackForwarding
 *     is true.
 */
size_t estimateSizeForLifting (GC_state s, pointer root) {
  size_t res;

  s->syncReason = SYNC_MISC;
  ENTER_LOCAL0 (s);

  if (DEBUG_SIZE)
    fprintf (stderr, "estimateSizeForLifting marking\n");
  res = dfsMarkByMode (s, root, emptyForeachObjectFun, MARK_MODE,
                       FALSE, FALSE, FALSE, TRUE);
  if (DEBUG_SIZE)
    fprintf (stderr, "estimateSizeForLifting unmarking\n");
  dfsMarkByMode (s, root, emptyForeachObjectFun, UNMARK_MODE,
                 FALSE, FALSE, FALSE, TRUE);

  LEAVE_LOCAL0 (s);
  return res;
}
