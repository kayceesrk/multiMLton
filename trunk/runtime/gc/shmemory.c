/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void* GC_shmalloc (size_t size) {
  assert (RCCE_ue () == 0);
  if (RCCE_ue () != 0) {
    fprintf (stderr, "Should not call GC_shmalloc from cores other than core 0\n");
    exit (1);
  }
  void* res = RCCE_shmalloc (size);
  if (res == NULL) {
    fprintf (stderr, "GC_shmalloc: failed\n");
    exit (1);
  }
  return res;
}

void* GC_mpbmalloc (size_t size) {
  size_t resultSize;
  void* res = RCCE_malloc_request (size, &resultSize);
  if (res == NULL) {
    fprintf (stderr, "GC_mpbmalloc: failed\n");
    exit (1);
  }

  return res;
}
