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
  return (void*) RCCE_shmalloc (size);
}
