/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct {
  size_t objectClosureSize;
  /* Represents the start of the toSpace on the sender side. */
  pointer toSpaceStart;
} MLton_RCCE_sendRecvControlPacket;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void MLton_RCCE_send (GC_state s, pointer p, int dest);
pointer MLton_RCCE_recv (GC_state s, int src);
double MLton_RCCE_wtime (void);


#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

