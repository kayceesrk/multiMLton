/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE GC_thread GC_copyFrames (int startOffset);
static pointer GC_getFrameBottom (void);
PRIVATE int GC_getFrameBottomAsOffset (void);
GC_thread GC_preemptAsync (GC_thread thrd, int startOffset);
void GC_noop (void);

void GC_printFrames (void);
void GC_printPointer (pointer p);
void GC_printStackTop (void);
void GC_printPointerAtOffset (int offset);

pointer GC_getPointerFromOffset (int offset);
int GC_getCopiedSize (void);

static inline void foo (GC_state s, GC_frameIndex i);

// offset from bottom of stack
PRIVATE void GC_jumpDown (GC_state s, int offset);
PRIVATE void GC_prefixAndSwitchTo (GC_state s, GC_thread th);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
