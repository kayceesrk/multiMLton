/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_BASIS))

bool GC_proceedToExtract (pointer p, int startOffset);
static pointer GC_getFrameBottom (void);
void GC_noop (void);
GC_thread GC_extractParasite (pointer p, int startOffset);
PRIVATE GC_thread GC_copyParasite (int startOffset);
PRIVATE int GC_getFrameBottomAsOffset (void);

/* Management and debugging functions */
void GC_printFrames (void);
void GC_printPointer (pointer p);
void GC_printStackTop (void);
void GC_printPointerAtOffset (int offset);
pointer GC_getPointerFromOffset (int offset);
int GC_getCopiedSize (void);

// offset from bottom of stack
PRIVATE void GC_jumpDown (GC_state s, int offset);
PRIVATE void GC_prefixAndSwitchTo (GC_state s, pointer p);

/* Measurement */
void GC_parasiteCreatedEvent (void);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
