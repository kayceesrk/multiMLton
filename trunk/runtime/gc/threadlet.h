/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


#define GC_THREADS

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE GC_thread GC_copyParasite (int startOffset);
static pointer GC_getFrameBottom (void);
PRIVATE int GC_getFrameBottomAsOffset (void);
bool GC_proceedToExtract (pointer p, int startOffset);
GC_thread GC_extractParasite (pointer p, int startOffset);
void GC_mltonNoop (void);

/* Management and debugging functions */
void GC_printFrames (void);
void GC_printPointer (pointer p);
void GC_printStackTop (void);
void GC_printPointerAtOffset (int offset);
pointer GC_getPointerFromOffset (int offset);
int GC_getCopiedSize (void);
void GC_debugPrint (int i);

static inline void foo (GC_state s, GC_frameIndex i);

// offset from bottom of stack
PRIVATE void GC_jumpDown (GC_state s, int offset);
PRIVATE void GC_prefixAndSwitchTo (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
