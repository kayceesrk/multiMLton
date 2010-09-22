#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE GC_thread GC_copyParasite (int startOffset);
static pointer GC_getFrameBottom (void);
PRIVATE int GC_getFrameBottomAsOffset (void);
bool GC_proceedToExtract (pointer p, int startOffset);
GC_thread GC_extractParasite (pointer p, int startOffset);
void GC_noop (void);

/* Management and debugging functions */
void GC_printFrames (void);
void GC_printPointer (pointer p);
void GC_printStackTop (void);
void GC_printPointerAtOffset (int offset);
pointer GC_getPointerFromOffset (int offset);
int GC_getCopiedSize (void);

static inline void foo (GC_state s, GC_frameIndex i);

// offset from bottom of stack
PRIVATE void GC_jumpDown (GC_state s, int offset);
PRIVATE void GC_prefixAndSwitchTo (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
