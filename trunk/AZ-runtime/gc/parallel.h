
#if (defined (MLTON_GC_INTERNAL_BASIS))

void Parallel_init (void);
void Parallel_initResources (GC_state s);

void Parallel_lock (Int32);
void Parallel_unlock (Int32);

Int32 Parallel_processorNumber (void);
Int32 Parallel_numberOfProcessors (void);
Int32 Parallel_numIOThreads (void);
Word64 Parallel_maxBytesLive (void);
void Parallel_resetBytesLive (void);

Int32 Parallel_fetchAndAdd (pointer p, Int32 v);
bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new);
Int32 Parallel_vCompareAndSwap (pointer p, Int32 old, Int32 new);

void Parallel_maybeWaitForGC (void);
void maybeWaitForGC (GC_state s);

void Parallel_disablePreemption (void);
void Parallel_enablePreemption (void);
void Parallel_wait (void);
void Parallel_wakeUpThread (Int32 p, Int32 dataIn);

long long
timeval_diff(struct timeval *difference,
             struct timeval *end_time,
             struct timeval *start_time);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

