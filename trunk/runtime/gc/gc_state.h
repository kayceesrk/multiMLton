/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_state {
  /* These fields are at the front because they are the most commonly
   * referenced, and having them at smaller offsets may decrease code
   * size and improve cache performance.
   */
  pointer frontier; /* start <= frontier < limit */
  pointer limit; /* limit = heap->start + heap->size */
  pointer stackTop; /* Top of stack in current thread. */
  pointer stackLimit; /* stackBottom + stackSize - maxFrameSize */
  pointer localHeapStart;
  pointer sharedHeapStart;
  pointer sharedHeapEnd;
  pointer sessionStart;
  struct GC_generationalMaps generationalMaps; /* generational maps for this heap */

  /* ML arrays and queues */
  SchedulerQueue* schedulerQueue;
  Lock* schedulerLocks;

  objptr* moveOnWBA;
  int32_t moveOnWBASize;
  int32_t moveOnWBAMaxSize;

  PreemptThread* preemptOnWBA;
  int32_t preemptOnWBASize;
  int32_t preemptOnWBAMaxSize;

  objptr* danglingStackList;
  int32_t danglingStackListSize;
  int32_t danglingStackListMaxSize;

  SpawnThread* spawnOnWBA;
  int32_t spawnOnWBASize;
  int32_t spawnOnWBAMaxSize;

  pointer sharedFrontier;
  pointer sharedLimit;
  bool tmpBool;
  pointer tmpPointer;
  int32_t tmpInt;
  size_t exnStack;

  /* Alphabetized fields follow. */
  size_t alignment; /* */
  bool amInGC;
  bool amOriginal;
  uint32_t procId;
  char **atMLtons; /* Initial @MLton args, processed before command line. */
  uint32_t atMLtonsLength;
  uint32_t atomicState;
  objptr callFromCHandlerThread; /* Handler for exported C calls (in heap). */
  struct GC_callStackState callStackState;
  bool canMinor; /* TRUE iff there is space for a minor gc. */
  struct GC_controls *controls;
  struct GC_cumulativeStatistics *cumulativeStatistics;
  objptr currentThread; /* Currently executing thread (in heap). */

  struct GC_forwardState forwardState;
  pointer ffiOpArgsResPtr;
  GC_frameLayout frameLayouts; /* Array of frame layouts. */
  uint32_t frameLayoutsLength; /* Cardinality of frameLayouts array. */
  /* Currently only used to hold raise operands. XXX at least i think so */
  Pointer *globalObjptrNonRoot;
  /* Ordinary globals */
  objptr *globals;
  uint32_t globalsLength;
  bool hashConsDuringGC;
  struct GC_heap *heap;
  struct GC_intInfInit *intInfInits;
  uint32_t intInfInitsLength;
  struct GC_lastMajorStatistics *lastMajorStatistics;
  struct GC_lastSharedMajorStatistics *lastSharedMajorStatistics;
  pointer limitPlusSlop; /* limit + GC_HEAP_LIMIT_SLOP */
  pointer sharedLimitPlusSlop;
  pointer start; /* Like heap->nursery but per processor.  nursery <= start <= frontier */
  pointer sharedStart;
  int (*loadGlobals)(FILE *f); /* loads the globals from the file. */
  uint32_t magic; /* The magic number for this executable. */
  uint32_t maxFrameSize;
  bool mutatorMarksCards;
  bool selectiveDebug;
  /* For PCML */
  pthread_t pthread;
  int32_t timeInterval; /* In milliseconds */
  bool enableTimer;
  /* The maximum amount of concurrency */
  int32_t numberOfProcs;
  /* For I/O threads */
  int32_t numIOThreads;
  GC_objectHashTable objectHashTable;
  GC_objectType objectTypes; /* Array of object types. */
  uint32_t objectTypesLength; /* Cardinality of objectTypes array. */
  /* States for each processor */
  GC_state procStates;
  struct GC_profiling profiling;
  GC_frameIndex (*returnAddressToFrameIndex) (GC_returnAddress ra);
  uint32_t returnToC;
  /* Roots that may be, for example, on the C call stack */
  objptr *roots;
  uint32_t rootsLength;
  objptr savedThread; /* Result of GC_copyCurrentThread.
                       * Thread interrupted by arrival of signal.
                       */
  objptr savedClosure; /* This is used for switching to a new thread */
  objptr pacmlThreadId; /* ThreadId of the current pacml thread */
  int (*saveGlobals)(FILE *f); /* saves the globals to the file. */
  bool saveWorldStatus; /* */
  struct GC_heap *secondaryLocalHeap; /* Used for major copying collection. */
  struct GC_heap *sharedHeap; /* Used as a uncollected shared heap for testing lwtgc */
  struct GC_heap *secondarySharedHeap; /* Used for major copying collection on shared heap */
  objptr signalHandlerThread; /* Handler for signals (in heap). */
  struct GC_signalsInfo signalsInfo;
  struct GC_sourceMaps sourceMaps;
  pointer stackBottom; /* Bottom of stack in current thread. */
  uintmax_t startTime; /* The time when GC_init or GC_loadWorld was called. */
  int32_t copiedSize;
  int32_t syncReason;
  struct GC_sysvals sysvals;
  struct GC_translateState translateState;
  struct GC_vectorInit *vectorInits;
  uint32_t vectorInitsLength;
  UT_array* reachable;
  CopyObjectMap* copyObjectMap;
  bool copyImmutable;
  GC_weak weaks; /* Linked list of (live) weak pointers */
  char *worldFile;
  UT_array* directCloXferArray; /* Array to store closures directly transferred to this core */

  /* DEV variables
   * ------------
   * The following variables are only used for development purposes. The are to
   * be removed/not used for production/benchmarking runs.
   */
  FILE* fp;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayGCState (GC_state s, FILE *stream);

static inline size_t sizeofGCStateCurrentStackUsed (GC_state s);
static inline void setGCStateCurrentThreadAndStack (GC_state s);
static void setGCStateCurrentSharedHeap (GC_state s,
                                         size_t oldGenBytesRequested,
                                         size_t nurseryBytesRequested,
                                         bool duringInit);
static void setGCStateCurrentLocalHeap (GC_state s,
                                        size_t oldGenBytesRequested,
                                        size_t nurseryBytesRequested);

static GC_state getGCStateFromPointer (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_setSelectiveDebug (GC_state *gs, bool b);
PRIVATE bool GC_getAmOriginal (GC_state *gs);
PRIVATE void GC_setAmOriginal (GC_state *gs, bool b);
PRIVATE bool GC_getIsPCML (void);
PRIVATE void GC_setControlsMessages (GC_state *gs, bool b);
PRIVATE void GC_setControlsSummary (GC_state *gs, bool b);
PRIVATE void GC_setControlsRusageMeasureGC (GC_state *gs, bool b);
PRIVATE uintmax_t GC_getCumulativeStatisticsBytesAllocated (GC_state *gs);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (GC_state *gs);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (GC_state *gs);
PRIVATE uintmax_t GC_getCumulativeStatisticsNumMinorGCs (GC_state *gs);
PRIVATE size_t GC_getCumulativeStatisticsMaxBytesLive (GC_state *gs);
PRIVATE void GC_setHashConsDuringGC (GC_state *gs, bool b);
PRIVATE size_t GC_getLastMajorStatisticsBytesLive (GC_state *gs);

PRIVATE pointer GC_getCallFromCHandlerThread (GC_state *gs);
PRIVATE void GC_setCallFromCHandlerThread (GC_state *gs, pointer p);
PRIVATE pointer GC_getCurrentThread (GC_state *gs);
PRIVATE pointer GC_getSavedThread (GC_state *gs);
PRIVATE void GC_setSavedThread (GC_state *gs, pointer p);
PRIVATE void GC_setSignalHandlerThread (GC_state *gs, pointer p);

PRIVATE void GC_print (int);
PRIVATE inline pointer GC_forwardBase (const GC_state s, const pointer p);
PRIVATE void GC_markCleanliness (const GC_state s, pointer target, pointer source,
                                 char* file, int line);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

//PRIVATE struct rusage* GC_getRusageGCAddr (GC_state s);

PRIVATE sigset_t* GC_getSignalsHandledAddr (GC_state *gs);
PRIVATE sigset_t* GC_getSignalsPendingAddr (GC_state *gs);
PRIVATE void GC_setGCSignalHandled (GC_state *gs, bool b);
PRIVATE bool GC_getGCSignalPending (GC_state *gs);
PRIVATE void GC_setGCSignalPending (GC_state *gs, bool b);
PRIVATE sigset_t* GC_getSignalsSet (GC_state *gs);
PRIVATE void GC_commEvent (void);
