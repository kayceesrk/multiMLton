/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static void displayCol (FILE *out, int width, const char *s) {
  int extra;
  int i;
  int len;

  len = strlen (s);
  if (len < width) {
    extra = width - len;
    for (i = 0; i < extra; i++)
      fprintf (out, " ");
  }
  fprintf (out, "%s\t", s);
}

static void displayCollectionStats (FILE *out, const char *name, struct rusage *ru,
                                    uintmax_t num, uintmax_t bytes) {
  uintmax_t ms;

  ms = rusageTime (ru);
  fprintf (out, "%s", name);
  displayCol (out, 7, uintmaxToCommaString (ms));
  displayCol (out, 7, uintmaxToCommaString (num));
  displayCol (out, 15, uintmaxToCommaString (bytes));
  displayCol (out, 15,
              (ms > 0)
              ? uintmaxToCommaString ((uintmax_t)(1000.0 * (float)bytes/(float)ms))
              : "-");
  fprintf (out, "\n");
}

static void summaryWrite (GC_state s,
                          struct GC_cumulativeStatistics* cumul,
                          FILE* out,
                          bool isCumul) {
  uintmax_t totalTime;
  uintmax_t gcTime;
  uintmax_t syncTime;
  //uintmax_t threadTime;
  uintmax_t rtTime;
  //uintmax_t lockTime;

  gcTime = timevalTime (&cumul->ru_gc);
  syncTime = timevalTime (&cumul->tv_sync);
  //threadTime = rusageTime (&cumul->ru_thread);
  rtTime = timevalTime (&cumul->tv_rt);
  /* lockTime = rusageTime (&cumul->ru_lock); */
  fprintf (out, "GC type\t\ttime ms\t number\t\t  bytes\t      bytes/sec\n");
  fprintf (out, "-------------\t-------\t-------\t---------------\t---------------\n");
  displayCollectionStats
    (out, "copying\t\t",
     &cumul->ru_gcCopying,
     cumul->numCopyingGCs,
     cumul->bytesCopied);
  displayCollectionStats
    (out, "mark-compact\t",
     &cumul->ru_gcMarkCompact,
     cumul->numMarkCompactGCs,
     cumul->bytesMarkCompacted);
  displayCollectionStats
    (out, "minor\t\t",
     &cumul->ru_gcMinor,
     cumul->numMinorGCs,
     cumul->bytesCopiedMinor);
  totalTime = getCurrentTime () - s->procStates[0].startTime;

  char str[20];
  sprintf (str, " ");
  if (isCumul) {
      totalTime *= s->numberOfProcs;
      sprintf (str, "(cumulative)");
  }

  fprintf (out, "\n");
  fprintf (out, "total time: %s ms %s\n",
           uintmaxToCommaString (totalTime), str);
  fprintf (out, "total GC time: %s ms (%.1f%%)\n",
           uintmaxToCommaString (gcTime),
           (0 == totalTime)
           ? 0.0
           : 100.0 * ((double) gcTime) / (double)totalTime);
  fprintf (out, "total sync time: %s ms (%.1f%%)\n",
           uintmaxToCommaString (syncTime),
           (0 == totalTime)
           ? 0.0
           : 100.0 * ((double) syncTime) / (double)totalTime);
  /*
     fprintf (out, "total thread time: %s ms (%.1f%%)\n",
     uintmaxToCommaString (threadTime),
     (0 == totalTime)
     ? 0.0
     : 100.0 * ((double) threadTime) / (double)totalTime);
   */
  fprintf (out, "total rt time: %s ms (%.1f%%)\n",
           uintmaxToCommaString (rtTime),
           (0 == totalTime)
           ? 0.0
           : 100.0 * ((double) rtTime) / (double)totalTime);
  /*
     fprintf (out, "total lock time: %s ms (%.1f%%)\n",
     uintmaxToCommaString (lockTime),
     (0 == totalTime)
     ? 0.0
     : 100.0 * ((double) lockTime) / (double)totalTime);
   */

  fprintf (out, "max pause time: %s ms\n",
           uintmaxToCommaString (cumul->maxPauseTime));

  fprintf (out, "\n");
  fprintf (out, "total bytes allocated: %s bytes\n",
           uintmaxToCommaString (cumul->bytesAllocated));
  fprintf (out, "total bytes lifted: %s bytes\n",
           uintmaxToCommaString (cumul->bytesLifted));
  fprintf (out, "total bytes filled: %s bytes\n",
           uintmaxToCommaString (cumul->bytesFilled));
  fprintf (out, "max bytes live: %s bytes\n",
           uintmaxToCommaString (cumul->maxBytesLive));
  fprintf (out, "max heap size: %s bytes\n",
           uintmaxToCommaString (cumul->maxHeapSize));
  fprintf (out, "max stack size: %s bytes\n",
           uintmaxToCommaString (cumul->maxStackSize));
  fprintf (out, "num cards marked: %s\n",
           uintmaxToCommaString (cumul->numCardsMarked));
  fprintf (out, "bytes scanned: %s bytes\n",
           uintmaxToCommaString (cumul->bytesScannedMinor));
  fprintf (out, "bytes hash consed: %s bytes\n",
           uintmaxToCommaString (cumul->bytesHashConsed));

  fprintf (out, "\n");
  fprintf (out, "sync for old gen array: %s\n",
           uintmaxToCommaString (cumul->syncForOldGenArray));
  fprintf (out, "sync for new gen array: %s\n",
           uintmaxToCommaString (cumul->syncForNewGenArray));
  fprintf (out, "sync for stack: %s\n",
           uintmaxToCommaString (cumul->syncForStack));
  fprintf (out, "sync for heap: %s\n",
           uintmaxToCommaString (cumul->syncForHeap));
  fprintf (out, "sync for lift: %s\n",
           uintmaxToCommaString (cumul->syncForLift));
  fprintf (out, "sync force: %s\n",
           uintmaxToCommaString (cumul->syncForce));
  fprintf (out, "sync misc: %s\n",
           uintmaxToCommaString (cumul->syncMisc));

  fprintf (out, "\n");
  fprintf (out, "num threads created: %s\n",
           uintmaxToCommaString (cumul->numThreadsCreated));
  fprintf (out, "num preempt on WB: %s\n",
           uintmaxToCommaString (cumul->numPreemptWB));
  fprintf (out, "num ideal preempt on WB: %s\n",
           uintmaxToCommaString (cumul->numMoveWB));
  float avgAvail =
      (cumul->numPreemptWB > 0)
      ? ((float)cumul->numReadyWB/cumul->numPreemptWB)
      : 0.0f;
  fprintf (out, "avg # threads ready on WB: %f\n", avgAvail);
}

static inline void initStat (struct GC_cumulativeStatistics* cumul) {
  cumul->bytesAllocated = 0;
  cumul->bytesFilled = 0;
  cumul->bytesCopied = 0;
  cumul->bytesCopiedMinor = 0;
  cumul->bytesHashConsed = 0;
  cumul->bytesMarkCompacted = 0;
  cumul->bytesScannedMinor = 0;
  cumul->bytesLifted = 0;
  cumul->maxBytesLive = 0;
  cumul->maxBytesLiveSinceReset = 0;
  cumul->maxHeapSize = 0;
  cumul->maxPauseTime = 0;
  cumul->maxStackSize = 0;
  cumul->numCardsMarked = 0;
  cumul->numLimitChecks = 0;
  cumul->syncForOldGenArray = 0;
  cumul->syncForNewGenArray = 0;
  cumul->syncForStack = 0;
  cumul->syncForHeap = 0;
  cumul->syncForLift = 0;
  cumul->syncForce = 0;
  cumul->syncMisc = 0;
  cumul->numCopyingGCs = 0;
  cumul->numHashConsGCs = 0;
  cumul->numMarkCompactGCs = 0;
  cumul->numMinorGCs = 0;
  cumul->numThreadsCreated = 0;
  cumul->numPreemptWB = 0;
  cumul->numMoveWB = 0;
  cumul->numReadyWB = 0;
  timevalZero (&cumul->ru_gc);
  rusageZero (&cumul->ru_gcCopying);
  rusageZero (&cumul->ru_gcMarkCompact);
  rusageZero (&cumul->ru_gcMinor);
  timevalZero (&cumul->tv_sync);
  rusageZero (&cumul->ru_thread);
  timevalZero (&cumul->tv_rt);
}

void GC_done (GC_state s) {
  FILE *out;
  out = stderr;
  minorGC (s);
  char fname[40];
  struct GC_cumulativeStatistics cumul;

  if (s->controls->summary) {
    initStat (&cumul);
    if (s->controls->summary == SUMMARY_INDIVIDUAL) {
      for (int proc=0; proc < s->numberOfProcs; proc++) {
        sprintf (fname, "gc-summary.%d.out", proc);
        out = fopen_safe (fname, "wb");
        summaryWrite (s, s->procStates[proc].cumulativeStatistics, out, FALSE);
        fclose_safe (out);
      }
    }

    for (int proc=0; proc < s->numberOfProcs; proc++) {
      struct GC_cumulativeStatistics* d =
        s->procStates[proc].cumulativeStatistics;
      cumul.bytesAllocated += d->bytesAllocated;
      cumul.bytesFilled += d->bytesFilled;
      cumul.bytesCopied += d->bytesCopied;
      cumul.bytesCopiedMinor += d->bytesCopiedMinor;
      cumul.bytesHashConsed += d->bytesHashConsed;
      cumul.bytesLifted += d->bytesLifted;
      cumul.bytesMarkCompacted += d->bytesMarkCompacted;
      cumul.bytesScannedMinor += d->bytesScannedMinor;
      cumul.maxBytesLive =
        (d->maxBytesLive > cumul.maxBytesLive)
        ? d->maxBytesLive
        : cumul.maxBytesLive;
      cumul.maxBytesLiveSinceReset =
        (d->maxBytesLiveSinceReset > cumul.maxBytesLiveSinceReset)
        ? d->maxBytesLiveSinceReset
        : cumul.maxBytesLiveSinceReset;
      cumul.maxHeapSize =
        (d->maxHeapSize > cumul.maxHeapSize)
        ? d->maxHeapSize
        : cumul.maxHeapSize;
      cumul.maxPauseTime =
        (d->maxPauseTime > cumul.maxPauseTime)
        ? d->maxPauseTime
        : cumul.maxPauseTime;
      cumul.maxStackSize =
        (d->maxStackSize > cumul.maxStackSize)
        ? d->maxStackSize
        : cumul.maxStackSize;
      cumul.numCardsMarked += d->numCardsMarked;
      cumul.numLimitChecks += d->numLimitChecks;
      cumul.syncForOldGenArray += d->syncForOldGenArray;
      cumul.syncForNewGenArray += d->syncForNewGenArray;
      cumul.syncForStack += d->syncForStack;
      cumul.syncForHeap += d->syncForHeap;
      cumul.syncForLift += d->syncForLift;
      cumul.syncForce += d->syncForce;
      cumul.syncMisc += d->syncMisc;
      cumul.numCopyingGCs += d->numCopyingGCs;
      cumul.numHashConsGCs += d->numHashConsGCs;
      cumul.numMarkCompactGCs += d->numMarkCompactGCs;
      cumul.numMinorGCs += d->numMinorGCs;
      cumul.numThreadsCreated += d->numThreadsCreated;
      cumul.numPreemptWB += d->numPreemptWB;
      cumul.numMoveWB += d->numMoveWB;
      cumul.numReadyWB += d->numReadyWB;
      timevalPlusMax (&cumul.ru_gc, &d->ru_gc, &cumul.ru_gc);
      rusagePlusMax (&cumul.ru_gcCopying,
                     &d->ru_gcCopying,
                     &cumul.ru_gcCopying);
      rusagePlusMax (&cumul.ru_gcMarkCompact,
                     &d->ru_gcMarkCompact,
                     &cumul.ru_gcMarkCompact);
      rusagePlusMax (&cumul.ru_gcMinor,
                     &d->ru_gcMinor,
                     &cumul.ru_gcMinor);
      timevalPlusMax (&cumul.tv_sync, &d->tv_sync, &cumul.tv_sync);
      rusagePlusMax (&cumul.ru_thread,
                     &d->ru_thread,
                     &cumul.ru_thread);
      timevalPlusMax (&cumul.tv_rt, &d->tv_rt, &cumul.tv_rt);
    }

    sprintf (fname, "gc-summary.cumul.out");
    out = fopen_safe (fname, "wb");
    summaryWrite (s, &cumul, out, TRUE);
    fclose_safe (out);
  }
  releaseHeap (s, s->heap);
  releaseHeap (s, s->secondaryLocalHeap);
}
