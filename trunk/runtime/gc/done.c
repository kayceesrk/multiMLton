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

void GC_done (GC_state s) {
  FILE *out;

  s->syncReason = SYNC_FORCE;
  ENTER0 (s);
  minorGC (s);
  out = stderr;
  if (s->controls->summary) {
    uintmax_t totalTime;
    uintmax_t gcTime;
    uintmax_t syncTime;
    //uintmax_t threadTime;
    uintmax_t rtTime;
    //uintmax_t lockTime;

    gcTime = timevalTime (&s->cumulativeStatistics->ru_gc);
    syncTime = timevalTime (&s->cumulativeStatistics->tv_sync);
    //threadTime = rusageTime (&s->cumulativeStatistics->ru_thread);
    rtTime = timevalTime (&s->cumulativeStatistics->tv_rt);
    /* lockTime = rusageTime (&s->cumulativeStatistics->ru_lock); */
    fprintf (out, "GC type\t\ttime ms\t number\t\t  bytes\t      bytes/sec\n");
    fprintf (out, "-------------\t-------\t-------\t---------------\t---------------\n");
    displayCollectionStats
      (out, "copying\t\t",
       &s->cumulativeStatistics->ru_gcCopying,
       s->cumulativeStatistics->numCopyingGCs,
       s->cumulativeStatistics->bytesCopied);
    displayCollectionStats
      (out, "mark-compact\t",
       &s->cumulativeStatistics->ru_gcMarkCompact,
       s->cumulativeStatistics->numMarkCompactGCs,
       s->cumulativeStatistics->bytesMarkCompacted);
    displayCollectionStats
      (out, "minor\t\t",
       &s->cumulativeStatistics->ru_gcMinor,
       s->cumulativeStatistics->numMinorGCs,
       s->cumulativeStatistics->bytesCopiedMinor);
    totalTime = getCurrentTime () - s->startTime;
    fprintf (out, "total time: %s ms\n",
             uintmaxToCommaString (totalTime));
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
             uintmaxToCommaString (s->cumulativeStatistics->maxPauseTime));
    fprintf (out, "total bytes allocated: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics->bytesAllocated));
    fprintf (out, "total bytes filled: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics->bytesFilled));
    fprintf (out, "max bytes live: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics->maxBytesLive));
    fprintf (out, "max heap size: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics->maxHeapSize));
    fprintf (out, "max stack size: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics->maxStackSize));
    fprintf (out, "num cards marked: %s\n",
             uintmaxToCommaString (s->cumulativeStatistics->numCardsMarked));
    fprintf (out, "bytes scanned: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics->bytesScannedMinor));

    fprintf (out, "sync for old gen array: %s\n",
             uintmaxToCommaString (s->cumulativeStatistics->syncForOldGenArray));
    fprintf (out, "sync for new gen array: %s\n",
             uintmaxToCommaString (s->cumulativeStatistics->syncForNewGenArray));
    fprintf (out, "sync for stack: %s\n",
             uintmaxToCommaString (s->cumulativeStatistics->syncForStack));
    fprintf (out, "sync for heap: %s\n",
             uintmaxToCommaString (s->cumulativeStatistics->syncForHeap));
    fprintf (out, "sync misc: %s\n",
             uintmaxToCommaString (s->cumulativeStatistics->syncMisc));

    fprintf (out, "bytes hash consed: %s bytes\n",
             uintmaxToCommaString (s->cumulativeStatistics->bytesHashConsed));
  }
  releaseHeap (s, s->heap);
  releaseHeap (s, s->secondaryLocalHeap);
}
