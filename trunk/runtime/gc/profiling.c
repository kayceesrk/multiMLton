/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

GC_profileMasterIndex sourceIndexToProfileMasterIndex (GC_state s,
                                                       GC_sourceIndex i)
 {
  GC_profileMasterIndex pmi;
  pmi = s->sourceMaps.sources[i].sourceNameIndex + s->sourceMaps.sourcesLength;
  if (DEBUG_PROFILE)
    fprintf (stderr, "%"PRIu32" = sourceIndexToProfileMasterIndex ("FMTSI") [%d]\n", pmi, i, Proc_processorNumber (s));
  return pmi;
}

GC_sourceNameIndex profileMasterIndexToSourceNameIndex (GC_state s,
                                                        GC_profileMasterIndex i) {
  assert (i >= s->sourceMaps.sourcesLength);
  return i - s->sourceMaps.sourcesLength;
}

char* profileIndexSourceName (GC_state s, GC_sourceIndex i) {
  char* res;

  if (i < s->sourceMaps.sourcesLength)
    res = getSourceName (s, i);
  else
    res = s->sourceMaps.sourceNames[profileMasterIndexToSourceNameIndex (s, i)];
  return res;
}

GC_profileStack getProfileStackInfo (GC_state s, GC_profileMasterIndex i) {
  assert (s->profiling.data != NULL);
  return &(s->profiling.data->stack[i]);
}

static int profileDepth = 0;

static void profileIndent (void) {
  int i;

  for (i = 0; i < profileDepth; ++i)
    fprintf (stderr, " ");
}

void addToStackForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;

  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  if (DEBUG_PROFILE)
    fprintf (stderr, "adding %s to stack  lastTotal = %"PRIuMAX"  lastTotalGC = %"PRIuMAX"\n",
             getSourceName (s, i),
             (uintmax_t)p->total,
             (uintmax_t)p->totalGC);
  ps->lastTotal = p->total;
  ps->lastTotalGC = p->totalGC;
}

void enterSourceForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;

  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  if (0 == ps->numOccurrences) {
    ps->lastTotal = p->total;
    ps->lastTotalGC = p->totalGC;
  }
  ps->numOccurrences++;
}

void enterForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex) {
  uint32_t i;
  GC_profileData p;
  GC_sourceIndex sourceIndex;
  uint32_t *sourceSeq;

  if (DEBUG_PROFILE)
    fprintf (stderr, "enterForProfiling ("FMTSSI")\n", sourceSeqIndex);
  assert (s->profiling.stack);
  assert (sourceSeqIndex < s->sourceMaps.sourceSeqsLength);
  p = s->profiling.data;
  sourceSeq = s->sourceMaps.sourceSeqs[sourceSeqIndex];
  for (i = 1; i <= sourceSeq[0]; i++) {
    sourceIndex = sourceSeq[i];
    if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
      profileIndent ();
      fprintf (stderr, "(entering %s\n",
               getSourceName (s, sourceIndex));
      profileDepth++;
    }
    enterSourceForProfiling (s, (GC_profileMasterIndex)sourceIndex);
    enterSourceForProfiling (s, sourceIndexToProfileMasterIndex (s, sourceIndex));
  }
}

void enterFrameForProfiling (GC_state s, GC_frameIndex i) {
  enterForProfiling (s, s->sourceMaps.frameSources[i]);
}

void GC_profileEnter (GC_state s) {
  enterForProfiling (s, getCachedStackTopFrameSourceSeqIndex (s));
}

void removeFromStackForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;

  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  if (DEBUG_PROFILE)
    fprintf (stderr, "removing %s from stack  ticksInc = %"PRIuMAX"  ticksGCInc = %"PRIuMAX"\n",
             profileIndexSourceName (s, i),
             (uintmax_t)(p->total - ps->lastTotal),
             (uintmax_t)(p->totalGC - ps->lastTotalGC));
  ps->ticks += p->total - ps->lastTotal;
  ps->ticksGC += p->totalGC - ps->lastTotalGC;
}

void leaveSourceForProfiling (GC_state s, GC_profileMasterIndex i) {
  GC_profileData p;
  GC_profileStack ps;

  p = s->profiling.data;
  ps = getProfileStackInfo (s, i);
  assert (ps->numOccurrences > 0);
  ps->numOccurrences--;
  if (0 == ps->numOccurrences)
    removeFromStackForProfiling (s, i);
}

void leaveForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex) {
  int32_t i;
  GC_profileData p;
  GC_sourceIndex sourceIndex;
  uint32_t *sourceSeq;

  if (DEBUG_PROFILE)
    fprintf (stderr, "leaveForProfiling ("FMTSSI")\n", sourceSeqIndex);
  assert (s->profiling.stack);
  assert (sourceSeqIndex < s->sourceMaps.sourceSeqsLength);
  p = s->profiling.data;
  sourceSeq = s->sourceMaps.sourceSeqs[sourceSeqIndex];
  for (i = sourceSeq[0]; i > 0; i--) {
    sourceIndex = sourceSeq[i];
    if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
      profileDepth--;
      profileIndent ();
      fprintf (stderr, "leaving %s) [%d]\n",
               getSourceName (s, sourceIndex),
               Proc_processorNumber (s));
    }
    leaveSourceForProfiling (s, (GC_profileMasterIndex)sourceIndex);
    leaveSourceForProfiling (s, sourceIndexToProfileMasterIndex (s, sourceIndex));
  }
}

void leaveFrameForProfiling (GC_state s, GC_frameIndex i) {
  leaveForProfiling (s, s->sourceMaps.frameSources[i]);
}

void GC_profileLeave (GC_state s) {
  leaveForProfiling (s, getCachedStackTopFrameSourceSeqIndex (s));
}


void incForProfiling (GC_state s, size_t amount, GC_sourceSeqIndex sourceSeqIndex) {
  uint32_t *sourceSeq;
  GC_sourceIndex topSourceIndex;

  if (DEBUG_PROFILE)
    fprintf (stderr, "incForProfiling (%"PRIuMAX", "FMTSSI") [%d]\n",
             (uintmax_t)amount, sourceSeqIndex, Proc_processorNumber (s));
  assert (sourceSeqIndex < s->sourceMaps.sourceSeqsLength);
  sourceSeq = s->sourceMaps.sourceSeqs[sourceSeqIndex];
  topSourceIndex =
    sourceSeq[0] > 0
    ? sourceSeq[sourceSeq[0]]
    : SOURCES_INDEX_UNKNOWN;
  if (DEBUG_PROFILE) {
    profileIndent ();
    fprintf (stderr, "bumping %s by %"PRIuMAX" [%d]\n",
             getSourceName (s, topSourceIndex), (uintmax_t)amount,
             Proc_processorNumber (s));
  }
  /* Must have compiled for profile alloc */
  if (PROFILE_ALLOC_INSTRUMENT) {
    fprintf (stderr, "ALLOC "FMTPTR" "FMTPTR" %s [%d]\n",
             (uintptr_t)s->frontier - amount, (uintptr_t)s->frontier,
             getSourceName (s, topSourceIndex), s->procId);
  }
  s->profiling.data->countTop[topSourceIndex] += amount;
  s->profiling.data->countTop[sourceIndexToProfileMasterIndex (s, topSourceIndex)] += amount;
  if (s->profiling.stack)
    enterForProfiling (s, sourceSeqIndex);
  if (SOURCES_INDEX_GC == topSourceIndex)
    s->profiling.data->totalGC += amount;
  else
    s->profiling.data->total += amount;
  if (s->profiling.stack)
    leaveForProfiling (s, sourceSeqIndex);
}

void GC_profileInc (GC_state s, size_t amount) {
  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_profileInc (%"PRIuMAX") [%d]\n", (uintmax_t)amount,
             Proc_processorNumber (s));
  incForProfiling (s, amount,
                   s->amInGC
                   ? SOURCE_SEQ_GC
                   : getCachedStackTopFrameSourceSeqIndex (s));
}

void GC_profileAllocInc (GC_state s, size_t amount) {
  if (s->profiling.isOn and (PROFILE_ALLOC == s->profiling.kind)) {
    if (DEBUG_PROFILE)
      fprintf (stderr, "GC_profileAllocInc (%"PRIuMAX") [%d]\n", (uintmax_t)amount,
             Proc_processorNumber (s));
    GC_profileInc (s, amount);
  }
}

GC_profileData profileMalloc (GC_state s) {
  GC_profileData p;
  uint32_t profileMasterLength;

  p = (GC_profileData)(malloc_safe (sizeof(*p)));
  p->total = 0;
  p->totalGC = 0;
  profileMasterLength = s->sourceMaps.sourcesLength + s->sourceMaps.sourceNamesLength;
  p->countTop = (uintmax_t*)(calloc_safe(profileMasterLength, sizeof(*(p->countTop))));
  if (s->profiling.stack)
    p->stack =
      (struct GC_profileStack *)
      (calloc_safe(profileMasterLength, sizeof(*(p->stack))));
  if (DEBUG_PROFILE)
    fprintf (stderr, FMTPTR" = profileMalloc ()\n", (uintptr_t)p);
  return p;
}

GC_profileData GC_profileMalloc (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return profileMalloc (s);
}

void profileFree (GC_state s, GC_profileData p) {
  if (DEBUG_PROFILE)
    fprintf (stderr, "profileFree ("FMTPTR") [%d]\n", (uintptr_t)p, Proc_processorNumber (s));
  free (p->countTop);
  if (s->profiling.stack)
    free (p->stack);
  free (p);
}

void GC_profileFree (__attribute__ ((unused)) GC_state *gs, GC_profileData p) {
  GC_state s = pthread_getspecific (gcstate_key);
  profileFree (s, p);
}

void writeProfileCount (GC_state s, FILE *f,
                        GC_profileData p, GC_profileMasterIndex i) {
  writeUintmaxU (f, p->countTop[i]);
  if (s->profiling.stack) {
    GC_profileStack ps;

    ps = &(p->stack[i]);
    writeString (f, " ");
    writeUintmaxU (f, ps->ticks);
    writeString (f, " ");
    writeUintmaxU (f, ps->ticksGC);
  }
  writeNewline (f);
}

void profileWrite (GC_state s, GC_profileData p, const char *fileName) {
  FILE *f;
  const char* kind;

  if (DEBUG_PROFILE)
    fprintf (stderr, "profileWrite("FMTPTR",%s) [%d]\n", (uintptr_t)p, fileName, Proc_processorNumber (s));
  f = fopen_safe (fileName, "wb");
  writeString (f, "MLton prof\n");
  kind = "";
  switch (s->profiling.kind) {
  case PROFILE_ALLOC:
    kind = "alloc\n";
    break;
  case PROFILE_COUNT:
    kind = "count\n";
    break;
  case PROFILE_NONE:
    die ("impossible PROFILE_NONE");
    break;
  case PROFILE_TIME_FIELD:
    kind = "time\n";
    break;
  case PROFILE_TIME_LABEL:
    kind = "time\n";
    break;
  default:
    assert (FALSE);
  }
  writeString (f, kind);
  writeString (f, s->profiling.stack ? "stack\n" : "current\n");
  writeUint32X (f, s->magic);
  writeNewline (f);
  writeUintmaxU (f, p->total);
  writeString (f, " ");
  writeUintmaxU (f, p->totalGC);
  writeNewline (f);
  writeUint32U (f, s->sourceMaps.sourcesLength);
  writeNewline (f);
  for (GC_sourceIndex i = 0; i < s->sourceMaps.sourcesLength; i++)
    writeProfileCount (s, f, p,
                       (GC_profileMasterIndex)i);
  writeUint32U (f, s->sourceMaps.sourceNamesLength);
  writeNewline (f);
  for (GC_sourceNameIndex i = 0; i < s->sourceMaps.sourceNamesLength; i++)
    writeProfileCount (s, f, p,
                       (GC_profileMasterIndex)(i + s->sourceMaps.sourcesLength));
  fclose_safe (f);
}

void GC_profileWrite (__attribute__ ((unused)) GC_state *gs, GC_profileData p, NullString8_t fileName) {
  GC_state s = pthread_getspecific (gcstate_key);
  profileWrite (s, p, (const char*)fileName);
}

void setProfTimer (long usec) {
  struct itimerval iv;

  iv.it_interval.tv_sec = 0;
  iv.it_interval.tv_usec = usec;
  iv.it_value.tv_sec = 0;
  iv.it_value.tv_usec = usec;
  unless (0 == setitimer (ITIMER_PROF, &iv, NULL))
    die ("setProfTimer: setitimer failed");
}

#if not HAS_TIME_PROFILING

/* No time profiling on this platform.  There is a check in
 * mlton/main/main.fun to make sure that time profiling is never
 * turned on.
 */
__attribute__ ((noreturn))
void initProfilingTime (__attribute__ ((unused)) GC_state s) {
  die ("no time profiling");
}

#else

static GC_state gs0 = NULL;


void GC_handleSigProf (code_pointer pc) {
  GC_frameIndex frameIndex;
  GC_state s;
  GC_sourceSeqIndex sourceSeqsIndex;

  s = pthread_getspecific (gcstate_key);
  if (DEBUG_PROFILE)
    fprintf (stderr, "GC_handleSigProf ("FMTPTR") [%d]\n", (uintptr_t)pc,
             Proc_processorNumber (s));

  bool isSomeProcInGC = false;

  /* XXX KC race condition between checking the amInGC flag and setting it on another processor */
  for (int proc = 0; proc < s->numberOfProcs; proc ++) {
      if ((s->procStates[proc]).amInGC == TRUE) {
          isSomeProcInGC = true;
          break;
      }
  }

  if (isSomeProcInGC)
    sourceSeqsIndex = SOURCE_SEQ_GC;
  else {
    frameIndex = getCachedStackTopFrameIndex (s);
    if (C_FRAME == s->frameLayouts[frameIndex].kind)
      sourceSeqsIndex = s->sourceMaps.frameSources[frameIndex];
    else {
      if (PROFILE_TIME_LABEL == s->profiling.kind) {
        uint32_t start, end, i;

        /* Binary search labels to find which method contains PC */
        start = 0;
        end = s->sourceMaps.sourceLabelsLength;
        while (end - start > 1) {
          i = (start+end)/2;
          if ((uintptr_t)s->sourceMaps.sourceLabels[i].label <= (uintptr_t)pc)
            start = i;
          else
            end = i;
        }
        i = start;

        /* The last label is dead code. Any address past it is thus unknown.
         * The first label is before all SML code. Before it is also unknown.
         */
        if (i-1 == s->sourceMaps.sourceLabelsLength ||
            (i == 0 &&
             (uintptr_t)pc < (uintptr_t)s->sourceMaps.sourceLabels[i].label)) {
          if (DEBUG_PROFILE)
            fprintf (stderr, "pc out of bounds [%d]\n", Proc_processorNumber (s));
          sourceSeqsIndex = SOURCE_SEQ_UNKNOWN;
        } else {
          sourceSeqsIndex = s->sourceMaps.sourceLabels[start].sourceSeqIndex;
        }
      } else {
        sourceSeqsIndex = s->sourceMaps.curSourceSeqsIndex;
      }
    }
  }
  incForProfiling (s, 1, sourceSeqsIndex);
}

void GC_profileDisable (void) {
  setProfTimer (0);
}
void GC_profileEnable (void) {
  setProfTimer (10000);
}


void turnOnProfilingTime (GC_state s) {
  assert (s);
  /*
   * Install catcher, which handles SIGUSR1 and calls MLton_Profile_inc.
   * SIGPROF is handled by the signal handler thread (c-main.h) and broadcast
   * as SIGUSR1 to every thread.
   *
   * One thing I should point out that I discovered the hard way: If the call
   * to sigaction does NOT specify the SA_ONSTACK flag, then even if you have
   * called sigaltstack(), it will NOT switch stacks, so you will probably die.
   * Worse, if the call to sigaction DOES have SA_ONSTACK and you have NOT
   * called sigaltstack(), it still switches stacks (to location 0) and you die
   * of a SEGV.  Thus the sigaction() call MUST occur after the call to
   * sigaltstack(), and in order to have profiling cover as much as possible,
   * you want it to occur right after the sigaltstack() call.
   */
  struct sigaction sa;
  sigemptyset (&sa.sa_mask);
  GC_setSigProfHandler (&sa);
  unless (sigaction (SIGUSR1, &sa, NULL) == 0)
    diee ("turnOnProfilingTime: sigaction failed");

  s->profiling.isProfilingTimeOn = TRUE;
}


static void initProfilingTime (GC_state s) {
  s->profiling.data = profileMalloc (s);
  if (PROFILE_TIME_LABEL == s->profiling.kind) {
    initSourceLabels (s);
  } else {
    s->sourceMaps.curSourceSeqsIndex = SOURCE_SEQ_UNKNOWN;
  }

  if (Proc_processorNumber (s) == 0)
    gs0 = s;
}

#endif

/* atexitForProfiling is for writing out an mlmon.out file even if the C code
 * terminates abnormally, e.g. due to running out of memory.  It will
 * only run if the usual SML profile atExit cleanup code did not
 * manage to run.
 */
static GC_state atexitForProfilingState;

void atexitForProfiling (void) {
  GC_state s;
  char fname[20];

  if (DEBUG_PROFILE)
    fprintf (stderr, "atexitForProfiling ()\n");
  s = atexitForProfilingState;
  if (s->profiling.isOn && s->profiling.data) {
    fprintf (stderr, "profiling is on[%d]\n", Proc_processorNumber(s));
    sprintf(fname, "mlmon.%d.out", Proc_processorNumber (s));
    profileWrite (s, s->profiling.data, fname);
  }
}

void initProfiling (GC_state s) {
  if (PROFILE_NONE == s->profiling.kind)
    s->profiling.isOn = FALSE;
  else {
    assert (s->sourceMaps.frameSourcesLength == s->frameLayoutsLength);
    switch (s->profiling.kind) {
    case PROFILE_ALLOC:
    case PROFILE_COUNT:
      s->profiling.data = profileMalloc (s);
      break;
    case PROFILE_NONE:
      die ("impossible PROFILE_NONE");
      break;
    case PROFILE_TIME_FIELD:
    case PROFILE_TIME_LABEL:
      initProfilingTime (s);
      turnOnProfilingTime (s);
      break;
    default:
      assert (FALSE);
    }
    //Must be done after profile mallocs
    s->profiling.isOn = TRUE;
    atexitForProfilingState = s;
    atexit (atexitForProfiling);
  }
}

void GC_profileDone (__attribute__ ((unused)) GC_state *gs) {
  GC_profileData p;
  GC_profileMasterIndex profileMasterIndex;
  GC_state s0 = pthread_getspecific (gcstate_key);
  char fname[20];
  assert (s0->profiling.isOn);
  for (int proc = 0; proc < s0->numberOfProcs; proc ++) {
    GC_state s = &(s0->procStates[proc]);
    if (DEBUG_PROFILE)
        fprintf (stderr, "GC_profileDone () [%d]\n",
                Proc_processorNumber (s));
    if (PROFILE_TIME_FIELD == s->profiling.kind
        or PROFILE_TIME_LABEL == s->profiling.kind)
        setProfTimer (0);
    s->profiling.isOn = FALSE;
    p = s->profiling.data;
    if (s->profiling.stack) {
        uint32_t profileMasterLength =
            s->sourceMaps.sourcesLength + s->sourceMaps.sourceNamesLength;
        for (profileMasterIndex = 0;
            profileMasterIndex < profileMasterLength;
            profileMasterIndex++) {
            if (p->stack[profileMasterIndex].numOccurrences > 0) {
                if (DEBUG_PROFILE)
                    fprintf (stderr, "done leaving %s [%d]\n",
                            profileIndexSourceName (s, profileMasterIndex),
                            Proc_processorNumber (s));
                removeFromStackForProfiling (s, profileMasterIndex);
            }
        }
    }

    /* Write out the profile results */
    sprintf(fname, "mlmon.%d.out", proc);
    profileWrite (s, s->profiling.data, fname);
    //XXX KC should I cleanup profiling data here??
  }
}

GC_profileData GC_getProfileCurrent (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->profiling.data;
}
void GC_setProfileCurrent (__attribute__ ((unused)) GC_state *gs, GC_profileData p) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->profiling.data = p;
}

