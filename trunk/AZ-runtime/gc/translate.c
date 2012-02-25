/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          translateHeap                           */
/* ---------------------------------------------------------------- */


//TODO KC merge local and shared translations, use information in
//translateState to decide if a pointer needs to be tranalated

void translateObjptrLocal (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->translateState.from);

  /* Do not translate pointers that does not belong to your heap */
  if (isPointerInHeap (s, s->sharedHeap, p)) {
      if (DEBUG_DETAILED)
          fprintf (stderr, "translateObjptrLocal: shared heap pointer "FMTPTR" translation skipped.\n",
                   (uintptr_t)p);
      return;
  }

  if (DEBUG_DETAILED)
      fprintf (stderr, "translateObjptrLocal: Remapping pointer "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)p, (uintptr_t)((p - s->translateState.from) + s->translateState.to));
  p = (p - s->translateState.from) + s->translateState.to;
  *opp = pointerToObjptr (p, s->translateState.to);

  if (!isPointerInHeap (s, s->heap, p)) {
    if (DEBUG)
        fprintf (stderr, "translate: p="FMTPTR" not in new heap. [%d]\n",
                 (uintptr_t)p, s->procId);
    assert (0);
  }
//  GC_header header = getHeader (p);
//  GC_objectTypeTag tag;
//  splitHeader (s, header, getHeaderp (p), &tag, NULL, NULL, NULL);
//  if (tag == STACK_TAG) {
//      GC_stack stack = (GC_stack)p;
//      if (DEBUG_TRANSLATE)
//          fprintf (stderr, "translateObjptrLocal: Remapping stack->thread objptr\n");
//      translateObjptrLocal (s, &stack->thread);
//  }
}

/* translateHeap (s, from, to, size)
 */
void translateHeap (GC_state s, pointer from, pointer to, size_t size) {
  pointer limit;

  if (from == to)
    return;

  if (DEBUG or s->controls->messages)
    fprintf (stderr,
             "[GC: Translating heap at "FMTPTR" of size %s bytes from "FMTPTR".] [%d]\n",
             (uintptr_t)to,
             uintmaxToCommaString(size),
             (uintptr_t)from, s->procId);
  s->translateState.from = from;
  s->translateState.to = to;
  s->translateState.size = size;
  /* Translate globals and heap. */
  foreachGlobalObjptrInScope (s, translateObjptrLocal);
  limit = to + size;
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, translateObjptrLocal, FALSE);

  s->translateState.from = BOGUS_POINTER;
  s->translateState.to = BOGUS_POINTER;
  s->translateState.size = 0;
  if (DEBUG)
    fprintf (stderr, "[GC: Translating heap done.] [%d]\n", s->procId);
}

void translateObjptrShared (GC_state s, objptr* opp) {
  pointer p;
  bool done;
  do {
    done = TRUE;
    p = objptrToPointer (*opp, s->sharedHeap->start);
    pointer oldP = p;

    if (DEBUG_DETAILED || DEBUG_TRANSLATE)
      fprintf (stderr, "translateObjptrShared(1) "FMTPTR"\n", (uintptr_t)p);
    //Only translate the pointers that are in the fromSpace
    if (p >= s->translateState.from and
        p < (s->translateState.from + s->translateState.size)) {
      p = (p - s->translateState.from) + s->translateState.to;
      *opp = pointerToObjptr (p, s->translateState.to);
      if (DEBUG_DETAILED or DEBUG_TRANSLATE)
        fprintf (stderr, "translateObjptrShared(2): old="FMTPTR" new="FMTPTR"\n",
                 (uintptr_t)oldP, (uintptr_t)p);
    }
    if (getHeader (p) == GC_FORWARDED) {
      if (DEBUG_DETAILED)
        fprintf (stderr, "translateObjptrShared saw forwarded pointer "FMTPTR"\n",
                 (uintptr_t)p);
      done = FALSE;
      *opp = *(objptr*)p;
    }
  } while (not done);

  GC_header header = getHeader (p);
  GC_objectTypeTag tag;
  splitHeader (s, header, getHeaderp (p), &tag, NULL,
               NULL, NULL, NULL, NULL);
  if (tag == STACK_TAG) {
    GC_stack stack = (GC_stack)p;
    if (DEBUG_TRANSLATE)
      fprintf (stderr, "translateObjptrShared: Remapping stack->thread objptr. stack="FMTPTR" thread="FMTOBJPTR"\n",
               (uintptr_t)stack, stack->thread);
    translateObjptrShared (s, &stack->thread);
  }
}

void translateObjptrInRange (GC_state s, objptr *opp) {
  pointer p = objptrToPointer (*opp, s->translateState.from);

  if (p >= s->translateState.from and
      p < (s->translateState.from + s->translateState.size)) {
    if (DEBUG_DETAILED)
        fprintf (stderr, "translateObjptrInRange: Remapping pointer "FMTPTR" to "FMTPTR"\n",
                (uintptr_t)p, (uintptr_t)((p - s->translateState.from) + s->translateState.to));
    p = (p - s->translateState.from) + s->translateState.to;
    *opp = pointerToObjptr (p, s->translateState.to);
  }
}

void translateRange (GC_state s, pointer from, pointer to, size_t size) {
  pointer limit;

  if (from == to)
    return;

  if (DEBUG or s->controls->messages)
    fprintf (stderr,
             "[GC: Translating range at "FMTPTR" of size %s bytes from "FMTPTR".] [%d]\n",
             (uintptr_t)to,
             uintmaxToCommaString(size),
             (uintptr_t)from, s->procId);
  s->translateState.from = from;
  s->translateState.to = to;
  s->translateState.size = size;
  limit = to + size;
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, translateObjptrInRange, FALSE);
  s->translateState.from = BOGUS_POINTER;
  s->translateState.to = BOGUS_POINTER;
  s->translateState.size = 0;
  if (DEBUG)
    fprintf (stderr, "[GC: Translating range done.] [%d]\n", s->procId);
}



void translateSharedHeap (GC_state s, pointer from, pointer to, size_t size) {
  pointer limit;

  if (from == to)
    return;

  if (DEBUG or s->controls->messages or DEBUG_TRANSLATE)
    fprintf (stderr,
             "[GC: Translating shared heap to "FMTPTR" of size %s bytes from "FMTPTR".] [%d]\n",
             (uintptr_t)to,
             uintmaxToCommaString(size),
             (uintptr_t)from, s->procId);
  s->translateState.from = from;
  s->translateState.to = to;
  s->translateState.size = size;
  /* Translate globals and heap. */
  foreachGlobalObjptr (s, translateObjptrShared);
  limit = to + size;

  for (int proc=0; proc < s->numberOfProcs; proc++) {
    GC_state r = &s->procStates[proc];
    pointer end = r->heap->start + r->heap->oldGenSize;
    foreachObjptrInRange (s, r->heap->start, &end, translateObjptrShared, FALSE);
    if (r->frontier > end)
      foreachObjptrInRange (s, r->heap->nursery, &r->frontier, translateObjptrShared, FALSE);
  }
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, translateObjptrShared, FALSE);

  s->translateState.from = BOGUS_POINTER;
  s->translateState.to = BOGUS_POINTER;
  s->translateState.size = 0;
  if (DEBUG || DEBUG_TRANSLATE)
    fprintf (stderr, "[GC: Translating shared heap done.] [%d]\n", s->procId);
}
