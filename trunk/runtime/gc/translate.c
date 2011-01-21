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

struct translateState {
  pointer from;
  pointer to;
};
static struct translateState translateState;

void translateObjptr (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, translateState.from);

  /* Do not translate pointers that does not belong to your heap */
  //XXX GCSH -- this needs to change
  if (isPointerInHeap (s, s->sharedHeap, p)) {
      if (DEBUG_DETAILED)
          fprintf (stderr, "translateObjptr: shared heap pointer "FMTPTR" translation skipped.\n",
                   (uintptr_t)p);
      return;
  }

  if (DEBUG_DETAILED)
      fprintf (stderr, "translateObjptr: Remapping pointer "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)p, (uintptr_t)((p - translateState.from) + translateState.to));
  p = (p - translateState.from) + translateState.to;
  *opp = pointerToObjptr (p, translateState.to);

  GC_objectTypeTag tag;
  GC_header header = getHeader (p);
  if (header == GC_FORWARDED)
      return;
  splitHeader (s, header, &tag, NULL, NULL, NULL);
  if (tag == STACK_TAG) {
      GC_stack stack = (GC_stack)p;
      if (DEBUG_TRANSLATE)
          fprintf (stderr, "translateObjptr: Remappting stack->thread objptr\n");
      translateObjptr (s, &stack->thread);
  }
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
  translateState.from = from;
  translateState.to = to;
  /* Translate globals and heap. */
  foreachGlobalObjptrInScope (s, translateObjptr);
  limit = to + size;
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, translateObjptr, FALSE);
  if (DEBUG)
    fprintf (stderr, "[GC: Translating heap done.] [%d]\n", s->procId);
}
