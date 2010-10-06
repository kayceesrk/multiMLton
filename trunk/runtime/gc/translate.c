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
  if (isPointerInSharedHeap (s, p)) {
      if (DEBUG)
          fprintf (stderr, "translateObjptr: shared heap pointer "FMTPTR" translation skipped.\n",
                   (uintptr_t)p);
      return;
  }

  if (DEBUG)
      fprintf (stderr, "translateObjptr: Remappting pointer "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)p, (uintptr_t)((p - translateState.from) + translateState.to));
  p = (p - translateState.from) + translateState.to;
  *opp = pointerToObjptr (p, translateState.to);
}

/* translateHeap (s, from, to, size)
 */
void translateHeap (GC_state s, pointer from, pointer to, size_t size) {
  pointer limit;

  if (from == to)
    return;

  if (DEBUG or s->controls->messages)
    fprintf (stderr,
             "[GC: Translating heap at "FMTPTR" of size %s bytes from "FMTPTR".]\n",
             (uintptr_t)to,
             uintmaxToCommaString(size),
             (uintptr_t)from);
  translateState.from = from;
  translateState.to = to;
  /* Translate globals and heap. */
  foreachGlobalObjptr (s, translateObjptr);
  limit = to + size;
  foreachObjptrInRange (s, alignFrontier (s, to), &limit, translateObjptr, FALSE);
}
