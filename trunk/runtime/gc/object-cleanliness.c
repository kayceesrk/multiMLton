/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

const char* numReferencesToString (GC_numReferences n);
const char* numReferencesToString (GC_numReferences n) {
  switch (n) {
    case 0:
      return "ZERO";
    case 1:
      return "ONE";
    case 2:
      return "MANY";
    default:
      return "MANY";
  }
}


void isObjectPointerVirginUnmark (__attribute__((unused)) GC_state s,
                            __attribute__((unused)) pointer current,
                            __attribute__((unused)) pointer prev) {
}


/* isObjectPointerVirginMark (s, p)
 *
 * This function needs s->tmpPointer to be explicitly set to the root of the
 * object's closure. Because, the notion of viginity differ for the object
 * closure's root and the internal nodes. For the root, there needs to be no
 * pointers for the object to be a virgin, whereas for the internal nodes, it
 * is OK to have one reference (that is reachable from the object root).
 *
 * The result is transmitted through s->tmpBool.
 *
 */
void isObjectPointerVirginMark (GC_state s, pointer p, pointer parent) {
  if (!s->tmpBool)
    return;

  assert (s->tmpPointer != BOGUS_POINTER);
  bool isVirgin = FALSE;


  if (s->tmpPointer == p)
    isVirgin = (countReferences (getHeader(p)) == ZERO);
  else
    isVirgin = (countReferences (getHeader(p)) < MANY);

  if (!isVirgin && !objectHasIdentity(s, getHeader(p)))
    isVirgin = TRUE;
  parent = parent;

#if 0
  if (s->selectiveDebug) {
    GC_header h = getHeader (p);
    if (isVirgin)
      fprintf (s->fp, "X%p [label=\""FMTPTR" | %s | %s\"];\n",
               p, (uintptr_t)p, numReferencesToString (countReferences (h)),
               boolToString (objectHasIdentity (s, h)));
    else
      fprintf (s->fp, "X%p [label=\""FMTPTR" | %s | %s\", \
                       style = filled, fillcolor = red];\n",
               p, (uintptr_t)p, numReferencesToString (countReferences (h)),
               boolToString (objectHasIdentity (s, h)));
    if (parent != NULL)
      fprintf (s->fp, "X%p -> X%p;\n", (uintptr_t)parent, (uintptr_t)p);
  }
#endif


  s->tmpBool = s->tmpBool && isVirgin;
}

void doesPointToTmpPointer (GC_state s, objptr* opp) {
  pointer p = objptrToPointer (*opp, s->heap->start);
  if (p == s->tmpPointer)
    s->tmpInt++;
}


bool GC_isObjectClean (GC_state s, pointer p) {
  bool hasIdentityTransitive, isUnbounded, isClosureVirgin;
  GC_header header = getHeader (p);
  GC_objectTypeTag tag;
  unsigned int objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  splitHeader (s, header, getHeaderp (p), &tag, NULL,
               NULL, NULL, &hasIdentityTransitive, &isUnbounded);

  if (tag == STACK_TAG) {
    isClosureVirgin = FALSE;
  }
  else {
    s->tmpBool = TRUE;
    s->tmpPointer = p;
    if (s->selectiveDebug) {
      char str[50];
      sprintf (str, "Closure_"FMTPTR".dot", (uintptr_t)p);
      s->fp = fopen (str, "w");
      if (s->fp == NULL) exit (1);
      fprintf (s->fp, "digraph {\n");
      fprintf (s->fp, "node [shape=record];\n");
    }
    dfsMarkByMode (s, p, isObjectPointerVirginMark, MARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    if (s->selectiveDebug) {
      fprintf (s->fp, "}\n");
      fclose (s->fp);
      s->fp = NULL;
    }
    isClosureVirgin = s->tmpBool;
    dfsMarkByMode (s, p, isObjectPointerVirginUnmark, UNMARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    s->tmpPointer = BOGUS_POINTER;
  }

  if ((DEBUG_OBJECT_TYPE_INFO or s->selectiveDebug)) {
    fprintf (stderr, "hasIdentityTransitive = %d isUnbounded = %d objectTypeIndex = %d \
                      isClosureVirgin = %d numPointerFromStack = %d\n",
                     hasIdentityTransitive, isUnbounded, objectTypeIndex,
                     isClosureVirgin, s->tmpInt);
  }

  s->selectiveDebug = FALSE;

  return isClosureVirgin;
}

