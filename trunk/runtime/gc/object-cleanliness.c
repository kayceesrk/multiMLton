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
 * The result is transmitted through s->tmpBool, which is the predicate
 * isClosureVirgin.
 *
 */
void isObjectPointerVirginMark (GC_state s, pointer p, pointer parent) {
  if (!s->tmpBool)
    return;

  assert (s->tmpPointer != BOGUS_POINTER);
  bool isVirgin = FALSE;
  GC_header h = getHeader (p);

  if (s->tmpPointer == p)
    isVirgin = (countReferences (h) == ZERO);
  else {
    isVirgin = (countReferences (h) < MANY);
  }

  //if (!isVirgin && !objectHasIdentity(s, h))
    //isVirgin = TRUE;

  if ((h & ~(VIRGIN_MASK|LIFT_MASK)) == 0x1 /* STACK_TAG */)
    isVirgin = FALSE;

  parent = parent; //To silence GCC warnings

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

/* doesPointerToMarkedObject
 * -------------------------
 * s->tmpPointer is the root of the thread closure
 * s->tmpBool is isClosureVirgin predicate
 * s->tmpInt counts the number of troublesome pointers from stack
 */
void doesPointToMarkedObject (GC_state s, objptr* opp) {
  pointer p = objptrToPointer (*opp, s->heap->start);
  if (isPointerMarked (p) && p != s->tmpPointer &&
      objectHasIdentity (s, getHeader(p))) {
    s->tmpInt++;
  }
}

bool GC_isThreadClosureClean (GC_state s, pointer p) {
  size_t size;
  return __GC_isThreadClosureClean (s, p, &size);
}

bool __GC_isThreadClosureClean (GC_state s, pointer p, size_t* size) {
  bool hasIdentityTransitive, isUnbounded, isClosureVirgin;
  unsigned int numPointersFromStack = -1;
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
#if 0
    if (s->selectiveDebug) {
      char str[50];
      sprintf (str, "Closure_"FMTPTR".dot", (uintptr_t)p);
      s->fp = fopen (str, "w");
      if (s->fp == NULL) exit (1);
      fprintf (s->fp, "digraph {\n");
      fprintf (s->fp, "node [shape=record];\n");
    }
#endif

    *size = dfsMarkByMode (s, p, isObjectPointerVirginMark, MARK_MODE,
                           FALSE, FALSE, TRUE, FALSE);

    //Walk the current stack and test if the current stack points to any marked object
    s->tmpInt = 0;
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    foreachObjptrInObject (s, (pointer)getStackCurrent (s), doesPointToMarkedObject, FALSE);

    isClosureVirgin = s->tmpBool;
    numPointersFromStack = s->tmpInt;

#if 0
    if (s->selectiveDebug) {
      fprintf (s->fp, "}\n");
      fclose (s->fp);
      s->fp = NULL;
    }
#endif
    dfsMarkByMode (s, p, isObjectPointerVirginUnmark, UNMARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);

    //Reset tmp* variables
    s->tmpPointer = BOGUS_POINTER;
    s->tmpBool = TRUE;
    s->tmpInt = 0;
  }

  if (DEBUG_CLEANLINESS || TRUE) {
    fprintf (stderr, "GC_isThreadClosureClean: hasIdentityTransitive = %d "
                     "isUnbounded = %d objectTypeIndex = %d size = %zu "
                     "isClosureVirgin = %d numPointerFromStack = %d\n",
             hasIdentityTransitive, isUnbounded, objectTypeIndex, *size,
             isClosureVirgin, numPointersFromStack);
  }

  return isClosureVirgin;
}

bool GC_isObjectClosureClean (GC_state s, pointer p) {
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

    dfsMarkByMode (s, p, isObjectPointerVirginMark, MARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    isClosureVirgin = s->tmpBool;
    dfsMarkByMode (s, p, isObjectPointerVirginUnmark, UNMARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    s->tmpPointer = BOGUS_POINTER;
  }

  if (DEBUG_CLEANLINESS) {
    fprintf (stderr, "GC_isObjectClosureClean: hasIdentityTransitive = %d \
                      isUnbounded = %d objectTypeIndex = %d isClosureVirgin = %d\n",
             hasIdentityTransitive, isUnbounded, objectTypeIndex, isClosureVirgin);
  }

  return isClosureVirgin;
}

