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
      return "LOCAL_MANY";
    case 3:
      return "GLOBAL_MANY";
    default:
      fprintf (stderr, "numReferencesToString: Unknown case\n");
      exit (1);
  }
}


bool isWriteCleanUnmark (__attribute__((unused)) GC_state s,
                         __attribute__((unused)) pointer current,
                         __attribute__((unused)) pointer prev) {
  return TRUE;
}

bool isSpawnCleanUnmark (__attribute__((unused)) GC_state s,
                              __attribute__((unused)) pointer current,
                              __attribute__((unused)) pointer prev) {
  return TRUE;
}


/* isWriteCleanMark (s, p)
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
bool isWriteCleanMark (GC_state s, pointer p, pointer parent) {
  parent = parent; //To silence GCC warnings

  assert (s->tmpPointer != BOGUS_POINTER);
  bool isVirgin = FALSE;
  GC_header h = getHeader (p);

  if (s->tmpPointer == p)
    isVirgin = (countReferences (h) == ZERO);
  else {
    isVirgin = (countReferences (h) <= ONE);
  }

  if (!isVirgin && !objectHasIdentity(s, h))
    isVirgin = TRUE;

  s->tmpBool = s->tmpBool && isVirgin;

  return s->tmpBool;
}

bool isSpawnCleanMark (GC_state s, pointer p, pointer parent) {
  parent = parent; //To silence GCC warnings

  assert (s->tmpPointer != BOGUS_POINTER);
  bool isVirgin = FALSE;
  GC_header h = getHeader (p);

  if (s->tmpPointer == p)
    isVirgin = (countReferences (h) == ZERO);
  else {
    isVirgin = (countReferences (h) <= ONE);
  }

  if (!isVirgin && !objectHasIdentityTransitive(s, h))
    isVirgin = TRUE;

  s->tmpBool = s->tmpBool && isVirgin;

  return s->tmpBool;
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

    *size = dfsMarkByMode (s, p, isSpawnCleanMark, MARK_MODE,
                           FALSE, FALSE, TRUE, FALSE);

    //Walk the current stack and test if the current stack points to any marked object
    s->tmpInt = 0;
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    foreachObjptrInObject (s, (pointer)getStackCurrent (s), doesPointToMarkedObject, FALSE);
    isClosureVirgin = s->tmpBool;
    numPointersFromStack = s->tmpInt;

    dfsMarkByMode (s, p, isSpawnCleanUnmark, UNMARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);

    //Reset tmp* variables
    s->tmpPointer = BOGUS_POINTER;
    s->tmpBool = TRUE;
    s->tmpInt = 0;
  }

  if (DEBUG_CLEANLINESS) {
    fprintf (stderr, "GC_isThreadClosureClean: hasIdentityTransitive = %d "
                     "isUnbounded = %d objectTypeIndex = %d size = %zu "
                     "isClosureVirgin = %d numPointerFromStack = %d\n",
             hasIdentityTransitive, isUnbounded, objectTypeIndex, *size,
             isClosureVirgin, numPointersFromStack);
  }

  return (isClosureVirgin && (numPointersFromStack == 0));
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

    dfsMarkByMode (s, p, isWriteCleanMark, MARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    isClosureVirgin = s->tmpBool;
    dfsMarkByMode (s, p, isWriteCleanUnmark, UNMARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    s->tmpPointer = BOGUS_POINTER;
  }

  if (DEBUG_CLEANLINESS) {
    fprintf (stderr, "GC_isObjectClosureClean: "FMTPTR" hasIdentityTransitive = %d \
                      isUnbounded = %d objectTypeIndex = %d isClosureVirgin = %d\n",
             (uintptr_t)p, hasIdentityTransitive, isUnbounded,
             objectTypeIndex, isClosureVirgin);
  }

  return isClosureVirgin;
}

static inline GC_numReferences countReferences (GC_header header) {
  if (header == GC_FORWARDED)
    return GLOBAL_MANY;
  return (header & VIRGIN_MASK) >> VIRGIN_SHIFT;
}


void GC_score (GC_state s, pointer lhs, pointer rhs) {
  GC_header h = getHeader (rhs);
  GC_header* hp = getHeaderp (rhs);
  GC_numReferences nr = countReferences (h);

  assert (isPointerInHeap (s, s->heap, lhs) ||
          isPointerInHeap (s, s->sharedHeap, lhs));
  assert (isPointerInHeap (s, s->heap, rhs) ||
          isPointerInHeap (s, s->sharedHeap, rhs));

  if (nr == ZERO) {
    *hp = (h & ~VIRGIN_MASK) | ONE_REF;
    fprintf (stderr, "GC_score: LHS="FMTPTR" RHS="FMTPTR" %s [%d]\n",
           (uintptr_t)lhs, (uintptr_t)rhs, numReferencesToString (countReferences (*hp)),
           s->procId);
    return;
  }

  bool isLHSInSession = (lhs >= s->sessionStart) && (lhs < s->frontier);
  bool isRHSInSession = (rhs >= s->sessionStart) && (rhs < s->frontier);

  if ((nr == ONE || nr == LOCAL_MANY) && isLHSInSession && isRHSInSession)
    *hp = (h & ~VIRGIN_MASK) | LOCAL_MANY_REF;
  else
    *hp = (h & ~VIRGIN_MASK) | GLOBAL_MANY_REF;

  fprintf (stderr, "GC_score: LHS="FMTPTR" RHS="FMTPTR" %s [%d]\n",
           (uintptr_t)lhs, (uintptr_t)rhs, numReferencesToString (countReferences (*hp)),
           s->procId);
}
