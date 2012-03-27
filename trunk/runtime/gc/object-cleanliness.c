/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

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
  bool isClean = FALSE;
  GC_header h = getHeader (p);

  if (s->tmpPointer == p)
    isClean = (getNumReferences (h) == ZERO);
  else
    isClean = (getNumReferences (h) <= ONE);

  if (!isClean && !objectHasIdentity(s, h) && s->controls->useIdentityForCleanliness)
    isClean = TRUE;

  s->tmpBool = s->tmpBool && isClean;

  return s->tmpBool;
}

bool isSpawnCleanMark (GC_state s, pointer p, pointer parent) {
  parent = parent; //To silence GCC warnings

  assert (s->tmpPointer != BOGUS_POINTER);
  bool isClean = FALSE;
  GC_header h = getHeader (p);
  GC_numReferences n = getNumReferences (h);

  if (n == ZERO) //p is clean if it has 0 references
    isClean = TRUE;
  else if (n == ONE) //If p not root, p is clean if it has 1 refs
    isClean = (p != s->tmpPointer);
  else if (n == LOCAL_MANY) //If p has locally many refs, then p must reside in the current session to be clean
    isClean = (p > s->sessionStart && p < s->frontier);
  else //If p has globally many refs, p is not clean
    isClean = FALSE;

  if (!isClean && !objectHasIdentity(s, h) && s->controls->useIdentityForCleanliness)
    isClean = TRUE;

  s->tmpBool = s->tmpBool && isClean;

  return s->tmpBool;
}


/* doesCurrentStackPointerToMarkedObject
 * -------------------------
 * s->tmpPointer is the root of the thread closure
 * s->tmpBool is isClosureVirgin predicate
 * s->tmpInt counts the number of troublesome pointers from stack
 */
void doesCurrentStackPointToMarkedObject (GC_state s, objptr* opp) {
  pointer p = objptrToPointer (*opp, s->heap->start);
  if (isPointerMarked (p) && p != s->tmpPointer &&
      objectHasIdentityTransitive (s, getHeader(p))) {
    s->tmpInt++;
  }
}

void doesPointToMarkedObject (GC_state s, objptr* opp) {
  pointer p = objptrToPointer (*opp, s->heap->start);
  if (isPointerMarked (p) && objectHasIdentityTransitive (s, getHeader(p)))
    s->tmpInt++;
}

bool foreachObjptrInUnmarkedObject (GC_state s, pointer p) {
  GC_header header = getHeader (p);
  unsigned int objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  if (objectTypeIndex == 0 || isPointerMarked (p))
    return TRUE; //Contiue with next object
  foreachObjptrInObject (s, p, doesPointToMarkedObject, TRUE);
  return TRUE;
}

bool GC_isThreadClosureClean (GC_state s, pointer p) {
  size_t size;
  return __GC_isThreadClosureClean (s, p, &size);
}

bool __GC_isThreadClosureClean (GC_state s, pointer p, size_t* size) {
  bool hasIdentityTransitive, isUnbounded, isClosureVirgin;

  GC_header header = getHeader (p);
  GC_objectTypeTag tag;
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
    isClosureVirgin = s->tmpBool;
    dfsMarkByMode (s, p, isSpawnCleanUnmark, UNMARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);

    //Reset tmp* variables
    s->tmpPointer = BOGUS_POINTER;
    s->tmpBool = TRUE;
    s->tmpInt = 0;
  }

  if (DEBUG_CLEANLINESS)
    fprintf (stderr, "GC_isThreadClosureClean: sessionSize = %zu "
                     "objectSize = %zu isClosureVirgin = %d [%d]\n",
             (size_t)(s->frontier - s->sessionStart), *size, isClosureVirgin, s->procId);

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

static inline GC_numReferences getNumReferences (const GC_header header) {
  if (header == GC_FORWARDED)
    return GLOBAL_MANY;
  return (header & VIRGIN_MASK) >> VIRGIN_SHIFT;
}

void setNumReferences (GC_header* headerp, GC_numReferences count) {
  GC_header h = *headerp;
  *headerp = (h & ~VIRGIN_MASK) | (count << VIRGIN_SHIFT);
}
