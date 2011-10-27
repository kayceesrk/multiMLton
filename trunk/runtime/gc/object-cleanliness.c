/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool objectHasIdentity (GC_state s, GC_header header) {
  assert (header != GC_FORWARDED);
  header &= ~(LIFT_MASK | VIRGIN_MASK);
  unsigned objectTypeIndex =  (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  GC_objectType objectType = &(s->objectTypes[objectTypeIndex]);
  return objectType->hasIdentity;
}

/* isObjectPointerVirgin (s, p)
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
void isObjectPointerVirgin (GC_state s, pointer p) {
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

  s->tmpBool = s->tmpBool && isVirgin;
}

void doesPointToTmpPointer (GC_state s, objptr* opp) {
  pointer p = objptrToPointer (*opp, s->heap->start);
  if (p == s->tmpPointer)
    s->tmpInt++;
}

bool GC_isClosureVirgin (GC_state s, pointer p) {
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
    s->tmpInt = 0;
    dfsMarkByMode (s, p, isObjectPointerVirgin, MARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    isClosureVirgin = s->tmpBool;
    dfsMarkByMode (s, p, isObjectPointerVirgin, UNMARK_MODE,
                  FALSE, FALSE, TRUE, FALSE);
    s->tmpPointer = BOGUS_POINTER;
  }

  //numPointerFromStack is broken since it only looks for pointers from stack
  //to the root of the closure
  if (DEBUG_OBJECT_TYPE_INFO || s->selectiveDebug) {
    fprintf (stderr, "hasIdentityTransitive = %d isUnbounded = %d objectTypeIndex = %d \
                      isClosureVirgin = %d numPointerFromStack = %d\n",
                     hasIdentityTransitive, isUnbounded, objectTypeIndex,
                     isClosureVirgin, s->tmpInt);
  }

  return isClosureVirgin;
}
