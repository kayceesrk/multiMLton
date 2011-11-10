/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

const char* objectTypeTagToString (GC_objectTypeTag tag) {
  switch (tag) {
  case ARRAY_TAG:
    return "ARRAY";
  case NORMAL_TAG:
    return "NORMAL";
  case STACK_TAG:
    return "STACK";
  case WEAK_TAG:
    return "WEAK";
  case HEADER_ONLY_TAG:
    return "HEADER_ONLY";
  case FILL_TAG:
    return "FILL";
  default:
    die ("bad GC_objectTypeTag %u", tag);
  }
}

/* getHeaderp (p)
 *
 * Returns a pointer to the header for the object pointed to by p.
 */
GC_header* getHeaderp (pointer p) {
  return (GC_header*)(p
                      - GC_HEADER_SIZE);
}

/* getHeader (p)
 *
 * Returns the header for the object pointed to by p.
 */
GC_header getHeader (pointer p) {
  return *(getHeaderp(p));
}

/*
 * Build the header for an object, given the index to its type info.
 */
GC_header buildHeaderFromTypeIndex (uint32_t t) {
  assert (t < TWOPOWER (TYPE_INDEX_BITS));
  return 1 | (t << 1);
}

bool objectHasIdentity (GC_state s, GC_header header) {
  assert (header != GC_FORWARDED);
  header &= ~(LIFT_MASK | VIRGIN_MASK);
  unsigned objectTypeIndex =  (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  GC_objectType objectType = &(s->objectTypes[objectTypeIndex]);
  return objectType->hasIdentity;
}

void splitHeader(GC_state s, GC_header header, __attribute__((unused)) GC_header* headerpunused,
                 GC_objectTypeTag *tagRet, bool *hasIdentityRet,
                 uint16_t *bytesNonObjptrsRet, uint16_t *numObjptrsRet,
                 bool *hasIdentityTransitiveRet, bool *isUnboundedRet) {
  unsigned int objectTypeIndex;
  GC_objectType objectType;
  GC_objectTypeTag tag;
  bool hasIdentity, hasIdentityTransitive, isUnbounded;
  uint16_t bytesNonObjptrs, numObjptrs;
  GC_header* headerp = NULL;

  assert (header != GC_FORWARDED);
  header &= ~(LIFT_MASK | VIRGIN_MASK);

  if (DEBUG_DETAILED)
      fprintf (stderr, "splitHeader p="FMTPTR" ("FMTHDR") [%d]\n", (uintptr_t)headerp, header, s->procId);
  assert (1 == (header & GC_VALID_HEADER_MASK));
  objectTypeIndex = (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  if (objectTypeIndex > s->objectTypesLength)
      fprintf (stderr, "objectTypeIndex : %u s->objectTypesLength : %u\n", objectTypeIndex, s->objectTypesLength);
  assert (objectTypeIndex < s->objectTypesLength);
  objectType = &(s->objectTypes[objectTypeIndex]);
  tag = objectType->tag;
  hasIdentity = objectType->hasIdentity;
  hasIdentityTransitive = objectType->hasIdentityTransitive;
  isUnbounded = objectType->isUnbounded;
  bytesNonObjptrs = objectType->bytesNonObjptrs;
  numObjptrs = objectType->numObjptrs;

  if (DEBUG_DETAILED)
    fprintf (stderr,
             "splitHeader ("FMTHDR")"
             "  objectTypeIndex = %u"
             "  tag = %s"
             "  hasIdentity = %s"
             "  hasIdentityTransitive = %s"
             "  isUnbounded = %s"
             "  bytesNonObjptrs = %"PRIu16
             "  numObjptrs = %"PRIu16"\n",
             header,
             objectTypeIndex,
             objectTypeTagToString(tag),
             boolToString(hasIdentity),
             boolToString(hasIdentityTransitive),
             boolToString(isUnbounded),
             bytesNonObjptrs, numObjptrs);

  if (tagRet != NULL)
    *tagRet = tag;
  if (hasIdentityRet != NULL)
    *hasIdentityRet = hasIdentity;
  if (bytesNonObjptrsRet != NULL)
    *bytesNonObjptrsRet = bytesNonObjptrs;
  if (numObjptrsRet != NULL)
    *numObjptrsRet = numObjptrs;
  if (hasIdentityTransitiveRet != NULL)
    *hasIdentityTransitiveRet = hasIdentityTransitive;
  if (isUnboundedRet != NULL)
    *isUnboundedRet = isUnbounded;
}

/* advanceToObjectData (s, p)
 *
 * If p points at the beginning of an object, then advanceToObjectData
 * returns a pointer to the start of the object data.
 */
pointer advanceToObjectData (__attribute__ ((unused)) GC_state s, pointer p) {
  GC_header header;
  pointer res;

  assert (isFrontierAligned (s, p));
  header = *(GC_header*)p;
  if (0 == header)
    /* Looking at the counter word in an array. */
    res = p + GC_ARRAY_HEADER_SIZE;
  else
    /* Looking at a header word. */
    res = p + GC_NORMAL_HEADER_SIZE;
  assert (isAligned ((uintptr_t)res, s->alignment));
  if (DEBUG_DETAILED)
    fprintf (stderr, FMTPTR" = advanceToObjectData ("FMTPTR")\n",
             (uintptr_t)res, (uintptr_t)p);
  return res;
}

pointer getBeginningOfObject (GC_state s, pointer p) {
  GC_header header;
  pointer res;
  GC_objectTypeTag tag;

  assert (isFrontierAligned (s, p));
  objptr op = pointerToObjptr (p, s->heap->start);
  fixFwdObjptr (s, &op);
  header = getHeader ((pointer)op);

  splitHeader (s, header, getHeaderp (p), &tag, NULL, NULL, NULL, NULL, NULL);
  if (tag == ARRAY_TAG)
    res = p - GC_ARRAY_HEADER_SIZE;
  else
    res = p - GC_NORMAL_HEADER_SIZE;
  assert (isAligned ((uintptr_t)res, s->alignment));
  if (DEBUG_DETAILED)
    fprintf (stderr, FMTPTR" = getBeginningOfObject ("FMTPTR")\n",
             (uintptr_t)res, (uintptr_t)p);
  return res;
}

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

void checkHeader (__attribute__((unused)) GC_state s, pointer p, char* file, int line) {
  if ((getHeader (p) & ~(VIRGIN_MASK | LIFT_MASK)) == (GC_header)0x11) {
    fprintf (stderr, "%s:%d P = "FMTPTR"\n", file, line, (uintptr_t)p);
  }
}
