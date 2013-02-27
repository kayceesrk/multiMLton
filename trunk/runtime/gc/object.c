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

bool objectHasIdentityTransitive (GC_state s, GC_header header) {
  assert (header != GC_FORWARDED);
  header &= ~(LIFT_MASK | VIRGIN_MASK);
  unsigned objectTypeIndex =  (header & TYPE_INDEX_MASK) >> TYPE_INDEX_SHIFT;
  GC_objectType objectType = &(s->objectTypes[objectTypeIndex]);
  return objectType->hasIdentityTransitive;
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


void checkHeader (GC_state s, pointer p, char* file, int line) {
  if (!isPointer (p) || p == (pointer)s->generationalMaps.cardMapAbsolute)
    return;

  if (FALSE) {
    fprintf (stderr, "CheckHeader: %s:%d P = "FMTPTR"\n", file, line, (uintptr_t)p);
  }
}
