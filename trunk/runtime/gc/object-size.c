/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

size_t sizeofArrayNoHeader (GC_state s,
                            GC_arrayLength numElements,
                            uint16_t bytesNonObjptrs, uint16_t numObjptrs) {
  size_t result;

  result = numElements * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
  /* Very small (including empty) arrays have OBJPTR_SIZE bytes for
   * the forwarding pointer.
   */
  if (result < OBJPTR_SIZE)
    result = OBJPTR_SIZE;
  return alignWithExtra (s, result, GC_ARRAY_HEADER_SIZE);
}

size_t sizeofStackNoHeader (__attribute__ ((unused)) GC_state s,
                            GC_stack stack) {
  size_t result;

  result = sizeof (struct GC_stack) + stack->reserved;
  return result;
}

size_t sizeofObject (GC_state s, pointer p) {
  size_t headerBytes, objectBytes;
  GC_header header;
  GC_objectTypeTag tag;
  pointer oldP = p;
  uint16_t bytesNonObjptrs, numObjptrs;

  header = getHeader (p);
  while (header == GC_FORWARDED) {
    if (DEBUG_DETAILED)
      fprintf (stderr,
               "sizeOfObject saw forwarded object "FMTPTR" [%d]\n",
               (uintptr_t)p, s->procId);
    objptr op = *((objptr*)p);
    p = objptrToPointer (op, s->sharedHeap->start);
    header = getHeader (p);
  }
  while ((header & 1) == 0) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "sizeofObject saw threaded header("FMTHDR") for object "FMTPTR" [%d]\n",
               header, (uintptr_t)p, s->procId);
    header = *(GC_header*)header;
  }
  splitHeader (s, header, getHeaderp (p), &tag, NULL,
               &bytesNonObjptrs, &numObjptrs, NULL, NULL);
  if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) {
    headerBytes = GC_NORMAL_HEADER_SIZE;
    objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  } else if (ARRAY_TAG == tag) {
    headerBytes = GC_ARRAY_HEADER_SIZE;
    objectBytes = sizeofArrayNoHeader (s, getArrayLength (p),
                                       bytesNonObjptrs, numObjptrs);
  } else if (STACK_TAG == tag) {
    headerBytes = GC_STACK_HEADER_SIZE;
    objectBytes = sizeofStackNoHeader (s, (GC_stack)oldP);
  }
  else if (HEADER_ONLY_TAG == tag) {
    headerBytes = GC_HEADER_SIZE;
    objectBytes = 0;
  }
  else if (FILL_TAG == tag) {
    GC_smallGapSize bytes;
    headerBytes = GC_HEADER_SIZE;
    bytes = *((GC_smallGapSize *)p);
    objectBytes = GC_SMALL_GAP_SIZE_SIZE + bytes;
  }
  else {
    headerBytes = 0;
    objectBytes = 0;
    assert (0 and "unknown tag in sizeofObject");
  }
  assert (isAligned (headerBytes + objectBytes, s->alignment));
  return headerBytes + objectBytes;
}

size_t sizeofObjectNoHeader (GC_state s, pointer p) {
  size_t objectBytes;
  GC_header header;
  GC_objectTypeTag tag;
  pointer oldP = p;
  uint16_t bytesNonObjptrs, numObjptrs;

  header = getHeader (p);
  while (header == GC_FORWARDED) {
    if (DEBUG_DETAILED)
      fprintf (stderr,
               "sizeOfObjectNoHeader saw forwarded object "FMTPTR" [%d]\n",
               (uintptr_t)p, s->procId);
    objptr op = *((objptr*)p);
    p = objptrToPointer (op, s->sharedHeap->start);
    header = getHeader (p);
  }
  while ((header & 1) == 0) {
    if (DEBUG_DETAILED)
      fprintf (stderr, "sizeofObjectNoHeader saw threaded header("FMTHDR") for object "FMTPTR" [%d]\n",
               header, (uintptr_t)p, s->procId);
    header = *(GC_header*)header;
  }
  splitHeader (s, header, getHeaderp (p), &tag, NULL,
               &bytesNonObjptrs, &numObjptrs, NULL, NULL);
  if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) {
    objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  } else if (ARRAY_TAG == tag) {
    objectBytes = sizeofArrayNoHeader (s, getArrayLength (p),
                                       bytesNonObjptrs, numObjptrs);
  } else if (STACK_TAG == tag) {
    objectBytes = sizeofStackNoHeader (s, (GC_stack)oldP);
  }
  else if (HEADER_ONLY_TAG == tag) {
    objectBytes = 0;
  }
  else if (FILL_TAG == tag) {
    GC_smallGapSize bytes;
    bytes = *((GC_smallGapSize *)p);
    objectBytes = GC_SMALL_GAP_SIZE_SIZE + bytes;
  }
  else {
    objectBytes = 0;
    assert (0 and "unknown tag in sizeofObject");
  }
  return objectBytes;
}

size_t sizeofObjectHeader (GC_state s, GC_header header) {
  GC_objectTypeTag tag;
  assert (header != GC_FORWARDED);
  splitHeader (s, header, NULL, &tag, NULL,
               NULL, NULL, NULL, NULL);
  if (tag == ARRAY_TAG)
    return GC_ARRAY_HEADER_SIZE;
  else
    return GC_NORMAL_HEADER_SIZE;
}
