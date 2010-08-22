/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

FILE* dotFile;

bool GC_canMoveDeferredSpawns (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    if (s->canMoveDeferredSpawns) {
        s->canMoveDeferredSpawns = FALSE;
        return TRUE;
    }
    return FALSE;
}


void createNode(GC_state s, objptr* opp);

void createNode(GC_state s, objptr *opp) {
    pointer p;
    p = objptrToPointer (*opp, s->heap->start);

    if (isPointerMarked (p)) return;

    GC_objectTypeTag tag;
    uint16_t bytesNonObjptrs, numObjptrs;
    bool hasIdentity;
    GC_header* headerp = getHeaderp (p);
    GC_header header = *headerp;
    splitHeader(s, header, &tag, &hasIdentity,
                &bytesNonObjptrs, &numObjptrs);

    //Mark this object
    header ^= MARK_MASK;
    *headerp = header;

    fprintf (stderr, "ObjectType = %s\n", objectTypeTagToString(tag));
    if (tag != NORMAL_TAG) return;

    pointer it = p + bytesNonObjptrs;
    pointer max = p + (numObjptrs * OBJPTR_SIZE);

    for ( ; it < max; it += OBJPTR_SIZE) {
        if (BOGUS_OBJPTR != *(objptr*)it)
            fprintf(dotFile, "_%08"PRIxOBJPTR" -> ""_%08"PRIxOBJPTR";\n", *opp, *(objptr*)it);
        callIfIsObjptr (s, (GC_foreachObjptrFun)createNode, (objptr*)it);
    }

}

void GC_walk (GC_state s, pointer p) {
      GC_objectTypeTag tag;
      uint16_t bytesNonObjptrs, numObjptrs;
      bool hasIdentity;
      GC_header header = getHeader (p);
      splitHeader(s, getHeader(p), &tag, &hasIdentity,
                  &bytesNonObjptrs, &numObjptrs);
    fprintf (stderr,
             "splitHeader ("FMTHDR")"
             "  tag = %s"
             "  hasIdentity = %s"
             "  bytesNonObjptrs = %"PRIu16
             "  numObjptrs = %"PRIu16"\n",
             header,
             objectTypeTagToString(tag),
             boolToString(hasIdentity),
             bytesNonObjptrs, numObjptrs);

    dotFile = fopen("objectLayout.dot","w");
    fprintf(dotFile, "digraph G {\nsize=\"4,4\";\n");
    objptr opp = pointerToObjptr(p, s->heap->start);
    createNode(s, &opp);
    GC_size (s,p);
    fprintf(dotFile, "}\n");
    fclose(dotFile);
}

void GC_moveToTestHeap (pointer p) {

    //Set up the forward state
    GC_state s = pthread_getspecific (gcstate_key);
    s->forwardState.toStart = s->testHeap.start;
    s->forwardState.toLimit = s->testHeap.availableSize;
    s->amInMinorGC = true;
    s->forwardState.back = s->testHeap.frontier;

    forwardObjptr (s, p);
}
