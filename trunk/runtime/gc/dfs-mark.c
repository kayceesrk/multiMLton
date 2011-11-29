/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                       Depth-first Marking                        */
/* ---------------------------------------------------------------- */

inline bool isPointerMarked (pointer p) {
  return MARK_MASK & getHeader (p);
}

bool isPointerMarkedByMode (pointer p, GC_markMode m) {
  switch (m) {
  case MARK_MODE:
    return isPointerMarked (p);
  case UNMARK_MODE:
    return not isPointerMarked (p);
  default:
    die ("bad mark mode %u", m);
  }
}

/* dfsMarkByMode (s, r, m, shc, slw, ish)
 *
 * Sets all the mark bits in the object graph pointed to by r.
 *
 * If m is MARK_MODE, it sets the bits to 1.
 * If m is UNMARK_MODE, it sets the bits to 0.
 *
 * If shc, it hash-conses the objects marked.
 *
 * If slw, it links the weak objects marked.
 *
 * If ish, then dfs will be limited to local heap.
 *
 * It returns the total size in bytes of the objects marked.
 */
size_t dfsMarkByMode (GC_state s, pointer root,
                      GC_foreachObjectDfsFun f,
                      GC_markMode mode,
                      bool shouldHashCons,
                      bool shouldLinkWeaks,
                      bool ignoreSharedHeap,
                      bool sizeEstimationForLifting) {
  GC_header mark; /* Used to set or clear the mark bit. */
  size_t size; /* Total number of bytes marked. */
  pointer cur; /* The current object being marked. */
  pointer prev; /* The previous object on the mark stack. */
  pointer next; /* The next object to mark. */
  pointer todo; /* A pointer to the pointer in cur to next. */
  GC_header header;
  GC_header* headerp;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  uint32_t objptrIndex; /* The i'th pointer in the object (element) being marked. */
  GC_header nextHeader;
  GC_header* nextHeaderp;
  GC_arrayCounter arrayIndex;
  pointer top; /* The top of the next stack frame to mark. */
  GC_returnAddress returnAddress;
  GC_frameLayout frameLayout;
  GC_frameOffsets frameOffsets;

  if (isPointerMarkedByMode (root, mode))
    /* Object has already been marked. */
    return 0;

  if (getHeader (root) == GC_FORWARDED) {
    if (DEBUG_DFS_MARK)
      fprintf (stderr, "dfsMarkByMode saw forwarded object "FMTPTR" [%d]\n",
               (uintptr_t)root, s->procId);
    objptr op = pointerToObjptr (root, s->heap->start);
    fixFwdObjptr (s, &op);
    root = objptrToPointer (op, s->heap->start);
  }

  if (ignoreSharedHeap and isPointerInHeap (s, s->sharedHeap, root)) {
      /* Object resides in the shared heap. Do not collect */
      if (DEBUG_LWTGC)
          fprintf (stderr, "dfsMarkByMode p = "FMTPTR"already LIFTED\n", (uintptr_t)root);
      return 0;
  }

  mark = (MARK_MODE == mode) ? MARK_MASK : 0;
  size = 0;
  cur = root;
  prev = NULL;
  headerp = getHeaderp (cur);
  header = *headerp;
  goto mark;
markNext:
  /* cur is the object that was being marked.
   * prev is the mark stack.
   * next is the unmarked object to be marked.
   * nextHeaderp points to the header of next.
   * nextHeader is the header of next.
   * todo is a pointer to the pointer inside cur that points to next.
   */
  if (DEBUG_DFS_MARK)
    fprintf (stderr,
             "markNext"
             "  cur = "FMTPTR"  next = "FMTPTR
             "  prev = "FMTPTR"  todo = "FMTPTR"\n",
             (uintptr_t)cur, (uintptr_t)next,
             (uintptr_t)prev, (uintptr_t)todo);
  assert (not isPointerMarkedByMode (next, mode));
  assert (nextHeaderp == getHeaderp (next));
  assert (nextHeader == getHeader (next));
  // assert (*(pointer*) todo == next);
  assert (fetchObjptrToPointer (s, todo, s->heap->start) == next);
  headerp = nextHeaderp;
  header = nextHeader;
  // *(pointer*)todo = prev;
  storeObjptrFromPointer (todo, prev, s->heap->start);
  prev = cur;
  cur = next;
mark:
  if (DEBUG_DFS_MARK)
    fprintf (stderr, "mark  cur = "FMTPTR"  prev = "FMTPTR"  mode = %s\n",
             (uintptr_t)cur, (uintptr_t)prev,
             (mode == MARK_MODE) ? "mark" : "unmark");
  /* cur is the object to mark.
   * prev is the mark stack.
   * headerp points to the header of cur.
   * header is the header of cur.
   */
  assert (not isPointerMarkedByMode (cur, mode));
  assert (header == getHeader (cur));
  assert (headerp == getHeaderp (cur));
  /* Apply f before marking */
  bool toContinue = f (s, cur, prev);
  header ^= MARK_MASK;
  /* Store the mark.  In the case of an object that contains a pointer to
   * itself, it is essential that we store the marked header before marking
   * the internal pointers (markInNormal below).  If we didn't, then we
   * would see the object as unmarked and traverse it again.
   */
  *headerp = header;
  splitHeader (s, header, headerp, &tag, NULL, &bytesNonObjptrs, &numObjptrs, NULL, NULL);
  if (NORMAL_TAG == tag) {
    if ((not sizeEstimationForLifting) ||
        (not isPointerInHeap (s, s->sharedHeap, cur))) {
      size += GC_NORMAL_HEADER_SIZE + bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    }
    if (not toContinue)
      goto ret;
    if (0 == numObjptrs) {
      /* There is nothing to mark. */
normalDone:
      if (shouldHashCons)
        cur = hashConsPointer (s, cur, TRUE);
      goto ret;
    }
    todo = cur + bytesNonObjptrs;
    objptrIndex = 0;
markInNormal:
    if (DEBUG_DFS_MARK)
      fprintf (stderr, "markInNormal  objptrIndex = %"PRIu32"\n", objptrIndex);
    assert (objptrIndex < numObjptrs);
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (s, todo, s->heap->start);
    if ((not isPointer (next)) or
        (ignoreSharedHeap and isPointerInHeap (s, s->sharedHeap, next))) {
markNextInNormal:
      assert (objptrIndex < numObjptrs);
      objptrIndex++;
      if (objptrIndex == numObjptrs) {
        /* Done.  Clear out the counters and return. */
        *headerp = header & ~COUNTER_MASK;
        goto normalDone;
      }
      todo += OBJPTR_SIZE;
      goto markInNormal;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      if (shouldHashCons)
        shareObjptr (s, (objptr*)todo);
      goto markNextInNormal;
    }
    *headerp = (header & ~COUNTER_MASK) | (objptrIndex << COUNTER_SHIFT);
    goto markNext;
  } else if (WEAK_TAG == tag) {
    /* Store the marked header and don't follow any pointers. */
    if (shouldLinkWeaks) {
      GC_weak w;

      w = (GC_weak)(cur + offsetofWeak (s));
      if (DEBUG_WEAK)
        fprintf (stderr, "marking weak "FMTPTR" ",
                 (uintptr_t)w);
      if (isObjptr (w->objptr)) {
        if (DEBUG_WEAK)
          fprintf (stderr, "linking\n");
        w->link = s->weaks;
        s->weaks = w;
      } else {
        if (DEBUG_WEAK)
          fprintf (stderr, "not linking\n");
      }
    }
    goto ret;
  } else if (ARRAY_TAG == tag) {
    /* When marking arrays:
     *   arrayIndex is the index of the element to mark.
     *   cur is the pointer to the array.
     *   objptrIndex is the index of the pointer within the element
     *     (i.e. the i'th pointer is at index i).
     *   todo is the start of the element.
     */

    if ((not sizeEstimationForLifting) ||
        (not isPointerInHeap (s, s->sharedHeap, cur))) {
      size += GC_ARRAY_HEADER_SIZE +
        sizeofArrayNoHeader (s, getArrayLength (cur), bytesNonObjptrs, numObjptrs);
    }
    if (not toContinue)
      goto ret;
    if (0 == numObjptrs or 0 == getArrayLength (cur)) {
      /* There is nothing to mark. */
arrayDone:
      if (shouldHashCons)
        cur = hashConsPointer (s, cur, TRUE);
      goto ret;
    }
    /* Begin marking first element. */
    arrayIndex = 0;
    todo = cur;
markArrayElt:
    assert (arrayIndex < getArrayLength (cur));
    objptrIndex = 0;
    /* Skip to the first pointer. */
    todo += bytesNonObjptrs;
markInArray:
    if (DEBUG_DFS_MARK)
      fprintf (stderr, "markInArray arrayIndex = %"PRIxARRCTR" objptrIndex = %"PRIu32"\n",
               arrayIndex, objptrIndex);
    assert (arrayIndex < getArrayLength (cur));
    assert (objptrIndex < numObjptrs);
    assert (todo == indexArrayAtObjptrIndex (s, cur, arrayIndex, objptrIndex));
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (s, todo, s->heap->start);
    if ((not isPointer (next)) or
        (ignoreSharedHeap and isPointerInHeap (s, s->sharedHeap, next))) {
markNextInArray:
      assert (arrayIndex < getArrayLength (cur));
      assert (objptrIndex < numObjptrs);
      assert (todo == indexArrayAtObjptrIndex (s, cur, arrayIndex, objptrIndex));
      todo += OBJPTR_SIZE;
      objptrIndex++;
      if (objptrIndex < numObjptrs)
        goto markInArray;
      arrayIndex++;
      if (arrayIndex < getArrayLength (cur))
        goto markArrayElt;
      /* Done.  Clear out the counters and return. */
      *getArrayCounterp (cur) = 0;
      *headerp = header & ~COUNTER_MASK;
      goto arrayDone;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      if (shouldHashCons)
        shareObjptr (s, (objptr*)todo);
      goto markNextInArray;
    }
    /* Recur and mark next. */
    *getArrayCounterp (cur) = arrayIndex;
    *headerp = (header & ~COUNTER_MASK) | (objptrIndex << COUNTER_SHIFT);
    goto markNext;
  } else {
    assert (STACK_TAG == tag);

    if ((not sizeEstimationForLifting) ||
        ((not isPointerInHeap (s, s->sharedHeap, cur))
         && ((GC_stack)cur)->isParasitic)) {
      size += GC_STACK_HEADER_SIZE +
        sizeof (struct GC_stack) + ((GC_stack)cur)->reserved;
    }
    top = getStackTop (s, (GC_stack)cur);
    assert (((GC_stack)cur)->used <= ((GC_stack)cur)->reserved);
markInStack:
    /* Invariant: top points just past the return address of the frame
     * to be marked.
     */
    assert (getStackBottom (s, (GC_stack)cur) <= top);
    if (DEBUG_DFS_MARK)
      fprintf (stderr, "markInStack  top = %"PRIuMAX"\n",
               (uintmax_t)(top - getStackBottom (s, (GC_stack)cur)));
    if (not toContinue)
      goto ret;
    if (top == getStackBottom (s, (GC_stack)(cur)))
      goto ret;
    if (f == isWriteCleanMark || f == isWriteCleanUnmark || f == isSpawnCleanMark || f == isSpawnCleanUnmark) {
      s->tmpBool = FALSE;
      goto ret;
    }
    objptrIndex = 0;
    returnAddress = *(GC_returnAddress*) (top - GC_RETURNADDRESS_SIZE);
    frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
    frameOffsets = frameLayout->offsets;
    ((GC_stack)cur)->markTop = top;
markInFrame:
    if (objptrIndex == frameOffsets [0]) {
      top -= frameLayout->size;
      goto markInStack;
    }
    todo = top - frameLayout->size + frameOffsets [objptrIndex + 1];
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (s, todo, s->heap->start);
    if (DEBUG_DFS_MARK)
      fprintf (stderr,
               "    offset %u  todo "FMTPTR"  next = "FMTPTR"\n",
               frameOffsets [objptrIndex + 1],
               (uintptr_t)todo, (uintptr_t)next);
    if ((not isPointer (next)) or
        (ignoreSharedHeap and isPointerInHeap (s, s->sharedHeap, next)) or
        (sizeEstimationForLifting and (not ((GC_stack)cur)->isParasitic))) {
      objptrIndex++;
      goto markInFrame;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      objptrIndex++;
      if (shouldHashCons)
        shareObjptr (s, (objptr*)todo);
      goto markInFrame;
    }
    ((GC_stack)cur)->markIndex = objptrIndex;
    goto markNext;
  }
  assert (FALSE);
ret:
  /* Done marking cur, continue with prev.
   * Need to set the pointer in the prev object that pointed to cur
   * to point back to prev, and restore prev.
   */
  if (DEBUG_DFS_MARK)
    fprintf (stderr, "return  cur = "FMTPTR"  prev = "FMTPTR"\n",
             (uintptr_t)cur, (uintptr_t)prev);
  assert (isPointerMarkedByMode (cur, mode));
  if (NULL == prev)
    return size;
  next = cur;
  cur = prev;
  headerp = getHeaderp (cur);
  header = *headerp;
  splitHeader (s, header, headerp, &tag, NULL, &bytesNonObjptrs, &numObjptrs, NULL, NULL);
  /* It's impossible to get a WEAK_TAG here, since we would never
   * follow the weak object pointer.
   */
  assert (WEAK_TAG != tag);
  if (NORMAL_TAG == tag) {
    todo = cur + bytesNonObjptrs;
    objptrIndex = (header & COUNTER_MASK) >> COUNTER_SHIFT;
    todo += objptrIndex * OBJPTR_SIZE;
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (s, todo, s->heap->start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap->start);
    if (shouldHashCons)
      markIntergenerationalPointer (s, (pointer*)todo);
    goto markNextInNormal;
  } else if (ARRAY_TAG == tag) {
    arrayIndex = getArrayCounter (cur);
    todo = cur + arrayIndex * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    objptrIndex = (header & COUNTER_MASK) >> COUNTER_SHIFT;
    todo += bytesNonObjptrs + objptrIndex * OBJPTR_SIZE;
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (s, todo, s->heap->start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap->start);
    if (shouldHashCons)
      markIntergenerationalPointer (s, (pointer*)todo);
    goto markNextInArray;
  } else {
    assert (STACK_TAG == tag);
    objptrIndex = ((GC_stack)cur)->markIndex;
    top = ((GC_stack)cur)->markTop;
    /* Invariant: top points just past a "return address". */
    returnAddress = *(GC_returnAddress*) (top - GC_RETURNADDRESS_SIZE);
    frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
    frameOffsets = frameLayout->offsets;
    todo = top - frameLayout->size + frameOffsets [objptrIndex + 1];
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (s, todo, s->heap->start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap->start);
    if (shouldHashCons)
      markIntergenerationalPointer (s, (pointer*)todo);
    objptrIndex++;
    goto markInFrame;
  }
  assert (FALSE);
}

bool emptyForeachObjectFun (__attribute__((unused)) GC_state s,
                            __attribute__((unused)) pointer current,
                            __attribute__((unused)) pointer prev) {
  return TRUE;
}

void dfsMarkWithHashConsWithLinkWeaks (GC_state s, objptr *opp) {
  pointer p;
  fixFwdObjptr (s, opp);
  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, emptyForeachObjectFun, MARK_MODE,
                 TRUE, TRUE, TRUE, FALSE);
}

void dfsMarkWithoutHashConsWithLinkWeaks (GC_state s, objptr *opp) {
  pointer p;
  fixFwdObjptr (s, opp);
  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, emptyForeachObjectFun, MARK_MODE,
                 FALSE, TRUE, TRUE, FALSE);
}

//Similar to dfsMarkWithoutHashConsWithLinkWeaksTraceShared
void dfsMarkTraceShared (GC_state s, objptr *opp) {
  pointer p;
  fixFwdObjptr (s, opp);
  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, emptyForeachObjectFun, MARK_MODE,
                 FALSE, TRUE, FALSE, FALSE);
}


void dfsUnmark (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap->start);
  dfsMarkByMode (s, p, emptyForeachObjectFun,
                 UNMARK_MODE, FALSE, FALSE, TRUE, FALSE);
}

