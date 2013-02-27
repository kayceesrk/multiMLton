/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef enum {
  MARK_MODE,
  UNMARK_MODE,
} GC_markMode;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

bool isPointerMarked (pointer p);
bool isPointerMarkedByMode (pointer p, GC_markMode m);
size_t dfsMarkByMode (GC_state s, pointer root,
                      GC_foreachObjectDfsFun f,
                      GC_markMode mode,
                      bool shouldHashCons,
                      bool shouldLinkWeaks,
                      bool ignoreSharedHeap,
                      bool sizeEstimationForLifting);
void dfsMarkWithHashConsWithLinkWeaks (GC_state s, objptr *opp);
void dfsMarkWithoutHashConsWithLinkWeaks (GC_state s, objptr *opp);
void dfsMarkTraceShared (GC_state s, objptr *opp);
void dfsUnmark (GC_state s, objptr *opp);
bool emptyForeachObjectFun (GC_state s, pointer current, pointer parent);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
