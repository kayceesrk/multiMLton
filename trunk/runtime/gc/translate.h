/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_translateState {
  pointer from;
  pointer to;
  size_t size;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void translateObjptrLocal (GC_state s, objptr *opp);
static void translateHeap (GC_state s, pointer from, pointer to, size_t size);
static void translateObjptrShared (GC_state s, objptr *opp);
static void translateSharedHeap (GC_state s, pointer from, pointer to, size_t size);
static void translateObjptrInRange (GC_state s, objptr *opp);
static void translateRange (GC_state s, pointer from, pointer to, size_t size);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
