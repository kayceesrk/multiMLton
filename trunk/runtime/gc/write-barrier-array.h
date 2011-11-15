/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_addToMoveOnWBA (GC_state s, pointer p);
//Kind is 0 -- HOST or 1 -- PARASITE
PRIVATE void GC_addToPreemptOnWBA (GC_state s, pointer p, int kind);
PRIVATE void GC_addToSpawnOnWBA (GC_state s, pointer p, int proc);

void liftAllObjptrsInMoveOnWBA (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void foreachObjptrInWBAs (GC_state s, GC_state fromState, GC_foreachObjptrFun f);
static inline void foreachObjptrInExportableWBAs (GC_state s, GC_state fromState, GC_foreachObjptrFun f);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
