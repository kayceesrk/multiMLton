/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE pointer GC_move (GC_state s, pointer object);
PRIVATE void GC_addToMoveOnWBA (GC_state s, pointer p);
PRIVATE void GC_addToPreemptOnWBA (GC_state s, pointer p);

void moveEachObjptrInObject (GC_state s, pointer object);
void liftAllObjectsDuringInit (GC_state s);
void liftAllObjptrsInMoveOnWBA (GC_state s);
static inline void assertLiftedObjptr (GC_state s, objptr *opp);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */


#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void foreachObjptrInWBAs (GC_state s, GC_state fromState,
                                        GC_foreachObjptrFun f);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
