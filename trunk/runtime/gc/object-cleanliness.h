/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool objectHasIdentity (GC_state s, GC_header h);
static inline void isObjectPointerVirgin (GC_state s, pointer p);
static inline void doesPointToTmpPointer (GC_state s, objptr* opp);
PRIVATE bool GC_isClosureVirgin (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
