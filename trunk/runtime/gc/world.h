/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void loadWorldFromFILE (GC_state s, FILE *f);
static void loadWorldFromFileName (GC_state s, const char *fileName);
static int saveWorldToFILE (GC_state s, FILE *f);
static int saveArray (void* a, size_t elSize, int32_t aSize, int32_t aMaxSize, FILE* f);
static int saveCircularBuffer (CircularBuffer* que, FILE* f);
static void loadArray (void* a, size_t elSize, int32_t* aSize, int32_t* aMaxSize, FILE* f);
static void loadCircularBuffer (CircularBuffer* que, FILE* f);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_saveWorld (GC_state s, NullString8_t fileName);
/* TRUE = success, FALSE = failure */
PRIVATE C_Errno_t(Bool_t) GC_getSaveWorldStatus (GC_state *gs);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
