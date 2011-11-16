/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/**< Circular Buffer Types */
typedef objptr KeyType;

typedef struct {
    uint32_t writePointer; /**< write pointer */
    uint32_t readPointer;  /**< read pointer */
    size_t size;         /**< size of circular buffer */
    KeyType keys[];    /**< Element of ciruclar buffer */
} CircularBuffer;

typedef struct {
  CircularBuffer* primary;
  CircularBuffer* secondary;
  CircularBuffer* parasites;
} SchedulerQueue;

typedef struct {
  int32_t count;
  int32_t id;
} Lock;

typedef struct {
  objptr op;
  int proc;
} SpawnThread;

typedef enum {
  HOST,
  PARASITE,
} ThreadKind;

typedef struct {
  objptr op;
  ThreadKind kind;
} PreemptThread;

typedef struct {
  pointer p;
  size_t size;
} ClosureToSpawn;


static UT_icd directCloXfer_icd = {sizeof (ClosureToSpawn), NULL, NULL, NULL};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_sqAcquireLock (GC_state s, int proc);
PRIVATE void GC_sqReleaseLock (GC_state s, int proc);
PRIVATE void GC_sqCreateQueues (GC_state s);
void sqEnque (GC_state s, pointer p, int proc, int i);
void flushFromDirectCloXferArray (GC_state s);
PRIVATE void GC_sqEnque (GC_state s, pointer p, int proc, int i);
PRIVATE pointer GC_sqDeque (GC_state s, int queue);
PRIVATE bool GC_sqIsEmpty (GC_state s);
PRIVATE bool GC_sqIsEmptyPrio (int i);
PRIVATE void GC_sqClean (GC_state s);
int sizeofSchedulerQueue (GC_state s, int i);
void GC_addToDirectCloXferArray (GC_state s, ClosureToSpawn c, int proc);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */


#if (defined (MLTON_GC_INTERNAL_FUNCS))

void foreachObjptrInSQ (GC_state s, SchedulerQueue* sq, GC_foreachObjptrFun f);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
