/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void initPointerToCore (GC_state s, size_t size) {
  if (Proc_processorNumber (s) != 0) {
    fprintf (stderr, "initPointerToCore: Must be called only by processor 0\n");
    exit (1);
  }

  int sz=size*sizeof(PointerToCore)+sizeof(PointerToCoreMap);
  PointerToCoreMap* map = (PointerToCoreMap*) GC_shmalloc (sz);
  if (map == NULL) {
    fprintf (stderr, "initPointerToCore: GC_shmalloc with size %zu failed\n", sz);
    exit(1);
  }
  map->maxSize = size;
  map->size = 0;
  s->pointerToCoreMap = map;
}

//Must be called by each processor
void finalizePointerToCore (GC_state s) {
  if (Proc_processorNumber (s) != 0) {
    fprintf (stderr, "finalizePointerToCore: Must be called only by processor 0\n");
    exit (1);
  }
  RCCE_shfree ((void*)s->pointerToCoreMap);
  s->pointerToCoreMap = NULL;
}

/* We will only remember pointers p in local heap, whose opp is in shared heap. */
void insertPointerToCore (GC_state s, pointer p, uint32_t core, bool isDanglingStack) {
  Parallel_lock (0);
  assert (isPointerInHeap (s, s->heap, p));
  PointerToCoreMap* map = s->pointerToCoreMap;
  for (size_t i=0; i < map->size; i++) {
    if (map->elem[i].p == p) {
      if (map->elem[i].core == core) {
        Parallel_unlock (0);
        return;
      }
      else if (map->elem[i].isDanglingStack && !isDanglingStack) {
        /* This condition to necessary to avoid adding adding the dangling
         * stack of another core to the current core's
         * pointerToDanglingStackList */
        Parallel_unlock (0);
        return;
      }
      else {
        assert (map->elem[i].core == core);
        fprintf (stderr, "insertPointerToCore: pointer "FMTPTR" already associated with core %u\n",
                 (uintptr_t)p, map->elem[i].core);
        exit (1);
      }
    }
  }

  if (map->size == map->maxSize) {
    fprintf (stderr, "insertPointerToCore: full\n");
    exit (1);
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "insertPointerToCore: pointer="FMTPTR" core=%d\n",
             (uintptr_t)p, core);
  map->elem[map->size].p = p;
  map->elem[map->size].core = core;
  map->elem[map->size].isDanglingStack = isDanglingStack;
  map->size++;
  Parallel_unlock (0);
}

static int32_t getCoreIdFromPointer (GC_state s, pointer p) {
  PointerToCoreMap* map = s->pointerToCoreMap;
  for (size_t i=0; i < map->size; i++) {
    if (map->elem[i].p == p)
      return map->elem[i].core;
  }
  assert (0 && "cannot find pointer in pointerToCoreMap");
  exit (1);
}

static int32_t getCoreIdFromPointer_safe (GC_state s, pointer p) {
  PointerToCoreMap* map = s->pointerToCoreMap;
  for (size_t i=0; i < map->size; i++) {
    if (map->elem[i].p == p)
      return map->elem[i].core;
  }
  return -1;
}

static int32_t testPointerIsDanglingStack (GC_state s, pointer p) {
  PointerToCoreMap* map = s->pointerToCoreMap;
  for (size_t i=0; i < map->size; i++) {
    if (map->elem[i].p == p) {
      if (map->elem[i].isDanglingStack)
        return TRUE;
      else
        return FALSE;
    }
  }
  assert (0 && "cannot find pointer in pointerToCoreMap");
  exit (1);
}



static bool isPointerInAnotherCore (GC_state s, objptr* opp, pointer p) {
  int coreId = getCoreIdFromPointer_safe (s, p);
  if (!isPointerInHeap (s, s->sharedHeap, (pointer)opp))
    fprintf (stderr, "isPointerInAnotherCore: opp must be in the shared heap.\n");
  if (coreId != -1 && coreId != Proc_processorNumber (s))
    return TRUE;
  return FALSE;
}
