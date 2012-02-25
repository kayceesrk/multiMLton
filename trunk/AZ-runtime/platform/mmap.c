static inline void* mmapAnon (void *start, size_t length) {
  start = malloc (length);
  //fprintf (stderr, "mmapAnon %p\n", start);
  return start;
}

static void munmap_safe (void *base, size_t length) {
  //fprintf (stderr, "munmap_safe %p\n", base);
  free (base);
  return;
}
