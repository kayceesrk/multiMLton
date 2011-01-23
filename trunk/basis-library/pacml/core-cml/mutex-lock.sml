structure MutexLock :> MUTEX_LOCK =
struct
  structure L = Lock

  type mutexlock = L.cmlLock

  val initLock = L.initCmlLock

  fun getLock lock =
    (MLtonThread.atomicBegin ();
     L.getCmlLock lock ThreadID.tidNum)

  fun releaseLock lock =
    (L.releaseCmlLock lock (ThreadID.tidNum);
    MLtonThread.atomicEnd ())

  val fetchAndAdd = PacmlFFI.fetchAndAdd
end
