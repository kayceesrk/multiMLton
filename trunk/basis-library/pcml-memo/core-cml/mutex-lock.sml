structure MutexLock :> MUTEX_LOCK =
struct
  structure L = Lock
  structure S = Scheduler

  type mutexlock = L.cmlLock

  val initLock = L.initCmlLock

  fun getLock lock =
    L.getCmlLock lock (S.tidNum ())

  fun releaseLock lock =
    L.releaseCmlLock lock (S.tidNum())

end
