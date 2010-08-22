signature MUTEX_LOCK =
sig
  type mutexlock

  val initLock : unit -> mutexlock
  val getLock : mutexlock -> unit
  val releaseLock : mutexlock -> unit

end
