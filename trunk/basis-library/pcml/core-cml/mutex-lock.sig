signature MUTEX_LOCK =
sig

  type mutexlock

  val initLock : unit -> mutexlock
  val getLock : mutexlock -> unit
  val releaseLock : mutexlock -> unit
  val fetchAndAdd: int ref * int -> int
end
