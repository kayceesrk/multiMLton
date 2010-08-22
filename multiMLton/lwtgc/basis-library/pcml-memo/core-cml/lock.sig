signature LOCK =
sig
  type lock
  type cmlLock

  val initCmlLock : unit -> cmlLock
  val getCmlLock : cmlLock -> int -> unit
  val releaseCmlLock : cmlLock -> int -> unit

  val initLock : unit -> lock
  val getLock : lock -> unit
  val releaseLock : lock -> unit

  val fetchAndAdd : int ref * int -> int
end
