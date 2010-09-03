signature LOCK =
sig
  type cmlLock

  val initCmlLock : unit -> cmlLock
  val getCmlLock : cmlLock -> (unit->int) -> unit
  val releaseCmlLock : cmlLock -> int -> unit

  val fetchAndAdd : int ref * int -> int

end
