signature LOCK =
sig
  type cmlLock = RepTypes.cmlLock

  val initCmlLock : unit -> cmlLock
  val getCmlLock : cmlLock -> (unit->int) -> unit
  val releaseCmlLock : cmlLock -> (unit->int) -> unit

end
