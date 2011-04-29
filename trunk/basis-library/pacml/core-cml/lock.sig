signature LOCK =
sig
  type cmlLock = RepTypes.cmlLock

  val initCmlLock : unit -> cmlLock
  val getCmlLock : cmlLock -> (unit->int) -> unit
  val releaseCmlLock : cmlLock -> (unit->int) -> unit

end

signature LOCK_EXTRA =
sig
  include LOCK
  val yieldForSpin : (unit -> unit) ref
end
