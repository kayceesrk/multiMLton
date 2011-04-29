signature THREAD =
sig
  include THREAD_ID
  val getTid : unit -> thread_id
  val exit : unit -> unit
  val yield : unit -> unit
  val spawnHost : (unit -> unit) -> thread_id
  val spawnParasite : (unit -> unit) -> unit
  val spawn : (unit -> unit) -> thread_id
  val spawnOnProc : ((unit -> unit) * int) -> thread_id

end

signature THREAD_EXTRA =
sig
  include THREAD
  val timeoutCleanup : (unit -> unit) ref
  val createHost : (unit->unit) -> RepTypes.runnable_host
  val reifyHostFromParasite : (int * RepTypes.parasite) -> RepTypes.runnable_host
  val reifyCurrent : unit -> unit
  val reifyCurrentIfParasite : unit -> unit
end
