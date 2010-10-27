signature THREAD =
sig
  
  val getTid : unit -> ThreadID.thread_id
  val exit : unit -> unit
  val spawnHost : (unit -> unit) -> ThreadID.thread_id
  val spawnParasite : (unit -> unit) -> unit

  val spawn : (unit -> unit) -> ThreadID.thread_id
  val spawnOnProc : ((unit -> unit) * int) -> ThreadID.thread_id

end

signature THREAD_EXTRA =
sig
  include THREAD
  val createHost : (unit->unit) -> RepTypes.runnable_host
  val reifyHostFromParasite : RepTypes.parasite -> RepTypes.runnable_host
end
