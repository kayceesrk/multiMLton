signature PROTO_THREAD =
sig
  include CRITICAL

  type thread_id = ThreadID.thread_id
  type thread_type = ThreadID.thread_type
  type 'a thread = 'a RepTypes.thread
  type rdy_thread = RepTypes.rdy_thread
  type runnable_host = RepTypes.runnable_host
  type parasite_state = RepTypes.parasite_state

  type lock_id = int

  (* Thread management *)
  val new : (unit -> unit) -> unit MLtonThread.t
  (* Continuation management *)
  val prepend : 'a thread * ('b -> 'a) -> 'b thread
  val prep : unit thread -> rdy_thread
  val prepVal : 'a thread * 'a -> rdy_thread
  val prepFn : 'a thread * (unit -> 'a) -> rdy_thread

  (* Manipulate current thread info *)
  val getThreadState : unit -> parasite_state
  val setThreadState : parasite_state -> unit
  val getThreadType : unit -> thread_type
  val setThreadType : thread_type -> unit
  val getParasiteBottom : unit -> int
  val setParasiteBottom : int -> unit
  val getNumPenaltySpawns : unit -> int
  val setNumPenaltySpawns : int -> unit
  val getLockId : unit -> int
  val setLockId : lock_id -> unit

  val enableParasitePreemption : unit -> unit
  val disableParasitePreemption : unit -> unit
  val getThreadTypeString : unit -> string

  (* Parasite manipulation *)
  val jumpDown : int -> unit
  val copyParasite : int -> RepTypes.parasite
  val toPreemptParasite : unit -> bool
  val proceedToExtractParasite : RepTypes.primHost * int -> bool
  val extractParasiteFromHost : RepTypes.primHost * int -> RepTypes.parasite
  val atomicPrefixAndSwitchToSpecial : lock_id * RepTypes.parasite -> unit
  val atomicPrefixAndSwitchTo : lock_id * RepTypes.parasite -> unit
  val spawnParasite : (unit -> unit) -> unit

  val getRunnableHost : rdy_thread -> runnable_host
end
