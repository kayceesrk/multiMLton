signature SCHEDULER =
sig
  type 'a thread = 'a RepTypes.thread
  type runnable_host = RepTypes.runnable_host
  type parasite = RepTypes.parasite
  type rdy_thread = RepTypes.rdy_thread

  val preempt : runnable_host -> unit
  val reset : bool -> unit
  val next : unit -> runnable_host
  val nextWithCounter : int * Time.time option option -> runnable_host
  val unwrap : (runnable_host -> runnable_host) ->
               (int * parasite -> runnable_host) ->
                MLtonThread.Runnable.t ->
                MLtonThread.Runnable.t

  (* Thread control *)
  val atomicSwitch : ('a thread -> runnable_host) -> 'a
  val switch : ('a thread -> runnable_host) -> 'a
  val ready : rdy_thread -> unit
  val atomicReady : rdy_thread -> unit
  val atomicReadyHost : runnable_host -> unit
  val readyForSpawn : runnable_host -> unit (* Increments live threads by 1 *)
  val atomicSwitchToNext : ('a thread -> unit) -> 'a
  val switchToNext : ('a thread -> unit) -> 'a

  val atomicSwitchToNextHostForWB : (rdy_thread -> unit) -> unit
  val atomicSwitchToNextParasiteForWB : (rdy_thread -> unit) -> unit

  (* scheduler control *)
  val deque : unit -> runnable_host option
  val preemptOnWriteBarrier : unit -> unit

end
