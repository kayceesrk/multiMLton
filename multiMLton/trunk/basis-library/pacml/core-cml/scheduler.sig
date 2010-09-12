signature SCHEDULER =
sig
  type 'a thread = 'a RepTypes.thread
  type runnable_host = RepTypes.runnable_host
  type parasite = RepTypes.parasite
  type rdy_thread = RepTypes.rdy_thread

  val preempt : runnable_host -> unit
  val reset : bool -> unit
  val next : unit -> runnable_host
  val nextWithCounter : int -> runnable_host
  val unwrap : (runnable_host -> runnable_host) ->
               (parasite -> runnable_host) ->
                MLtonThread.Runnable.t ->
                MLtonThread.Runnable.t

  (* Thread control *)
  val atomicSwitch : ('a thread -> runnable_host) -> 'a
  val ready : rdy_thread -> unit
  val atomicReady : rdy_thread -> unit
  val atomicSwitchToNext : ('a thread -> unit) -> 'a
  val switchToNext : ('a thread -> unit) -> 'a
end
