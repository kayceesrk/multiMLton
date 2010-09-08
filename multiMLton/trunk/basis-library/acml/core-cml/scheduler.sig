(* scheduler.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* scheduler.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature SCHEDULER =
   sig
      include CRITICAL

      type thread_id = ThreadID.thread_id
      type thread_type = ThreadID.thread_type
      type 'a thread = 'a RepTypes.thread
      type rdy_thread = RepTypes.rdy_thread
      type threadlet = Primitive.MLton.Thread.thread

      type 'a recv_threadlet
      type 'a send_threadlet
      type rdy_threadlet

      val prep : unit thread -> rdy_thread
      val prepVal : 'a thread * 'a -> rdy_thread
      val prepFn : 'a thread * (unit -> 'a) -> rdy_thread

      val getThreadId : 'a thread -> thread_id
      val getCurThreadId : unit -> thread_id
      val tidMsg : unit -> string
      val tidNum : unit -> int

      val getParasiteBottom : unit -> int
      val setParasiteBottom : int -> unit
      val setThreadletType : thread_type -> unit

      val parasite : (unit -> unit) -> unit
      val atomicPrefixAndSwitchTo : Primitive.MLton.Thread.thread -> unit
      val finishWork : unit -> bool

      val ready : rdy_thread -> unit
      (* Used by spawn. If spawning on same processor then bool is false*)
      val readySpawn : rdy_thread -> RepTypes.placement -> unit
      (* Used be send/recv. Takes processor number *)
      val readyOnProc : (rdy_thread * int) -> unit
      val next : unit -> rdy_thread
      val next' : int -> rdy_thread

      val switch : ('a thread -> rdy_thread) -> 'a
      val atomicSwitch : ('a thread -> rdy_thread) -> 'a
      val switchToNext : ('a thread -> unit) -> 'a
      val atomicSwitchToNext : ('a thread -> unit) -> 'a
      val readyAndSwitch : (unit -> rdy_thread) -> unit
      val atomicReadyAndSwitch : (unit -> rdy_thread) -> unit
      val readyAndSwitchToNext : (unit -> unit) -> unit
      val atomicReadyAndSwitchToNext : (unit -> unit) -> unit

      val new  : (thread_id -> ('a -> unit)) -> 'a thread
      val new' : (thread_id -> (unit -> unit)) -> unit thread

      val prepend : 'a thread * ('b -> 'a) -> 'b thread
      val unwrap : (rdy_thread -> rdy_thread) ->
                   (MLtonThread.Runnable.t -> MLtonThread.Runnable.t)

      val reset : bool -> unit
      val preempt : rdy_thread -> unit

      val wrapFunction : ((unit -> unit) -> thread_id -> (unit -> unit)) option ref

      (**
      * Inflate and async to a CML thread
      *
      * @param async
      *
      * @return runnable CML thread
      *)
      val reifyHostFromParasite : threadlet -> rdy_thread

     val getThreadType : unit -> RepTypes.thread_type

     val disableParasitePreemption : unit -> unit
     val enableParasitePreemption : unit -> unit


   end
