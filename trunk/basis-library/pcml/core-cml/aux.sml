(* running.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* running.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * A flag to tell us if CML is running.  This gets set and cleared in the
 * RunCMLFn functor, but other modules need to test it.
 *)

signature AUX =
sig
  val tidMsg : unit -> string
  val processorNumber : unit -> int
  val numberOfProcessors : int
  val numThreadsLive : unit -> int
end

structure Aux : AUX =
  struct
    val tidMsg = Scheduler.tidMsg
    val processorNumber = Basic.processorNumber
    val numberOfProcessors = Basic.numberOfProcessors
    val numThreadsLive = fn () => !Basic.numThreadsLive
  end
