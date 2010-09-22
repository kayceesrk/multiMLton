signature BASIC =
sig
  (* the empty type *)
  type void
  type thread_id = ThreadID.thread_id
  type rdy_thread = RepTypes.rdy_thread
  type queue_prio = RepTypes.queue_prio

  type 'a t

  val processorNumber : unit -> int

  (* end the current task and return control to the scheduler *)
  val return : unit -> void

  val enablePreemption : unit -> unit
  val disablePreemption : unit -> unit

  (* Work Management *)
  (* adds work to the current processor queue *)
  (* bool is true is spawning *)
  val addWork : rdy_thread -> bool -> queue_prio -> unit
  (* Takes the target processor number as input *)
  val addWorkTo : rdy_thread -> int -> queue_prio -> unit
  val getWork : queue_prio -> rdy_thread option
  (* Returns true if all threads are finished *)
  val finishWork : unit -> bool
  (* Clears the processor queues *)
  val clearWork : unit -> unit
  (* Checks of the queue for the current processor is empty *)
  val empty : unit -> bool

  (* general errors related to parallelism *)
  exception Parallel of string

  (* informational *)
  val policyName : string
  val numberOfProcessors : int
  val maxBytesLive : unit -> Word64.word
  val resetBytesLive : unit -> unit

  (* ThreadId management *)
  val setCurThreadId : thread_id -> unit
  val getCurThreadId : unit -> thread_id
  (* Sets all elements of threadID array to dummyId *)
  val clearThreadIds : unit -> unit

  val numThreadsLive : int ref
end
