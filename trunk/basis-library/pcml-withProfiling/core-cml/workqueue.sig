signature WORKQUEUE =
sig

  (* processor identifier *)
  type proc = int
  (* abstract type of work *)
  type work

  (* these take the identifier of the current processor as their first
   argument *)
  (* add new work to the queue *)
  (* bool is true for adding to same processor *)
  val addWork : proc -> work -> bool -> unit
  val addWorkTo : proc -> work -> unit
  (* remove the next, highest priority work *)
  val getWork : proc -> (work option)
  val clearWork : unit -> unit
  (* mark the most recent unit of work as done *)
  val finishWork : unit -> bool
  (* check if empty without locking *)
  val empty : int -> bool

  (* name of the current policy *)
  val policyName : string

  val numThreadsLive : int ref
end
