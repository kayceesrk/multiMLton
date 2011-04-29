signature SCHEDULER_QUEUES =
sig
  type thread_id = ThreadID.thread_id
  type queue_prio = RepTypes.queue_prio
  type runnable_host = RepTypes.runnable_host

  val enque : runnable_host * queue_prio -> unit
  val deque : queue_prio -> runnable_host option

  (* Polls every processor starting from current processor for availability *)
  val dequeAny : unit -> runnable_host option
  val empty : unit -> bool
  val clean : unit -> unit
end
