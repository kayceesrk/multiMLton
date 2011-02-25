signature SCHEDULER_QUEUES =
sig
  type thread_id = ThreadID.thread_id
  type queue_prio = RepTypes.queue_prio
  type rdy_thread = RepTypes.rdy_thread
  type runnable_host = RepTypes.runnable_host
  type parasite = RepTypes.parasite

  val enqueHost : runnable_host * queue_prio -> unit
  val enqueParasite : (int * parasite) -> unit
  val dequeHost : queue_prio -> runnable_host option
  val dequeParasite : unit -> (int * parasite)

  (* Polls every processor starting from current processor for availability *)
  val dequeAny : unit -> rdy_thread option
  val emptyHostQ : unit -> bool
  val emptyParasiteQ : unit -> bool
  val clean : unit -> unit
end
