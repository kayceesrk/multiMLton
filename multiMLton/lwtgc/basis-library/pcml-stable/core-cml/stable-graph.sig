signature STABLE_GRAPH =
  sig
    val spawnThread   : (ThreadID.thread_id * ThreadID.thread_id) -> unit
    val spawnThread2  : (ThreadID.thread_id * ThreadID.thread_id) -> unit

    val schedThread   : ((unit -> unit) ref* ThreadID.thread_id) -> unit
    val schedThread2  : ((unit -> unit) ref* ThreadID.thread_id) -> unit

    val completeCom   : (ThreadID.thread_id * ThreadID.thread_id) -> unit
    val partialCom    : (ThreadID.thread_id * TransID.trans_id) -> unit
    val enterSS       : (ThreadID.thread_id * (unit -> unit) * unit GCAbleGraph.Node.t option ref) -> unit
    val exitSS        : ThreadID.thread_id -> unit
    val debug         : string -> unit
    val addReference  : (ThreadID.thread_id * (unit -> unit)) -> unit
    val stabilizeGraph: ThreadID.thread_id * unit GCAbleGraph.Node.t option ->
 ((ThreadID.thread_id * unit MLtonThread.t) list * ThreadID.thread_id list)

    (*used by sync-var.sml*)
    val addSyncVarWrite : (ThreadID.thread_id * (unit -> unit)) -> unit
    val addSyncVarCom   : (ThreadID.thread_id * ThreadID.thread_id) -> unit

    val inStableSection: ThreadID.thread_id -> bool
  end
