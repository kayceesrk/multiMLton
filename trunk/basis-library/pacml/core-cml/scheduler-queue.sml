structure SchedulerQueues : SCHEDULER_QUEUES =
struct

  open Critical
  structure Q = ImpQueue
  structure A = Array
  structure R = RepTypes
  structure L = Lock

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = false)

  datatype runnable_host = datatype RepTypes.runnable_host
  type queue_prio = RepTypes.queue_prio
  type thread_id = RepTypes.thread_id

  fun debug msg = Debug.sayDebug ([], (msg))
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))

  (* These processors are used only to run IO CML thread *)
  val numIOProcs = PacmlFFI.numIOProcessors

  val numberOfProcessors = PacmlFFI.numberOfProcessors

  (* Only these processors are used to run general CML threads *)
  val numComputeProcessors = numberOfProcessors - numIOProcs

  (* Create separate queues for each processor. Each processor has a
   * primary and a secondary queue *)
  val threadQs = A.tabulate (numberOfProcessors, fn _ => (Q.new (), Q.new ()))
  val locks = A.tabulate (numberOfProcessors, fn _ => L.initCmlLock ())

  fun acquireQlock p = L.getCmlLock (A.unsafeSub (locks, p)) (PacmlFFI.processorNumber)
  fun releaseQlock p = L.releaseCmlLock (A.unsafeSub (locks, p)) (PacmlFFI.processorNumber ())

  fun enque (rthrd as RHOST (tid, t), prio) =
  let
    val _ = atomicBegin ()
    val targetProc = ThreadID.getProcId (tid)
    val _ = acquireQlock targetProc
    val (pri, sec) = A.unsafeSub (threadQs, targetProc)
    val q = case prio of
                 R.PRI => pri
               | _ => sec
    val _ = Q.enque (q, rthrd)
    val _ = releaseQlock targetProc
    val _ = PacmlFFI.wakeUp (targetProc, 1)
    val _ = atomicEnd ()
  in
    ()
  end

  fun deque (prio) =
  let
    val _ = atomicBegin ()
    val fromProc = PacmlFFI.processorNumber ()
    val _ = acquireQlock fromProc
    val (pri, sec) = A.unsafeSub (threadQs, fromProc)
    val rthrd = case prio of
                     R.PRI => Q.deque (pri)
                   | R.SEC => Q.deque (sec)
                   | R.ANY => case Q.deque (pri) of
                                   SOME t => SOME t
                                 | NONE => Q.deque (sec)
    val _ = releaseQlock fromProc
    val _ = atomicEnd ()
  in
    rthrd
  end

  fun empty () =
  let
    val _ = PacmlFFI.maybeWaitForGC ()
    val proc = PacmlFFI.processorNumber ()
    val (pri, sec) = A.unsafeSub (threadQs, proc)
  in
    (Q.empty pri) andalso (Q.empty sec)
  end

  fun clean () = Array.app (fn (x,y) => (Q.reset x;Q.reset y)) threadQs

end
