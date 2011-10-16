functor MLSchedulerQueues () : SCHEDULER_QUEUES =
struct

  open Critical
  structure Q = ImpQueue
  structure A = Array
  structure R = RepTypes
  structure L = Lock

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  datatype runnable_host = datatype RepTypes.runnable_host
  type queue_prio = RepTypes.queue_prio
  type thread_id = RepTypes.thread_id

  fun debug msg = Debug.sayDebug ([], (msg))
  fun debug' msg = debug (fn () => msg()^" : "^Int.toString(PacmlFFI.processorNumber()))

  (* These processors are used only to run IO CML thread *)
  val numIOProcs = PacmlFFI.numIOProcessors

  val numberOfProcessors = PacmlFFI.numberOfProcessors

  (* Only these processors are used to run general CML threads *)
  val numComputeProcessors = PacmlFFI.numComputeProcessors

  (* Create separate queues for each processor. Each processor has a
   * primary and a secondary queue *)
  val threadQs = A.tabulate (numberOfProcessors, fn _ => (Q.new (), Q.new ()))
  val locks = A.tabulate (numberOfProcessors, fn _ => L.initCmlLock ())

  fun acquireQlock p = L.getCmlLock (A.unsafeSub (locks, p)) (PacmlFFI.processorNumber)
  fun releaseQlock p = L.releaseCmlLock (A.unsafeSub (locks, p)) (PacmlFFI.processorNumber)

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

  fun dequeFromProc (prio, fromProc, lockProc) =
  let
    val _ = atomicBegin ()
    val _ = acquireQlock lockProc
    val (pri, sec) = A.unsafeSub (threadQs, fromProc)
    val rthrd = case prio of
                     R.PRI => Q.deque (pri)
                   | R.SEC => Q.deque (sec)
                   | R.ANY => case Q.deque (pri) of
                                   SOME t => SOME t
                                 | NONE => Q.deque (sec)
    val _ = releaseQlock lockProc
    val _ = atomicEnd ()
  in
    rthrd
  end

  fun deque (prio) =
  let
    val fromProc = PacmlFFI.processorNumber ()
  in
    dequeFromProc (prio, fromProc, fromProc)
  end

  fun emptyProc (proc) =
  let
    val (pri, sec) = A.unsafeSub (threadQs, proc)
  in
    (Q.empty pri) andalso (Q.empty sec)
  end

  fun empty () =
  let
    val _ = PacmlFFI.maybeWaitForGC ()
    val proc = PacmlFFI.processorNumber ()
  in
    emptyProc (proc)
  end

  fun dequeAny () =
  let
    val _ = PacmlFFI.maybeWaitForGC ()
    val procNum = PacmlFFI.processorNumber ()
    val numComp = PacmlFFI.numComputeProcessors
  in
    (* the io processors do not steal *)
    if (procNum >= numComp) then
      (if emptyProc (procNum) then
        NONE
      else
        dequeFromProc (R.ANY, procNum, procNum))
    else (* Try to steal from someone else's queue, starting from yours *)
      (let
        fun loop (n) =
          if n = numComp then NONE
          else if emptyProc ((n + procNum) mod numComp) then
            loop (n+1)
          else (case dequeFromProc (R.ANY, (n + procNum) mod numComp, procNum) of
                    NONE => loop (n+1)
                  | v => v)
      in
        loop (0)
      end)
  end


  fun clean () =
    Array.app (fn (x,y) => (Q.reset x;Q.reset y)) threadQs

end
