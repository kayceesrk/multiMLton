functor CSchedulerQueues () : SCHEDULER_QUEUES =
struct

  open Critical
  structure Q = ImpQueue
  structure A = Array
  structure R = RepTypes
  structure L = Lock
  structure PrimSQ = Primitive.MLton.SchedulerQueue

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  datatype runnable_host = datatype RepTypes.runnable_host
  type queue_prio = RepTypes.queue_prio
  type thread_id = RepTypes.thread_id

  fun debug msg = Debug.sayDebug ([], (msg))
  fun debug' msg = debug (fn () => msg^"."^(ProtoThread.getThreadTypeString())
                                   ^" : "^Int.toString(PacmlFFI.processorNumber()))

  val pri = 0
  val sec = 1

  val _ = PrimSQ.createQueues ()

  fun enque (rthrd as RHOST (tid, t), prio) =
  let
    val _ = atomicBegin ()
    val _ = debug' ("sqEnque: "^(MLtonThread.threadStatus t))
    val targetProc = ThreadID.getProcId (tid)
    val _ = PrimSQ.acquireLock targetProc
    val q = case prio of
                 R.PRI => pri
               | _ => sec
    val _ = PrimSQ.enque (rthrd, targetProc, q)
    val _ = PrimSQ.releaseLock targetProc
    val _ = PacmlFFI.wakeUp (targetProc, 1)
    val _ = atomicEnd ()
  in
    ()
  end

  fun deque (prio) =
  let
    val proc = PacmlFFI.processorNumber ()
    val _ = atomicBegin ()
    val _ = PrimSQ.acquireLock proc
    val rthrd = case prio of
                     R.PRI => PrimSQ.deque (pri)
                   | R.SEC => PrimSQ.deque (sec)
                   | R.ANY => case PrimSQ.deque (pri) of
                                   SOME t => SOME t
                                 | NONE => PrimSQ.deque (sec)
    val _ = case rthrd of
                 NONE => ()
               | SOME (RHOST (_, t)) =>
                   debug' ("sqDeque: "^(MLtonThread.threadStatus t))
    val _ = PrimSQ.releaseLock proc
    val _ = atomicEnd ()
  in
    rthrd
  end

  fun empty () =
  let
    val _ = PacmlFFI.maybeWaitForGC ()
  in
    PrimSQ.isEmpty ()
  end

  fun dequeAny () = raise Fail "ml-scheduler-queue: dequeAny not implemented"

  fun clean () =  PrimSQ.clean ()

end
