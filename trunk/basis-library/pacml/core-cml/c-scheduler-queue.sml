functor CSchedulerQueues () : SCHEDULER_QUEUES =
struct

  open Critical
  structure Q = ImpQueue
  structure A = Array
  structure R = RepTypes
  structure PrimSQ = PacmlPrim.SchedulerQueue

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

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
    val _ = (MLtonThread.threadStatus t) (* DO NOT REMOVE *)
    val targetProc = ThreadID.getProcId (tid)
    (* val _ = PrimSQ.acquireLock targetProc *)
    val q = case prio of
                 R.PRI => pri
               | _ => sec
    val _ = PrimSQ.enque (rthrd, targetProc, q)
    (* val _ = PrimSQ.releaseLock targetProc *)
    val _ = PacmlFFI.wakeUp (targetProc, 1)
    val _ = atomicEnd ()
  in
    ()
  end

  fun deque (prio) =
  let
    val proc = PacmlFFI.processorNumber ()
    val _ = atomicBegin ()
    (* val _ = PrimSQ.acquireLock proc *)
    val rthrd = case prio of
                     R.PRI => PrimSQ.deque (pri)
                   | R.SEC => PrimSQ.deque (sec)
                   | R.ANY => case PrimSQ.deque (pri) of
                                   SOME t => SOME t
                                 | NONE => PrimSQ.deque (sec)
    val _ = case rthrd of
                 NONE => "NONE"
               | SOME (RHOST (_, t)) =>
                   MLtonThread.threadStatus t (* DO NOT REMOVE *)
    (* val _ = PrimSQ.releaseLock proc *)
    val _ = atomicEnd ()
  in
    rthrd
  end

  val empty = PrimSQ.isEmpty
  val clean =  PrimSQ.clean

  fun dequeAny () = raise Fail "ml-scheduler-queue: dequeAny not implemented"


end
