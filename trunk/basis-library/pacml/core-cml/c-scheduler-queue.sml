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
  datatype rdy_thread = datatype RepTypes.rdy_thread

  type queue_prio = RepTypes.queue_prio
  type thread_id = RepTypes.thread_id
  type parasite = RepTypes.parasite

  fun debug msg = Debug.sayDebug ([], (msg))
  fun debug' msg = debug (fn () => msg()^"."^(ProtoThread.getThreadTypeString())
                                   ^" : "^Int.toString(PacmlFFI.processorNumber()))

  val pri = 0
  val sec = 1

  val _ = PrimSQ.createQueues ()

  fun enqueHost (rthrd as RHOST (tid, t), prio) =
  let
    val _ = Assert.assertAtomic (fn () => "enqueHost", NONE)
    val _ = (MLtonThread.threadStatus t) (* XXX dummy *)
    val targetProc = ThreadID.getProcId (tid)
    val q = case prio of
                 R.PRI => pri
               | _ => sec
    val _ = PrimSQ.enque (H_RTHRD (rthrd), targetProc, q)
    val _ = PacmlFFI.wakeUp (targetProc, 1)
  in
    ()
  end

  fun enqueParasite (lockId, par) =
  let
    val _ = Assert.assertAtomic (fn () => "enqueParasite", NONE)
    val _ = PrimSQ.enque (P_RTHRD (lockId, par),
                          PacmlFFI.processorNumber (),
                          2)
  in
    ()
  end


  fun dequeHost (prio) =
  let
    val _ = Assert.assertAtomic (fn () => "dequeHost", NONE)
    val proc = PacmlFFI.processorNumber ()
    val rthrd = case prio of
                     R.PRI => PrimSQ.deque (pri)
                   | R.SEC => PrimSQ.deque (sec)
                   | R.ANY => case PrimSQ.deque (pri) of
                                   SOME t => SOME t
                                 | NONE => PrimSQ.deque (sec)
    val rhost = case rthrd of
                 NONE => NONE
               | SOME (H_RTHRD (RHOST (tid, t))) =>
                   (ignore (MLtonThread.threadStatus t) (* XXX dummy *)
                   ; SOME (RHOST (tid, t)))
               | _ => (print "dequeHost: Impossible\n";
                       raise Fail "dequeHost: Impossible")
  in
    rhost
  end

  fun dequeParasite () =
  let
    val _ = Assert.assertAtomic (fn () => "dequeHost", NONE)
    val (lockId, par) = case PrimSQ.deque (2) of
                             NONE => (print "dequeParasite is NONE";
                                      raise Fail "dequeParasite is NONE")
                           | SOME (P_RTHRD (lockId, par)) => (lockId, par)
                           | SOME (H_RTHRD (_)) => (print "dequeParasite: Impossible";
                                                   raise Fail "dequeParasite")
  in
    (lockId, par)
  end


  val emptyHostQ = PrimSQ.isEmpty
  fun emptyParasiteQ () = PrimSQ.isEmptyPrio (2)
  val clean =  PrimSQ.clean

  fun dequeAny () = raise Fail "ml-scheduler-queue: dequeAny not implemented"


end
