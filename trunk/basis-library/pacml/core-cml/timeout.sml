structure Timeout : TIME_OUT_EXTRA =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Critical

  structure S = Scheduler
  structure E = Event
  structure L = Lock
  structure PT = ProtoThread
  structure TID = ThreadID

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => (msg())^" : "^Int.toString(PacmlFFI.processorNumber()))

  (* this holds an approximation of the current time of day.  It is
    * cleared at each pre-emption, and initialized on demand (by getTime).
    *)
  val clockArr = Array.tabulate (PacmlFFI.numberOfProcessors, fn _ => NONE)

  (* returns an approximation of the current time of day
    * (this is at least as accurate as the time quantum).
    *)
  fun getTime () =
  let
    val clock = Array.unsafeSub (clockArr, PacmlFFI.processorNumber ())
  in
      case clock of
        NONE => let
                  val t = Time.now()
                in
                  (Array.update (clockArr, PacmlFFI.processorNumber (), SOME t);
                  t)
                end
      | SOME t => t
  end

  (* The queue of threads waiting for timeouts.
    * It is sorted in increasing order of time value.
    *)
  structure TQ = FunPriorityQueue(structure Key = struct open Time type t = time end)
  type item = int ref * S.rdy_thread
  val timeQArray : item TQ.t array = Array.tabulate (PacmlFFI.numberOfProcessors, fn _ => TQ.new ())

  val cas = PacmlFFI.vCompareAndSwap

  fun cleaner (readied: unit -> unit) elt =
    let
      val now = getTime ()
      val (txid, t) = TQ.Elt.value elt
      fun inflateAndReady (t) =
      let
        val rt = case t of
                      RepTypes.P_RTHRD (lockId, parasite) =>
                        RepTypes.H_RTHRD (Thread.reifyHostFromParasite (lockId, parasite))
                    | _ => t
        val rhost = case rt of
                         RepTypes.H_RTHRD (rhost) => rhost
                       | _ => raise Fail "TimeOut.cleaner: Impossible. Must be a host thread"
      in
        S.readyForSpawn (rhost)
      end

      fun matchLp () =
        if Time.>(TQ.Elt.key elt, now) then
          false
        else
          let
            val res = cas (txid, 0, 2) (* Try to sync it *)
          in
            if (res = 0) then (* We got it *)
              (readied ();
               inflateAndReady (t);
               true)
            else if (res = 1) then (* Someone has claimed it.. try again *)
              matchLp ()
            else (* already synched *)
              true
          end
    in
      matchLp ()
    end

  fun timeWait (time, txid, t) =
  let
    val _ = Assert.assertAtomic' ("TimeOut.timeWait", NONE)
    val timeQ = Array.unsafeSub (timeQArray, PacmlFFI.processorNumber ())
    val timeQ = TQ.enque (timeQ, time, (txid, t))
  in
    Array.update (timeQArray, PacmlFFI.processorNumber (), timeQ)
  end

  fun timeOutEvt time =
  let
    fun blockFn (txid) =
      let
        val () = debug' (fn () => "timeOutEvt(3.2.1)") (* Atomic 1 *)
        val () = Assert.assertAtomic' ("TimeOut.timeOutEvt(3.2.1)", SOME 1)
        val () =
          S.atomicSwitchToNext
            (fn t => timeWait (Time.+(time, getTime ()), txid, PT.prep t))
        val () = debug' (fn () => "timeOutEvt(3.2.3)") (* NonAtomic *)
        val () = Assert.assertNonAtomic' "TimeOut.timeOutEvt(3.2.3)"
      in
        ()
      end
    fun pollFn () =
      let
        val () = Assert.assertAtomic' ("TimeOut.timeOutEvt.pollFn", NONE)
        val () = debug' (fn () => "timeOutEvt(2)") (* Atomic 1 *)
        val () = Assert.assertAtomic' ("TimeOut.timeOutEvt(2)", SOME 1)
      in
       (* (print (concat ["Timeout: time = ", LargeInt.toString (Time.toSeconds (time)),
                        "zeroTime = ", LargeInt.toString (Time.toSeconds (Time.zeroTime)),
                        "\n"]); *)
        if Time.<=(time, Time.zeroTime) then
          E.enabled {prio = ~1, doitFn = fn () => (debug' (fn () => "timeOutEvt(3.1)"); atomicEnd ())}
        else E.blocked blockFn
      end
  in
    E.bevt pollFn
  end

  fun atTimeEvt time =
    let
      fun doitFn () =
        let
          val () = debug' (fn () => "atTimeEvt(3.1.1)") (* Atomic 1 *)
          val () = Assert.assertAtomic' ("TimeOut.atTimeEvt(3.2.1)", SOME 1)
          val () = atomicEnd ()
        in
          ()
        end
      fun blockFn (txid) =
        let
          val () = debug' (fn () => "atTimeEvt(3.2.1)") (* Atomic 1 *)
          val () = Assert.assertAtomic' ("TimeOut.atTimeEvt(3.2.1)", SOME 1)
          val () =
              S.atomicSwitchToNext (fn t => (timeWait (time, txid, PT.prep t)))
          val () = debug' (fn () => "atTimeEvt(3.2.3)") (* NonAtomic *)
          val () = Assert.assertNonAtomic' "TimeOut.atTimeEvt(3.2.3)"
        in
          ()
        end
      fun pollFn () =
          let
            val () = debug' (fn () => "atTimeEvt(2)") (* Atomic 1 *)
            val () = Assert.assertAtomic' ("TimeOut.atTimeEvt(2)", SOME 1)
          in
            if Time.<=(time, getTime())
                then E.enabled {prio = ~1, doitFn = doitFn}
                else E.blocked blockFn
          end
    in
      E.bevt pollFn
    end

  (* reset various pieces of state *)
  fun reset () = Array.modify (fn _ => TQ.new ()) timeQArray
  fun preemptTime () = Array.update (clockArr, PacmlFFI.processorNumber (), NONE)

  (* what to do at a preemption *)
  fun preempt () : Time.time option option =
    let
      val () = Assert.assertAtomic' ("TimeOut.preempt", SOME 1)
      val timeQ = Array.unsafeSub (timeQArray, PacmlFFI.processorNumber ())
      val res =
          let
            val res =
              (if TQ.empty timeQ then NONE
              else
                let
                  val readied = ref false
                  val timeQ = TQ.cleanPrefix (timeQ, cleaner (fn () => readied := true))
                  val () = Array.update (timeQArray, PacmlFFI.processorNumber (), timeQ)
                  val res =
                    if !readied
                    then SOME NONE
                    else case TQ.peek timeQ of
                              NONE => NONE
                            | SOME elt => SOME(SOME(Time.zeroTime))
                in
                  res
                end)
          in
            res
          end
      val () = Assert.assertAtomic' ("TimeOut.preempt", SOME 1)
    in
      res
    end

  (* Assign timeoutCleanup function. This will be used by Thread.yield *)
  val _ = Thread.timeoutCleanup := (fn () => (preemptTime ();
                                              ignore (preempt ())))





end
