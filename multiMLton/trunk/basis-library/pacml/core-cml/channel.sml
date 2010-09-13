structure Channel : CHANNEL_EXTRA =
struct
  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  open Critical

  structure Q = ImpQueue
  structure L = Lock
  structure TID = ThreadID
  structure S = Scheduler
  structure PT = ProtoThread

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))

  datatype thread = datatype RepTypes.thread

  datatype 'a chan = CHAN of {prio: int ref,
                              inQ : (int ref * ('a thread)) Q.t,
                              outQ : (int ref * ('a * unit thread)) Q.t,
                              lock : L.cmlLock}

  fun channel () = CHAN {prio = ref 1,
                         inQ = Q.new (),
                         outQ = Q.new (),
                         lock = L.initCmlLock ()}

  val cas = PacmlFFI.vCompareAndSwap
  fun mkTxId () = ref 0

  (* bump a priority value by one, returning the old value *)
  fun bumpPriority (p as ref n) =
    PacmlFFI.fetchAndAdd (p,1)

  (* functions to clean channel input and output queues *)
  local
      open Q

      (* Remove SYNCHED(2) *)
      fun cleaner (txid, _) =
        if (!txid = 2) then true else false
  in
      fun cleanAndChk (prio, q) : int =
        (cleanPrefix (q, cleaner)
          ; if empty q
              then 0
              else bumpPriority prio)

      fun cleanAndDeque q =
        dequeLazyClean (q, cleaner)

      fun cleanAndEnque (q, item) =
        (cleanSuffix (q, cleaner);
         enque (q, item))

  end

  fun send (CHAN {prio, inQ, outQ, lock}, msg) =
    let
      val () = Assert.assertNonAtomic' "channel.send"
      val () = Assert.assertNonAtomic' "channel.send(1)"
      val () = debug' "channel.send(1)"
      val () = atomicBegin ()
      val () = L.getCmlLock lock TID.tidNum
      val () = Assert.assertAtomic' ("channel.send(2)", SOME 1)
      val () = debug' "channel.send(2)"
      fun tryLp () =
        case cleanAndDeque (inQ) of
              SOME (rtxid, rt) =>
                (let
                  val () = debug' "Channel.tryLp"
                  fun matchLp () =
                    (let
                      val () = debug' "Channel.matchLp"
                      val res = cas (rtxid, 0, 2)
                     in
                      if res = 0 then
                        (let (* WAITING -- we got it *)
                          val _ = prio := 1
                          val () = TID.mark (TID.getCurThreadId ())
                          val () = L.releaseCmlLock lock (TID.tidNum())
                          val rthrd = PT.prepVal (rt, msg)
                        in
                          S.atomicReady (rthrd) (* Implicit atomic end *)
                        end)
                      else if res = 1 then matchLp () (* CLAIMED *)
                      else tryLp () (* SYNCHED *)
                    end) (* matchLp ends *)
                in
                  matchLp ()
                end) (* SOME ends *)
            | NONE =>
                S.atomicSwitchToNext (fn st => (cleanAndEnque (outQ, (mkTxId (), (msg, st)))
                                               ; L.releaseCmlLock lock (TID.tidNum())))
                (* tryLp ends *)
      val () = tryLp ()
    in
      ()
    end (* send ends *)

  fun sendPoll (CHAN {prio, inQ, outQ, lock}, msg) =
    let
      val () = Assert.assertNonAtomic' "channel.sendPoll"
      val () = Assert.assertNonAtomic' "channel.sendPoll(1)"
      val () = debug' "channel.sendPoll(1)"
      val () = atomicBegin ()
      val () = L.getCmlLock lock TID.tidNum
      val () = Assert.assertAtomic' ("channel.sendPoll(2)", SOME 1)
      val () = debug' "channel.sendPoll(2)"
      fun tryLp () =
        case cleanAndDeque (inQ) of
              SOME (rtxid, rt) =>
                (let
                  fun matchLp () =
                    (let
                      val res = cas (rtxid, 0, 2)
                     in
                      if res = 0 then
                        (let (* WAITING -- we got it *)
                          val _ = prio := 1
                          val () = TID.mark (TID.getCurThreadId ())
                          val () = L.releaseCmlLock lock (TID.tidNum())
                          val rthrd = PT.prepVal (rt, msg)
                          val _ = S.atomicReady (rthrd) (* Implicit atomic end *)
                        in
                          true
                        end)
                      else if res = 1 then matchLp () (* CLAIMED *)
                      else tryLp () (* SYNCHED *)
                    end) (* matchLp ends *)
                in
                  matchLp ()
                end) (* SOME ends *)
            | NONE => false
            (* tryLp ends *)
    in
      tryLp ()
    end (* sendPoll ends *)



  fun recv (CHAN {prio, inQ, outQ, lock}) =
    let
      val () = Assert.assertNonAtomic' "channel.recv"
      val () = Assert.assertNonAtomic' "channel.recv(1)"
      val () = debug' "channel.recv(1)"
      val () = atomicBegin ()
      val () = L.getCmlLock lock TID.tidNum
      val () = Assert.assertAtomic' ("channel.recv(2)", SOME 1)
      val () = debug' "channel.recv(2)"
      fun tryLp () =
        case cleanAndDeque (outQ) of
            SOME (rtxid, (msg, st)) =>
             (let
                fun matchLp () =
                 (let
                    val res = cas (rtxid, 0, 2)
                  in
                    if res = 0 then
                      (let (* WAITING -- we got it *)
                        val _ = prio := 1
                        val () = TID.mark (TID.getCurThreadId ())
                        val _ = L.releaseCmlLock lock (TID.tidNum())
                        val rthrd = PT.prep (st)
                        val _ = S.atomicReady (rthrd)
                      in
                        msg
                      end)
                    else if res = 1 then matchLp () (* CLAIMED *)
                    else tryLp () (* SYNCHED *)
                  end) (* matchLp ends *)
              in
                matchLp ()
              end)
          | NONE =>
              S.atomicSwitchToNext (fn rt => (cleanAndEnque (inQ, (mkTxId (), rt))
                                              ; L.releaseCmlLock lock (TID.tidNum ())))
          (* tryLp ends *)
    in
      tryLp ()
    end

  fun recvPoll (CHAN {prio, inQ, outQ, lock}) =
    let
      val () = Assert.assertNonAtomic' "channel.recvPoll"
      val () = Assert.assertNonAtomic' "channel.recvPoll(1)"
      val () = debug' "channel.recvPoll(1)"
      val () = atomicBegin ()
      val () = L.getCmlLock lock TID.tidNum
      val () = Assert.assertAtomic' ("channel.recvPoll(2)", SOME 1)
      val () = debug' "channel.recvPoll(2)"
      fun tryLp () =
        case cleanAndDeque (outQ) of
            SOME (rtxid, (msg, st)) =>
             (let
                fun matchLp () =
                 (let
                    val res = cas (rtxid, 0, 2)
                  in
                    if res = 0 then
                      (let (* WAITING -- we got it *)
                        val _ = prio := 1
                        val () = TID.mark (TID.getCurThreadId ())
                        val _ = L.releaseCmlLock lock (TID.tidNum())
                        val rthrd = PT.prep (st)
                        val _ = S.atomicReady (rthrd)
                      in
                        SOME msg
                      end)
                    else if res = 1 then matchLp () (* CLAIMED *)
                    else tryLp () (* SYNCHED *)
                  end) (* matchLp ends *)
              in
                matchLp ()
              end)
          | NONE => NONE
          (* tryLp ends *)
    in
      tryLp ()
    end

end
