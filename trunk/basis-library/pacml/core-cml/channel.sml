structure Channel : CHANNEL_EXTRA =
struct
  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Critical

  structure Q = CirQueue
  structure L = Lock
  structure TID = ThreadID
  structure S = Scheduler
  structure PT = ProtoThread
  structure E = Event
  structure T = Thread

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => msg()^"."^(PT.getThreadTypeString())
                                   ^" : "^Int.toString(PacmlFFI.processorNumber()))

  datatype thread = datatype RepTypes.thread
  datatype thread_type = datatype RepTypes.thread_type
  type 'a sevt = 'a Event.sevt

  datatype 'a chan = CHAN of {prio: int ref,
                              inQ : (int ref * ('a thread)) Q.t,
                              outQ : (int ref * ('a * unit thread)) Q.t,
                              lock : L.cmlLock}

  fun channel () = CHAN {prio = ref 1,
                         inQ = Q.new (),
                         outQ = Q.new (),
                         lock = L.initCmlLock ()}


  fun sameChannel (CHAN {prio = prio1, ...}, CHAN {prio =
        prio2, ...}) =
            prio1 = prio2
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
          ; if isEmpty q
              then 0
              else bumpPriority prio)

      fun cleanAndDeque q =
        (cleanPrefix (q, cleaner);
         deque (q))

      fun cleanAndEnque (q, item) =
        (cleanSuffix (q, cleaner);
         enque (q, SOME item))

      val undeque =
        fn (q, item) => undeque (q, SOME item)

  end

  fun send (ch as CHAN {prio, inQ, outQ, lock}, msg) =
    let
      val () = Assert.assertNonAtomic' "channel.send"
      val () = Assert.assertNonAtomic' "channel.send(1)"
      val () = debug' (fn () => "channel.send(1)")
      val () = atomicBegin ()
      val () = L.getCmlLock lock PT.getLockId
      val () = Assert.assertAtomic' ("channel.send(2)", SOME 1)
      val () = debug' (fn () => "channel.send(2)")
      fun tryLp () =
        case cleanAndDeque (inQ) of
              SOME (rtxid, rt) =>
                (let
                  val () = debug' (fn () => "Channel.send.tryLp")
                  fun matchLp () =
                    (let
                      val () = debug' (fn () => "Channel.send.matchLp")
                      val res = cas (rtxid, 0, 2)
                     in
                      if res = 0 then
                        (let (* WAITING -- we got it *)
                          val _ = prio := 1
                          val () = L.releaseCmlLock lock PT.getLockId
                          val rthrd = PT.prepVal (rt, msg)
                          (* rthrd should not be used after atomicReady *)
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
                                               ; L.releaseCmlLock lock PT.getLockId
                                               ; debug' (fn () => "Channel.send.NONE")))
                (* tryLp ends *)
      val () = tryLp ()
    in
      ()
    end (* send ends *)

  fun aSend (ch, v) =
    ProtoThread.spawnParasite
      (fn () => let
                  val _ = PT.disableParasitePreemption ()
                  val _ = send (ch, v)
                in
                  PT.enableParasitePreemption ()
                end)


  fun sendEvt (CHAN {prio, inQ, outQ, lock}, msg) =
    let
      fun doitFn () =
        let
          val () = Assert.assertAtomic' ("Channel.sendEvt.doitFn", NONE)
          val () = debug' (fn () => "Channel.sendEvt(3.1.1)") (* Atomic 1 *)
          val () = Assert.assertAtomic' ("Channel.sendEvt(3.1.1)", SOME 1)
          val () = L.getCmlLock lock PT.getLockId
          fun tryLp () =
            case cleanAndDeque (inQ) of
                  SOME (rtxid, rt) =>
                    (let
                      val () = debug' (fn () => "Channel.sendEvt.tryLp")
                      fun matchLp () =
                        (let
                          val () = debug' (fn () => "Channel.sendEvt.matchLp")
                          val res = cas (rtxid, 0, 2)
                        in
                          if res = 0 then
                            (let (* WAITING -- we got it *)
                              val _ = prio := 1
                              val () = L.releaseCmlLock lock PT.getLockId
                              val rthrd = PT.prepVal (rt, msg)
                              (* rthrd should not be used after atomicReady *)
                            in
                              S.atomicReady (rthrd) (* Implicit atomic end *)
                            end)
                          else if res = 1 then matchLp () (* CLAIMED *)
                          else tryLp () (* SYNCHED *)
                        end) (* matchLp ends *)
                    in
                      matchLp ()
                    end) (* SOME ends *)
                | NONE => (L.releaseCmlLock lock PT.getLockId;
                           raise RepTypes.DOIT_FAIL)
          (* tryLp ends *)
          val () = tryLp ()
        in
          ()
        end (* doitFn ends *)
      fun blockFn (mytxid) =
        let
          val () = Assert.assertAtomic' ("Channel.sendEvt.blockFn", NONE)
          val () = debug' (fn () => "Channel.sendEvt(3.2.1)") (* Atomic 1 *)
          val () = Assert.assertAtomic' ("Channel.sendEvt(3.2.1)", SOME 1)
          val () = L.getCmlLock lock PT.getLockId
          fun tryLp () =
            case cleanAndDeque (inQ) of
                  SOME (v as (rtxid, rt)) =>
                    let
                      val () = if mytxid = rtxid then raise Fail "Same event" else ()
                      val () = debug' (fn () => "Channel.sendEvt.tryLp")
                      fun matchLp () =
                        let
                          val () = debug' (fn () => "Channel.sendEvt.matchLp")
                          val res = cas (mytxid, 0, 1) (* Try to claim it *)
                        in
                          if res = 0 then
                            let
                              val res2 = cas (rtxid, 0, 2)
                            in
                              (if res2 = 0 then
                                (let (* WAITING -- we got it *)
                                  val _ = prio := 1
                                  val _ = mytxid := 2
                                  val () = L.releaseCmlLock lock PT.getLockId
                                  val rthrd = PT.prepVal (rt, msg)
                                  val () = T.reifyCurrentIfParasite () (* XXX KC temp fix for exceptions *)
                                in
                                  S.ready (rthrd) (* Implicit atomic end *)
                                end)
                              else if res2 = 1 then
                                (mytxid := 0; matchLp ())
                              else (mytxid := 0; tryLp ()))
                            end
                          else if res = 1 then matchLp () (* CLAIMED *) (* In timeEvt *)
                          else (undeque (inQ, v);
                                L.releaseCmlLock lock PT.getLockId ;
                                S.atomicSwitchToNext (fn _ => ()))
                        end (* matchLp ends *)
                    in
                      matchLp ()
                    end (* SOME ends *)
                | NONE =>
                    let
                      val msg = S.atomicSwitchToNext (fn st => (cleanAndEnque (outQ, (mytxid, (msg, st)))
                                                  ; L.releaseCmlLock lock PT.getLockId
                                                  ; debug' (fn () => "Channel.sendEvt.NONE")))
                      (* XXX KC temp fix for exceptions *)
                      val () = atomicBegin ()
                      val () = T.reifyCurrentIfParasite ()
                    in
                      msg
                    end
          (* tryLp ends *)
          val () = tryLp ()
        in
          ()
        end (* blockFn ends *)
    fun pollFn () =
      let
        val () = Assert.assertAtomic' ("Channel.sendEvt.pollFn", NONE)
        val () = debug' (fn () => "Channel.sendEvt(2)") (* Atomic 1 *)
        val () = Assert.assertAtomic' ("Channel.sendEvt(2)", SOME 1)
        val () = L.getCmlLock lock PT.getLockId
        val v = cleanAndChk (prio, inQ)
        val () = L.releaseCmlLock lock PT.getLockId
        val () = debug' (fn () => "Channel.sendEvt(3)") (* Atomic 1 *)
      in
        case v of
            0 => E.blocked blockFn
          | prio => E.enabled {prio = prio, doitFn = doitFn}
      end
    in
      E.bevt pollFn
    end

  fun aSendEvt (ch, msg) = E.aevt(sendEvt (ch, msg))


  fun sendPoll (CHAN {prio, inQ, outQ, lock}, msg) =
    let
      val () = Assert.assertNonAtomic' "channel.sendPoll"
      val () = Assert.assertNonAtomic' "channel.sendPoll(1)"
      val () = debug' (fn () => "channel.sendPoll(1)")
      val () = atomicBegin ()
      val () = L.getCmlLock lock PT.getLockId
      val () = Assert.assertAtomic' ("channel.sendPoll(2)", SOME 1)
      val () = debug' (fn () => "channel.sendPoll(2)")
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
                          val () = L.releaseCmlLock lock PT.getLockId
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


  fun recvEvt (CHAN {prio, inQ, outQ, lock}) =
    let
      fun doitFn () =
        let
          val () = Assert.assertAtomic' ("Channel.recvEvt.doitFn", NONE)
          val () = debug' (fn () => "Channel.recvEvt(3.1.1)") (* Atomic 1 *)
          val () = Assert.assertAtomic' ("Channel.recvEvt(3.1.1)", SOME 1)
          val () = L.getCmlLock lock PT.getLockId
          fun tryLp () =
            case cleanAndDeque (outQ) of
                  SOME (stxid, (msg, st)) =>
                    (let
                      val () = debug' (fn () => "Channel.recvEvt.tryLp")
                      fun matchLp () =
                        (let
                          val () = debug' (fn () => "Channel.recvEvt.matchLp")
                          val res = cas (stxid, 0, 2)
                        in
                          if res = 0 then
                            (let (* WAITING -- we got it *)
                              val _ = prio := 1
                              val () = L.releaseCmlLock lock PT.getLockId
                              val rthrd = PT.prep (st)
                              val _ = S.atomicReady (rthrd) (* Implicit atomic end *)
                            in
                              msg
                            end)
                          else if res = 1 then matchLp () (* CLAIMED *)
                          else tryLp () (* SYNCHED *)
                        end) (* matchLp ends *)
                    in
                      matchLp ()
                    end) (* SOME ends *)
                | NONE => (L.releaseCmlLock lock PT.getLockId;
                           raise RepTypes.DOIT_FAIL)
          (* tryLp ends *)
        in
          tryLp ()
        end (* doitFn ends *)
      fun blockFn (mytxid) =
        let
          val () = Assert.assertAtomic' ("Channel.recvEvt.blockFn", NONE)
          val () = debug' (fn () => "Channel.recvEvt(3.2.1)") (* Atomic 1 *)
          val () = Assert.assertAtomic' ("Channel.recvEvt(3.2.1)", SOME 1)
          val () = L.getCmlLock lock PT.getLockId
          fun tryLp () =
            case cleanAndDeque (outQ) of
                  SOME (v as (stxid, (msg, st))) =>
                    let
                      val () = if mytxid = stxid then raise Fail "Same event" else ()
                      val () = debug' (fn () => "Channel.recvEvt.tryLp")
                      fun matchLp () =
                        let
                          val () = debug' (fn () => "Channel.recvEvt.matchLp")
                          val res = cas (mytxid, 0, 1) (* Try to claim it *)
                        in
                          if res = 0 then
                            let
                              val res2 = cas (stxid, 0, 2)
                            in
                              (if res2 = 0 then
                                (let (* WAITING -- we got it *)
                                  val _ = prio := 1
                                  val _ = mytxid := 2
                                  val () = L.releaseCmlLock lock PT.getLockId
                                  val rthrd = PT.prep (st)
                                  val () = T.reifyCurrentIfParasite () (* XXX KC temp fix for exceptions *)
                                  val _ = S.ready (rthrd) (* Implicit atomic end *)
                                in
                                  msg
                                end)
                              else if res2 = 1 then
                                (mytxid := 0; matchLp ())
                              else (mytxid := 0; tryLp ()))
                            end
                          else if res = 1 then matchLp () (* CLAIMED *)
                          else (undeque (outQ, v);
                                L.releaseCmlLock lock PT.getLockId;
                                S.atomicSwitchToNext (fn _ => ()))
                        end (* matchLp ends *)
                    in
                      matchLp ()
                    end (* SOME ends *)
                | NONE =>
                    let
                      val msg = S.atomicSwitchToNext (fn rt => (cleanAndEnque (inQ, (mytxid, rt))
                                                  ; L.releaseCmlLock lock PT.getLockId
                                                  ; debug' (fn () => "Channel.recvEvt.NONE")))
                      (* XXX KC temp fix for exceptions *)
                      val () = atomicBegin ()
                      val () = T.reifyCurrentIfParasite ()
                    in
                      msg
                    end

          (* tryLp ends *)
        in
          tryLp ()
        end (* blockFn ends *)
    fun pollFn () =
      let
        val () = Assert.assertAtomic' ("Channel.recvEvt.pollFn", NONE)
        val () = debug' (fn () => "Channel.recvEvt(2)") (* Atomic 1 *)
        val () = Assert.assertAtomic' ("Channel.recvEvt(2)", SOME 1)
        val () = L.getCmlLock lock PT.getLockId
        val v = cleanAndChk (prio, outQ)
        val () = L.releaseCmlLock lock PT.getLockId
        val () = debug' (fn () => "Channel.recvEvt(3)") (* Atomic 1 *)
      in
        case v of
            0 => E.blocked blockFn
          | prio => E.enabled {prio = prio, doitFn = doitFn}
      end
    in
      E.bevt pollFn
    end

  fun aRecvEvt (ch) = E.aevt(recvEvt (ch))

  fun recv (CHAN {prio, inQ, outQ, lock}) =
    let
      val () = Assert.assertNonAtomic' "channel.recv"
      val () = Assert.assertNonAtomic' "channel.recv(1)"
      val () = debug' (fn () => "channel.recv(1)")
      val () = atomicBegin ()
      val () = L.getCmlLock lock PT.getLockId
      val () = Assert.assertAtomic' ("channel.recv(2)", SOME 1)
      val () = debug' (fn () => "channel.recv(2)")
      fun tryLp () =
        case cleanAndDeque (outQ) of
            SOME (stxid, (msg, st)) =>
             (let
                val () = debug' (fn () => "Channel.recv.tryLp")
                fun matchLp () =
                 (let
                    val () = debug' (fn () => "Channel.recv.matchLp")
                    val res = cas (stxid, 0, 2)
                  in
                    if res = 0 then
                      (let (* WAITING -- we got it *)
                        val _ = prio := 1
                        val _ = L.releaseCmlLock lock PT.getLockId
                        val rthrd = PT.prep (st)
                        (* rthrd should not be used after atomic ready *)
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
                                              ; L.releaseCmlLock lock PT.getLockId
                                              ; debug' (fn () => "Channel.recv.NONE")))
          (* tryLp ends *)
    in
      tryLp ()
    end

  fun recvPoll (CHAN {prio, inQ, outQ, lock}) =
    let
      val () = Assert.assertNonAtomic' "channel.recvPoll"
      val () = Assert.assertNonAtomic' "channel.recvPoll(1)"
      val () = debug' (fn () => "channel.recvPoll(1)")
      val () = atomicBegin ()
      val () = L.getCmlLock lock PT.getLockId
      val () = Assert.assertAtomic' ("channel.recvPoll(2)", SOME 1)
      val () = debug' (fn () => "channel.recvPoll(2)")
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
                        val _ = L.releaseCmlLock lock PT.getLockId
                        val rthrd = PT.prep (st)
                        (* rthrd should not be used after atomicReady *)
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
