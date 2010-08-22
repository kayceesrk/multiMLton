(* channel.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* channel.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of synchronous channels.
 *
 *
 * XXX KC : This is not true anymore.
 * ----------------------------------------------------------------
 * To ensure that we always leave the atomic region exactly once, we
 * require that the blocking operation be responsible for leaving the
 * atomic region (in the event case, it must also execute the clean-up
 * action).  The doitFn always transfers control to the blocked thread
 * without leaving the atomic region.  Note that the send (and sendEvt)
 * blockFns run using the receiver's thread ID.
 *)

structure Channel : CHANNEL_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure Q = ImpQueue
      structure S = Scheduler
      structure E = Event
      structure L = Lock
      structure B = Basic

      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], (msg))
      fun debug' msg = debug (fn () => msg^" : "
                                    ^Int.toString(B.processorNumber()))


      datatype trans_id = datatype TransID.trans_id
      datatype trans_id_state = datatype TransID.trans_id_state


      datatype 'a chan =
         CHAN of {prio : int ref,
                  inQ  : (trans_id * ('a S.thread) * int) Q.t,
                  outQ : (trans_id * (('a S.thread * int) S.thread) * int) Q.t,
                  lock : L.cmlLock}

      (*
      fun resetChan (CHAN {prio, inQ, outQ}) =
         (prio := 1
          ; Q.reset inQ
          ; Q.reset outQ)
      *)

      fun channel () =
        CHAN {prio = ref 1, inQ = Q.new (), outQ = Q.new (), lock = L.initCmlLock ()}

      (* sameChannel : ('a chan * 'a chan) -> bool *)
      fun sameChannel (CHAN {prio = prio1, ...}, CHAN {prio =
        prio2, ...}) =
            prio1 = prio2


      (* bump a priority value by one, returning the old value *)
      fun bumpPriority (p as ref n) =
        L.fetchAndAdd (p,1)

      (* functions to clean channel input and output queues *)
      local
         fun cleaner (TXID {txst,cas}, _, _) =
            case cas (txst, SYNCHED,SYNCHED) of SYNCHED => true | _ => false
      in
         fun cleanAndChk (prio, q) : int =
           (Q.clean (q, cleaner)
             ; if Q.empty q
                  then 0
                  else bumpPriority prio)

         fun cleanAndDeque q =
            Q.cleanAndDeque (q, cleaner)

         fun enqueAndClean (q, item) =
            Q.enqueAndClean (q, item, cleaner)

      end

      fun pN () : int  = B.processorNumber ()

      fun send (CHAN {prio, inQ, outQ, lock}, msg) =
         let
            val () = Assert.assertNonAtomic' "Channel.send"
            val () = debug' "Chennel.send(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.send(1)"
            val () = S.atomicBegin ()
            val () = debug' "Acquiring lock"
            val () = L.getCmlLock lock (S.tidNum())
            val () = debug' "Channel.send(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.send(2)", SOME 1)
            val curProcNum = pN ()

            fun tryLp () =
               (case cleanAndDeque (inQ) of
                  SOME (rtxid as TXID {txst, cas}, rt, procNum) =>
                    let fun matchLp () =
                      (case cas(txst, WAITING, SYNCHED) of
                            WAITING => (* we got it *)
                              (let
                                  val () = debug' "Channel.send(3.1.1)" (* Atomic 1 *)
                                  val () = Assert.assertAtomic' ("Channel.send(3.1.1)", SOME 1)
                                  (* val () = if not (Q.empty (inQ) orelse Q.empty(outQ)) then Assert.fail ("Both qs not empty") else () *)
                                  val () = prio := 1
                                  val () = debug' ("Channel.send(3.1.2) Adding thread to "
                                                  ^Int.toString(procNum))
                                  val () = debug' "Releasing lock"
                                  val () = L.releaseCmlLock lock (S.tidNum())
                                  val () = S.readyOnProc (S.prepVal (rt, msg), procNum)
                                  val () = S.atomicEnd ()
                                  val () = debug' "Channel.send(3.1.3)" (* NonAtomic *)
                                  val () = Assert.assertNonAtomic' "Channel.send(3.1.3)"
                              in
                                  ()
                              end)
                          | CLAIMED => matchLp ()
                          | SYNCHED => tryLp ()
                      ) (* matchLP ends *)
                    in
                      case !txst of
                           SYNCHED => tryLp ()
                         | _ => matchLp ()
                    end
                    (* case SOME ends *)
                | NONE =>
                     let
                        val () = debug' "Channel.send(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.send(3.2.1)", SOME 1)
                        (* val () = if not (Q.empty (inQ) orelse Q.empty(outQ)) then Assert.fail ("Both qs not empty") else () *)
                        val (rt, rProcNum) =
                           S.atomicSwitchToNext
                           (fn st =>
                           (Q.enque (outQ, (TransID.mkTxId (), st, curProcNum))
                           ; debug' "In atomicSwitchToNext : After send(3.2.1)"
                           ; L.releaseCmlLock lock (S.tidNum())))
                        val () = debug' "Channel.send(3.2.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' ("Channel.send(3.2.2")
                        val () = debug' ("Channel.send(3.2.3) Adding thread to "
                                         ^Int.toString(rProcNum))
                        val () = S.readyOnProc(S.prepVal (rt,msg), rProcNum)
                        (* yield *)
                        (*val () = S.readyAndSwitchToNext (fn()=>())*)
                        val () = debug' "Chanell.send(3.2.4)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.send(3.2.2)"
                     in
                        ()
                     end)
                     (* tryLp ends *)
            val () = tryLp ()
            val () = debug' "Channel.send(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.send(4)"

         in
           ()
         end

      fun sendEvt (CHAN {prio, inQ, outQ, lock}, msg) =
         let
            fun doitFn () =
               let
                  val () = Assert.assertAtomic' ("Channel.sendEvt.doitFn", NONE)
                  val () = debug' "Channel.sendEvt(3.1.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.sendEvt(3.1.1)", SOME 1)
                  val curProcNum = pN ()
                  fun tryLp () =
                    case Q.deque inQ of
                       NONE => (debug' "Channel.sendEvt(3.1.2).NONE";
                                L.releaseCmlLock lock (S.tidNum());
                                NONE)
                     | SOME (rtxid as TXID {txst, cas}, rt, procNum) =>
                         let
                           val () = debug' "Channel.sendEvt(3.1.2).SOME"
                           fun matchLp () =
                             case cas (txst, WAITING, SYNCHED) of
                                  WAITING => ( prio := 1
                                             ; L.releaseCmlLock lock (S.tidNum())
                                             ; S.readyOnProc (S.prepVal (rt, msg), procNum)
                                             ; S.atomicEnd ()
                                             ; SOME ())
                                | CLAIMED => matchLp ()
                                | _ => tryLp ()
                         in
                           case !txst of
                                WAITING => matchLp ()
                              | CLAIMED => matchLp ()
                              | SYNCHED => tryLp ()
                         end
                  val () = L.getCmlLock lock (S.tidNum())
                  val ret = tryLp ()
                  val () = debug' "Channel.sendEvt(3.1.3)"
                  val () = case ret of
                                NONE => Assert.assertAtomic' ("Channel.sendEvt(3.1.3)", SOME 1)
                              | SOME _ => Assert.assertNonAtomic' "Channel.sendEvt(3.1.4)"
               in
                 ret
               end
            fun blockFn {transId as TXID {txst = myTxst, cas = myCas}, cleanUp, next} =
               let
                  val () = Assert.assertAtomic' ("Channel.sendEvt.blockFn", NONE)
                  val () = debug' "Channel.sendEvt(3.2.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.sendEvt(3.2.1)", SOME 1)
                  val curProcNum = pN ()
                  fun tryLp () =
                   case (Q.deque inQ) of
                        NONE => let
                                  val () = debug'
                                  "Channel.sendEvt(3.2.2).tryLp.NONE" (* Atomic 1 *)
                                  val (rt, rProcNum) =
                                    S.atomicSwitch
                                    (fn st => (enqueAndClean (outQ, (transId, st, curProcNum))
                                              ; L.releaseCmlLock lock (S.tidNum())
                                              ; next ()))
                                  val () = S.readyOnProc (S.prepVal (rt, msg), rProcNum)
                                in
                                  ()
                                end
                      | SOME (v as (rtxid as TXID {txst, cas}, rt, procNum)) =>
                          let
                            val () = debug' "Channel.sendEvt(3.2.2).tryLp.SOME"
                            val () = if myTxst = txst then
                                        raise Fail "Same event"
                                     else ()
                            fun matchLp () =
                              (case myCas(myTxst, WAITING, CLAIMED) of
                                   (* try to claim the matching event *)
                                   WAITING => (case cas (txst, WAITING, SYNCHED) of
                                                   WAITING => (* We got it *)
                                                        (prio := 1
                                                      ; myTxst := SYNCHED
                                                      ; L.releaseCmlLock lock (S.tidNum())
                                                      ; S.atomicEnd ()
                                                      ; S.readyOnProc (S.prepVal (rt, msg), procNum))
                                                 | CLAIMED =>   (myTxst := WAITING
                                                              ; matchLp ())
                                                 | SYNCHED => (myTxst := WAITING
                                                            ; tryLp ()))
                                  (* In timeEvt *)
                                | CLAIMED => matchLp ()
                                | SYNCHED =>
                                        (Q.undeque(inQ, v);
                                         L.releaseCmlLock lock (S.tidNum());
                                         S.atomicSwitchToNext (fn _ => ())))
                          in
                            case !txst of
                                 WAITING => matchLp ()
                               | CLAIMED => matchLp ()
                               | SYNCHED => tryLp ()
                          end
                  val () = L.getCmlLock lock (S.tidNum())
                  val ret = tryLp ()
                  val () = S.doAtomic (cleanUp)
                  val () = debug' "Channel.sendEvt(3.2.3)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Channel.sendEvt(3.2.2)"
               in
                 ret
               end
          fun pollFn () =
             let
               val () = Assert.assertAtomic' ("Channel.sendEvt.pollFn", NONE)
               val () = debug' "Channel.sendEvt(2)" (* Atomic 1 *)
               val () = Assert.assertAtomic' ("Channel.sendEvt(2)", SOME 1)
               val () = L.getCmlLock lock (S.tidNum())
               val v = cleanAndChk (prio, inQ)
               val () = L.releaseCmlLock lock (S.tidNum())
               val () = debug' "Channel.sendEvt(3)" (* Atomic 1 *)
             in
                case v of
                   0 => E.blocked blockFn
                 | prio => E.enabled {prio = prio, doitFn = doitFn}
             end
         in
            E.bevt pollFn
         end

      fun sendPoll (CHAN {prio, inQ, lock, ...}, msg) =
         let
            val () = Assert.assertNonAtomic' "Channel.sendPoll"
            val () = debug' "Chennel.sendPoll(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.sendPoll(1)"
            val () = S.atomicBegin ()
            val () = debug' "Acquiring lock"
            val () = L.getCmlLock lock (S.tidNum())
            val () = debug' "Channel.sendPoll(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.sendPoll(2)", SOME 1)
            fun tryLp () =
               (case cleanAndDeque (inQ) of
                  SOME (rtxid as TXID {txst, cas}, rt, procNum) =>
                    let fun matchLp () =
                      (case cas(txst, WAITING, SYNCHED) of
                            WAITING => (* we got it *)
                              (let
                                  val () = debug' "Channel.sendPoll(3.1.1)" (* Atomic 1 *)
                                  val () = Assert.assertAtomic' ("Channel.sendPoll(3.1.1)", SOME 1)
                                  val () = prio := 1
                                  val () = debug' ("Channel.sendPoll(3.1.2) Adding thread to "
                                                  ^Int.toString(procNum))
                                  val () = debug' "Releasing lock"
                                  val () = L.releaseCmlLock lock (S.tidNum())
                                  val () = S.readyOnProc (S.prepVal (rt, msg), procNum)
                                  val () = S.atomicEnd ()
                                  (* KC : yield *)
                                  (*val () = S.readyAndSwitchToNext (fn()=>())*)
                                  val () = debug' "Channel.sendPoll(3.1.3)" (* NonAtomic *)
                                  val () = Assert.assertNonAtomic' "Channel.sendPoll(3.1.3)"
                              in
                                true
                              end)
                          | CLAIMED => matchLp ()
                          | SYNCHED => tryLp ()
                      ) (* matchLP ends *)
                    in
                      case !txst of
                           SYNCHED => tryLp ()
                         | _ => matchLp ()
                    end
                    (* case SOME ends *)
                | NONE =>
                     let
                        val () = debug' "Channel.sendPoll(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.sendPoll(3.2.1)", SOME 1)
                        val () = L.releaseCmlLock lock (S.tidNum())
                        val () = S.atomicEnd ()
                        val () = debug' "Channel.sendPoll(3.2.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' ("Channel.sendPoll(3.2.2")
                        val () = debug' "Chanell.sendPoll(3.2.4)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.sendPoll(3.2.2)"
                     in
                       false
                     end)
                     (* tryLp ends *)
            val msg = tryLp ()
            val () = debug' "Channel.sendPoll(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.sendPoll(4)"
         in
           msg
         end

      fun recv (CHAN {prio, inQ, outQ, lock}) =
         let
            val () = Assert.assertNonAtomic' "Channel.recv"
            val () = debug' "Channel.recv(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recv(1)"
            val () = S.atomicBegin ()
            val () = debug' "Acquiring lock"
            val () = L.getCmlLock lock (S.tidNum())
            val () = debug' "Channel.recv(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.recv(2)", SOME 1)
            val curProcNum = pN ()
            fun tryLp () =
               (case cleanAndDeque (outQ) of
                  SOME (TXID {txst, cas}, st, procNum) =>
                   (let fun matchLp () =
                    (case cas(txst, WAITING, SYNCHED) of
                          WAITING => (* we got it *)
                            (let
                                val () = debug' "Channel.recv(3.1.1)" (* Atomic 1 *)
                                val () = Assert.assertAtomic' ("Channel.recv(3.1.1)", SOME 1)
                                val msg =
                                  S.atomicSwitchToNext
                                  (fn rt =>
                                    (debug' ("Channel.recv(3.1.2) Adding thread to "
                                            ^Int.toString(procNum))
                                    ;prio := 1
                                    ; debug' "Releasing lock"
                                    ; L.releaseCmlLock lock (S.tidNum ())
                                    ; S.readyOnProc (S.prepVal (st, (rt, curProcNum)), procNum)
                                    ))
                                val () = debug' "Channel.recv(3.1.3)" (* NonAtomic *)
                                val () = Assert.assertNonAtomic' "Channel.recv(3.1.1)"
                            in
                                msg
                            end)
                        | CLAIMED => matchLp ()
                        | SYNCHED => tryLp ()
                    ) (* matchLp ends*)
                  in
                    case !txst of
                         SYNCHED => tryLp ()
                       | _ => matchLp ()
                  end)
                  (* case SOME ends *)
                | NONE =>
                     let
                        val () = debug' "Channel.recv(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recv(3.2.1)", SOME 1)
                        val msg =
                           S.atomicSwitchToNext
                           (fn rt => (enqueAndClean (inQ, (TransID.mkTxId (), rt, curProcNum))
                           ; debug' "In atomicSwitchToNext : After recv(3.2.1)"
                           ; debug' "Releasing lock"
                           ; L.releaseCmlLock lock (S.tidNum())))
                        val () = debug' "Channel.recv(3.2.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.recv(3.2.2)"
                     in
                        msg
                     end)
                     (* tryLp ends *)
            val msg = tryLp ()
            val () = debug' "Channel.recv(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recv(4)"
         in
            msg
         end

      fun recvEvt (CHAN {prio, inQ, outQ, lock})  =
         let
            fun doitFn () =
               let
                  val () = Assert.assertAtomic' ("Channel.recvEvt.doitFn", NONE)
                  val () = debug' "Channel.recvEvt(3.1.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.recvEvt(3.1.1)", SOME 1)
                  val () = L.getCmlLock lock (S.tidNum())
                  val curProcNum = pN ()
                  fun tryLp () =
                    case (Q.deque outQ) of
                       NONE => (debug' "Channel.recvEvt(3.1.2).NONE";
                                L.releaseCmlLock lock (S.tidNum());
                                NONE)
                     | SOME (stxid as TXID {txst, cas}, st, procNum) =>
                         let
                           val () = debug' "Channel.recvEvt(3.1.2).SOME"
                           fun matchLp () =
                             case cas (txst, WAITING, SYNCHED) of
                                  WAITING =>
                                    let
                                      val _ =  prio := 1
                                      val _ = L.releaseCmlLock lock (S.tidNum ())
                                      val res = (S.atomicSwitchToNext (fn rt =>
                                                    S.readyOnProc (S.prepVal (st, (rt, curProcNum)), procNum)))
                                    in
                                      SOME res
                                    end
                                | CLAIMED => matchLp ()
                                | _ => tryLp ()
                         in
                           case !txst of
                                WAITING => matchLp ()
                              | CLAIMED => matchLp ()
                              | SYNCHED => tryLp ()
                         end
                  val msg = tryLp ()
                  val () = debug' "Channel.recvEvt(3.1.3)"
                  val () = case msg of
                                NONE => Assert.assertAtomic' ("Channel.recvEvt(3.1.3)", SOME 1)
                              | SOME _ => Assert.assertNonAtomic' "Channel.recvEvt(3.1.4)"
               in
                  msg
               end
            fun blockFn {transId as TXID {txst = myTxst, cas = myCas}, cleanUp, next} =
               let
                  val () = Assert.assertAtomic' ("Channel.recvEvt.blockFn", NONE)
                  val () = debug' "Channel.recvEvt(3.2.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.recvEvt(3.2.1)", SOME 1)
                  val curProcNum = pN ()
                 fun tryLp () =
                   (case (Q.deque outQ) of
                        NONE => (let
                                  val () = debug'
                                  "Channel.recvEvt(3.2.2).tryLp.NONE" (* Atomic 1 *)
                                  val msg = S.atomicSwitch
                                            (fn rt =>
                                                  (enqueAndClean (inQ, (transId,
                                                  rt, curProcNum))
                                                  ; L.releaseCmlLock lock (S.tidNum())
                                                   ; next ()))
                                in
                                  msg
                                end)
                      | SOME (v as (stxid as TXID {txst, cas}, st, procNum)) =>
                          (let
                            val () = debug' "Channel.recvEvt(3.2.2).tryLp.SOME" (* Atomic 1 *)
                            val () = if myTxst = txst then
                                        raise Fail "Same event"
                                     else ()
                            fun matchLp () =
                              (case myCas(myTxst, WAITING, CLAIMED) of
                                   (* try to claim the matching event *)
                                   WAITING => (case cas (txst, WAITING, SYNCHED) of
                                                   WAITING => (* We got it *)
                                                        S.atomicSwitchToNext
                                                       (fn rt => (prio := 1
                                                      ; myTxst := SYNCHED
                                                      ; L.releaseCmlLock lock (S.tidNum())
                                                      ; S.readyOnProc (S.prepVal
                                                      (st, (rt,curProcNum)), procNum)))
                                                 | CLAIMED =>   (myTxst := WAITING
                                                              ; matchLp ())
                                                 | SYNCHED => (myTxst := WAITING
                                                            ; tryLp ()))
                                  (* In timeEvt *)
                                | CLAIMED => matchLp ()
                                | SYNCHED =>
                                         (Q.undeque(outQ, v);
                                         L.releaseCmlLock lock (S.tidNum());
                                         (* One of the events was synched.
                                          * switch to another thread *)
                                         S.atomicSwitchToNext ( fn _ => ())))
                          in
                            (case !txst of
                                 WAITING => matchLp ()
                               | CLAIMED => matchLp ()
                               | SYNCHED => tryLp ())
                          end))
                  val () = L.getCmlLock lock (S.tidNum())
                  val msg = tryLp ()
                  val () = S.doAtomic (cleanUp)
                  val () = debug' "Channel.recvEvt(3.2.3)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Channel.recvEvt(3.2.3)"
               in
                 msg
               end
            fun pollFn () =
               let
                  val () = Assert.assertAtomic' ("Channel.recvEvt.pollFn", NONE)
                  val () = debug' "Channel.recvEvt(2)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Channel.recvEvt(2)", SOME 1)
                  val () = L.getCmlLock lock (S.tidNum())
                  val v = cleanAndChk (prio, outQ)
                  val () = L.releaseCmlLock lock (S.tidNum())
               in
                  case v of
                     0 => E.blocked blockFn
                   | prio => E.enabled {prio = prio, doitFn = doitFn}
               end
         in
            E.bevt pollFn
         end

      fun recvPoll (CHAN {prio, outQ, lock, ...}) =
         let
            val () = Assert.assertNonAtomic' "Channel.recvPoll"
            val () = debug' "Channel.recvPoll(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recvPoll(1)"
            val () = S.atomicBegin ()
            val () = debug' "Acquiring lock"
            val () = L.getCmlLock lock (S.tidNum())
            val () = debug' "Channel.recvPoll(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Channel.recvPoll(2)", SOME 1)
            val p = pN ()
            fun tryLp () =
               (case cleanAndDeque outQ of
                  SOME (stxid as TXID {txst, cas}, st, procNum) =>
                  let fun matchLp () =
                    (case cas(txst, WAITING, SYNCHED) of
                          WAITING => (* we got it *)
                            (let
                                val () = debug' "Channel.recvPoll(3.1.1)" (* Atomic 1 *)
                                val () = Assert.assertAtomic' ("Channel.recvPoll(3.1.1)", SOME 1)
                                val msg =
                                  S.atomicSwitchToNext
                                  (fn rt =>
                                    (debug' ("Channel.recvPoll(3.1.2) Adding thread to "
                                            ^Int.toString(procNum))
                                    ;prio := 1
                                    ; debug' "Releasing lock"
                                    ; L.releaseCmlLock lock (S.tidNum ())
                                    ; S.readyOnProc (S.prepVal (st, (rt, p)), procNum)
                                    ))
                                val () = debug' "Channel.recvPoll(3.1.3)" (* NonAtomic *)
                                val () = Assert.assertNonAtomic' "Channel.recvPoll(3.1.1)"
                            in
                                SOME msg
                            end)
                        | CLAIMED => matchLp ()
                        | SYNCHED => tryLp ()
                    ) (* matchLp ends*)
                  in
                    case !txst of
                         SYNCHED => tryLp ()
                       | _ => matchLp ()
                  end
                  (* case SOME ends *)
                | NONE =>
                     let
                        val () = debug' "Channel.recvPoll(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Channel.recvPoll(3.2.1)", SOME 1)
                        val () = L.releaseCmlLock lock (S.tidNum())
                         val () = S.atomicEnd ()
                        val () = debug' "Channel.recvPoll(3.2.2)" (* NonAtomic *)
                        val () = Assert.assertNonAtomic' "Channel.recvPoll(3.2.2)"
                     in
                        NONE
                     end)
                     (* tryLp ends *)
            val msg = tryLp ()
            val () = debug' "Channel.recvPoll(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Channel.recvPoll(4)"
         in
            msg
         end
   end
