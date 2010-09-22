(* mailbox.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* mailbox.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Asynchronous channels (called mailboxes).
 *)

structure Mailbox : MAILBOX_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure Q = FunQueue
      structure S = Scheduler
      structure E = Event
      structure L = Lock
      structure B = Basic

      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg^" : "
                                    ^Int.toString(B.processorNumber()))

      datatype trans_id = datatype TransID.trans_id
      datatype trans_id_state = datatype TransID.trans_id_state


      (* the state of a mailbox.  The queue of the NONEMPTY constructor should
       * never be empty (use EMPTY instead).
       *)
      datatype 'a state =
         EMPTY of (TransID.trans_id * 'a S.thread * int) Q.t
       | NONEMPTY of (int * 'a Q.t)

      datatype 'a mbox = MB of ('a state ref * L.cmlLock)

      (*
      fun resetMbox (MB state) = state := EMPTY (Q.new ())
      *)

      fun mailbox () = MB (ref (EMPTY (Q.new ())), L.initCmlLock ())

      fun sameMailbox (MB (s1, _), MB (s2, _)) = (s1 = s2)

      local
         fun cleaner (TXID {txst,cas}, _, _) =
            case cas (txst, SYNCHED,SYNCHED) of SYNCHED => true | _ => false
      in
         fun cleanAndDeque q =
            Q.cleanAndDeque (q, cleaner)
      end

      fun pN () : int  = B.processorNumber ()

      fun send (MB (state, lock), x) =
         let
            val () = debug' "Mailbox.send(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Mailbox.send"
            val () = Assert.assertNonAtomic' "Mailbox.send(1)"
            val () = S.atomicBegin ()
            val () = L.getCmlLock lock (S.tidNum())
            val () = debug' "Mailbox.send(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Mailbox.send(2)", SOME 1)
            val () =
               case !state of
                  EMPTY q =>
                     let
                        val () = debug' "Mailbox.send(3.1.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Mailbox.send(3.1.1)", SOME 1)
                        fun tryLp () =
                              (case (cleanAndDeque q) of
                                  (SOME (TXID {txst, cas}, t', procNum), q') =>
                                      (let fun matchLp () =
                                          (case cas (txst, WAITING, SYNCHED) of
                                               WAITING => (* We got it *)
                                                  (let
                                                    val _ = state := EMPTY q'
                                                    val _ = L.releaseCmlLock lock (S.tidNum())
                                                    val _ = S.readyOnProc (S.prepVal (t', x), procNum)
                                                  in
                                                    S.atomicEnd ()
                                                  end)
                                              | CLAIMED => matchLp ()
                                              | SYNCHED => tryLp ()
                                           ) (* matchLp ends *)
                                      in
                                        case !txst of
                                             SYNCHED => tryLp ()
                                           | _ => matchLp ()
                                      end)
                                      (* case SOME ends*)
                                  |  (NONE, _) =>
                                      (let val q = Q.new ()
                                      in state := NONEMPTY (1, Q.enque (q, x))
                                      end
                                      ; L.releaseCmlLock lock (S.tidNum())
                                      ; S.atomicEnd())
                              ) (* tryLp ends *)

                     in
                        tryLp ()
                     end
                     (* KC : sanity check. NONEMPTY can only happen when a send
                     * succeeds. We know state is empty now and we are holding
                     * the lock while we are executing tryLp - matchLp code. So
                     * we cannot get to NONEMPTY once we know the state is EMPTY
                     * *)
                | NONEMPTY (p, q) =>
                     let
                        val () = debug' "Mailbox.send(3.2.1)" (* Atomic 1 *)
                        val () = Assert.assertAtomic' ("Mailbox.send(3.2.1)", SOME 1)
                        val () = (state := NONEMPTY (p, Q.enque (q, x))
                                 ; L.releaseCmlLock lock (S.tidNum())
                                 ; S.atomicEnd ())
                     in
                       (* yield *)
                       S.readyAndSwitchToNext(fn()=>())
                     end
            val () = debug' "Mailbox.send(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Mailbox.send(4)"
         in
            ()
         end

      fun getMsg (state, q, lock) =
         let
            val (msg, q') =
               case Q.deque q of
                  SOME (msg, q') => (msg, q')
                | NONE => raise Fail "Mailbox:getMsg"
            val () = if Q.empty q'
                        then state := EMPTY (Q.new ())
                        else state := NONEMPTY (1, q')
           val () = L.releaseCmlLock lock (S.tidNum())
            val () = S.atomicEnd ()
            val () = debug' "Mailbox.getMsg(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Mailbox.getMsg(4)"
         in
            msg
         end

      fun recv (MB (state, lock)) =
         let
            val () = Assert.assertNonAtomic' "Mailbox.recv"
            val () = debug' "Mailbox.recv(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Mailbox.recv(1)"
            val () = S.atomicBegin ()
           val () = L.getCmlLock lock (S.tidNum())
            val () = debug' "Mailbox.recv(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Mailbox.recv(2)", SOME 1)
           val curProcNum = pN ()
            val msg =
               case !state of
                  EMPTY q =>
                     let
                        val msg =
                           S.atomicSwitchToNext
                           (fn t => (state := EMPTY (Q.enque (q, (TransID.mkTxId (), t, curProcNum)));
                                     L.releaseCmlLock lock (S.tidNum())))
                     in
                        msg
                     end
                | NONEMPTY (_, q) => getMsg (state, q, lock)
            val () = debug' "Mailbox.recv(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Mailbox.recv(4)"
         in
            msg
         end

    fun recvEvt (MB (state, lock)) =
      let
        fun doitFn () =
          let
            val () = Assert.assertAtomic' ("Mailbox.recvEvt.doitFn(1)", SOME 1)
            val _ = L.getCmlLock lock (S.tidNum())
            val x =
              case !state of
                   EMPTY _ => (L.releaseCmlLock lock (S.tidNum()); NONE)
                 | NONEMPTY (prio, q) =>
                  (state := NONEMPTY (prio + 1, q);
                    SOME (getMsg (state, q, lock)))
            val () = case x of
                          SOME _ => Assert.assertNonAtomic' "Mailbox.recvEvt.doitFn(2)"
                        | NONE => Assert.assertAtomic' ("Mailbox.recvEvt.doitFn(2)", SOME 1)
          in
            x
          end

        fun blockFn {transId as TXID {txst = myTxst, cas = myCas}, cleanUp: unit -> unit, next} =
          let
            val () = Assert.assertAtomic' ("Mailbox.recvEvt.blockFn(1)", SOME 1)
            val curProcNum = pN ()
            val _ = L.getCmlLock lock (S.tidNum ())
            val msg =
                case !state of
                  EMPTY q =>
                    S.atomicSwitch
                    (fn t => (state := EMPTY (Q.enque (q, (transId, t, curProcNum)))
                     ; L.releaseCmlLock lock (S.tidNum ())
                     ; next ()))
                | NONEMPTY (prio, q) =>
                    let
                      fun matchLp () =
                        (case myCas(myTxst, WAITING, CLAIMED) of
                              WAITING => (state := NONEMPTY (prio + 1, q);
                                          myTxst := SYNCHED;
                                          getMsg (state, q, lock))
                            | SYNCHED => (L.releaseCmlLock lock (S.tidNum());
                                          S.atomicSwitchToNext (fn _ => ()))
                            | CLAIMED => matchLp ())
                    in
                      matchLp ()
                    end
            val () = S.doAtomic (cleanUp)
            val () = Assert.assertNonAtomic' "Mailbox.recvEvt.blockFn(2)"
          in
            msg
          end

        fun pollFn () =
            case !state of
              EMPTY _ => E.blocked blockFn
            | NONEMPTY (prio, q) =>
                  E.enabled {prio = prio, doitFn = doitFn}
      in
        E.bevt pollFn
      end

      fun recvPoll (MB (state, lock)) =
      let val msg=
         (S.atomicBegin()
          ; L.getCmlLock lock (S.tidNum ())
          ; case !state of
               EMPTY _ => (L.releaseCmlLock lock (S.tidNum()); S.atomicEnd(); NONE)
             | NONEMPTY (_, q) => SOME (getMsg (state, q, lock)))

          val () = debug' "Mailbox.recvPoll(1)" (* NonAtomic *)
          val () = Assert.assertNonAtomic' "Mailbox.recvPoll(1)"
      in
        msg
      end
  end
