(* Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)


structure PChannel :> P_CHANNEL_EXTRA =
struct

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  structure Prim = Primitive.MLton.Threadlet
  structure Pointer = Primitive.MLton.Pointer
  structure Q = ImpQueue
  structure S = Scheduler
  structure B = Basic
  structure L = Lock

  type threadlet = Primitive.MLton.Thread.thread

  fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], (msg))
  fun debug' msg = debug (fn () => msg^" : "
                               ^Int.toString(B.processorNumber()))

  (* Transaction ID mapping
   * ----------------------
   * 0 - WAITING
   * 1 - CLAIMED
   * 2 - SYNCHED
   *
   * See, John Reppy et al., Parallel CML, ICFP 09, for synchronization protocol
   *
   * XXX KC -- Wish I had macros..
   *)

  fun debug'' msg =  print (msg^"["^Int.toString(B.processorNumber())^"]\n")


  (* Copies frames from given an offset from bottom of stack to the previous frame's top *)
  val printFrames = _import "GC_printFrames" : unit -> unit;
  val copyFrames = _import "GC_copyFrames" : int -> threadlet;

  fun mkTxId () = ref 0
  val cas = ParallelInternal.vCompareAndSwap

  datatype thread_type = datatype RepTypes.thread_type

  datatype 'a recvThreadlet = datatype 'a Scheduler.recvThreadlet
  datatype 'a sendThreadlet = datatype 'a Scheduler.sendThreadlet

  datatype 'a chan = CHAN of {prio: int ref,
                                inQ : (int ref * ('a recvThreadlet)) Q.t,
                                outQ : (int ref * ('a sendThreadlet)) Q.t,
                                lock : L.cmlLock}

  fun channel () = CHAN {prio = ref 1,
                           inQ = Q.new (),
                           outQ = Q.new (),
                           lock = L.initCmlLock ()}

  (* bump a priority value by one, returning the old value *)
  fun bumpPriority (p as ref n) =
    L.fetchAndAdd (p,1)


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

  val dontInline = Primitive.dontInline

  val pSpawn = S.async

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
              SOME (rtxid, e) =>
                (let
                  fun matchLp () =
                    (let
                      val res = cas (rtxid, 0, 2)
                     in
                      if res = 0 then
                        (let (* WAITING -- we got it *)
                          val _ = prio := 1
                          val () = ThreadID.mark (B.getCurThreadId ())
                          val _ =
                            (case e of
                                AR (thlet, handoffFun) =>
                                  let
                                    val _ = L.releaseCmlLock lock (TID.tidNum())
                                    val _ = handoffFun := (fn () => msg)
                                    val _ = debug' "send-SOME-AR"
                                    val _ = S.atomicPrefixAndSwitchTo (thlet) (* Implicit atomicEnd *)
                                  in
                                    ()
                                  end
                              | MR (thrd, procNum) =>
                                  let
                                    val _ = L.releaseCmlLock lock (TID.tidNum())
                                    val _ = debug' "send-SOME-MR"
                                    val rdyThrd = S.prepVal (thrd, msg)
                                    val _ = S.readyOnProc (rdyThrd, procNum)
                                    val _ = S.atomicEnd ()
                                  in
                                    ()
                                  end)
                        in
                          ()
                        end)
                      else if res = 1 then matchLp () (* CLAIMED *)
                      else tryLp () (* SYNCHED *)
                    end) (* matchLp ends *)
                in
                  matchLp ()
                end) (* SOME ends *)
            | NONE =>
                let
                  val state = S.getThreadType ()
                  val _ = (case state of
                                HOST =>
                                  let
                                    val _ = debug' "send-NONE-HOST"
                                    val procNum = B.processorNumber ()
                                    val () =
                                        S.atomicSwitchToNext (
                                              fn st =>
                                                (cleanAndEnque (outQ, (mkTxId(), MS (st, msg, procNum)))
                                                ; (L.releaseCmlLock lock (TID.tidNum()))))
                                  in
                                    ()
                                  end
                              | PARASITE =>
                                  let
                                    val _ = debug' "send-NONE-PARASITE"
                                    (* sandBox will be executed only during the first time.
                                      * when then Async resumes, this is not executed. *)
                                    fun sandBox () =
                                      let
                                        val _ = debug' "send sandBox"
                                        val thlet = copyFrames (S.getParasiteBottom ())
                                        val _ = cleanAndEnque (outQ, (mkTxId(), AS (thlet, msg)))
                                        val _ = L.releaseCmlLock lock (TID.tidNum())
                                        val _ = Prim.jumpDown (S.getParasiteBottom ())  (* Implicit atomicEnd *)
                                        val _ = print "\nsend : Should not see this"
                                      in
                                        ()
                                      end
                                    val _ = dontInline (sandBox)
                                  in
                                    ()
                                  end)
                in
                  ()
                end (* NONE ends *)
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
              SOME (rtxid, e) =>
                (let
                  fun matchLp () =
                    (let
                      val res = cas (rtxid, 0, 2)
                     in
                      if res = 0 then
                        (let (* WAITING -- we got it *)
                          val _ = prio := 1
                          val () = ThreadID.mark (B.getCurThreadId ())
                          val _ =
                            (case e of
                                AR (thlet, handoffFun) =>
                                  let
                                    val _ = L.releaseCmlLock lock (TID.tidNum())
                                    val _ = handoffFun := (fn () => msg)
                                    val _ = debug' "sendPoll-SOME-AR"
                                    val _ = S.atomicPrefixAndSwitchTo (thlet) (* Implicit atomicEnd *)
                                  in
                                    ()
                                  end
                              | MR (thrd, procNum) =>
                                  let
                                    val _ = L.releaseCmlLock lock (TID.tidNum())
                                    val _ = debug' "sendPoll-SOME-MR"
                                    val rdyThrd = S.prepVal (thrd, msg)
                                    val _ = S.readyOnProc (rdyThrd, procNum)
                                    val _ = S.atomicEnd ()
                                  in
                                    ()
                                  end)
                        in
                          true
                        end)
                      else if res = 1 then matchLp () (* CLAIMED *)
                      else tryLp () (* SYNCHED *)
                    end) (* matchLp ends *)
                in
                  matchLp ()
                end) (* SOME ends *)
            | NONE =>
                let
                  val _ = L.releaseCmlLock lock (TID.tidNum())
                  val _ = S.atomicEnd ()
                in
                  false
                end (* NONE ends *)
                (* tryLp ends *)
      val () = tryLp ()
    in
      ()
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
        (case cleanAndDeque (outQ) of
            SOME (rtxid, e) =>
             (let
                fun matchLp () =
                 (let
                    val res = cas (rtxid, 0, 2)
                  in
                    if res = 0 then
                      (let (* WAITING *)
                        val _ = prio := 1
                        val () = ThreadID.mark (B.getCurThreadId ())
                        val msg =
                          case e of
                            AS (thlet, msg) =>
                              let
                                val _ = debug' "recv-SOME-AS"
                                val _ = L.releaseCmlLock lock (TID.tidNum())
                                val _ = S.atomicPrefixAndSwitchTo (thlet)
                                (* Atomic 0 *)
                              in
                                msg
                              end
                          | MS (thrd, msg, procNum) =>
                              let
                                val _ = debug' "recv-SOME-MS"
                                val _ = L.releaseCmlLock lock (TID.tidNum())
                                val rdyThrd = S.prepVal (thrd, ())
                                val _ = S.readyOnProc (rdyThrd, procNum)
                                val _ = S.atomicEnd ()
                              in
                                msg
                              end
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
              let
                val state = S.getThreadType ()
                val msg = (case state of
                                HOST =>
                                let
                                  val _ = debug' "recv-NONE-HOST"
                                  val procNum = B.processorNumber ()
                                  val msg =
                                    S.atomicSwitchToNext (
                                        fn rt =>
                                          (cleanAndEnque (inQ, (mkTxId (), MR (rt, procNum)))
                                          ; L.releaseCmlLock lock (TID.tidNum())))
                                in
                                  msg
                                end
                              | PARASITE =>
                                let
                                  val _ = debug' "recv-NONE-PARASITE"
                                  val handoffFun = ref (fn () => Primitive.MLton.bogus ())
                                  (* sandBox will be executed only during the first time.
                                    * when then Async resumes, this is not executed. *)
                                  fun sandBox () =
                                    let
                                      val _ = debug' "recv sandBox"
                                      val thlet = copyFrames (S.getParasiteBottom ())
                                      val _ = cleanAndEnque (inQ, (mkTxId(), AR (thlet, handoffFun)))
                                      val _ = L.releaseCmlLock lock (TID.tidNum())
                                      val _ = Prim.jumpDown (S.getParasiteBottom ()) (* Implicit atomicEnd () *)
                                      (* Atomic 0 *)
                                      val _ = print "\nrecv : Should not see this"
                                    in
                                      ()
                                    end
                                  val _ = dontInline (sandBox)
                                in
                                  !handoffFun ()
                                end)
              in
                msg (* NONE ends *)
              end)
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
        (case cleanAndDeque (outQ) of
            SOME (rtxid, e) =>
             (let
                fun matchLp () =
                 (let
                    val res = cas (rtxid, 0, 2)
                  in
                    if res = 0 then
                      (let (* WAITING *)
                        val _ = prio := 1
                        val () = ThreadID.mark (B.getCurThreadId ())
                        val msg =
                          case e of
                            AS (thlet, msg) =>
                              let
                                val _ = debug' "recvPoll-SOME-AS"
                                val _ = L.releaseCmlLock lock (TID.tidNum())
                                val _ = S.atomicPrefixAndSwitchTo (thlet)
                                (* Atomic 0 *)
                              in
                                msg
                              end
                          | MS (thrd, msg, procNum) =>
                              let
                                val _ = debug' "recvPoll-SOME-MS"
                                val _ = L.releaseCmlLock lock (TID.tidNum())
                                val rdyThrd = S.prepVal (thrd, ())
                                val _ = S.readyOnProc (rdyThrd, procNum)
                                val _ = S.atomicEnd ()
                              in
                                msg
                              end
                      in
                        SOME msg
                      end)
                    else if res = 1 then matchLp () (* CLAIMED *)
                    else tryLp () (* SYNCHED *)
                  end) (* matchLp ends *)
              in
                matchLp ()
              end)
          | NONE =>
              let
                val _ = L.releaseCmlLock lock (TID.tidNum ())
                val _ = S.atomicEnd ()
              in
                NONE (* NONE ends *)
              end)
          (* tryLp ends *)
    in
      tryLp ()
    end

end
