(* Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)


structure Threadlet :> THREADLET =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

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

  datatype 'a recvThreadlet = AR of (threadlet * S.statRec * (unit -> 'a) ref)
                            | MR of ('a S.thread * int)

  datatype 'a sendThreadlet = AS of (threadlet * S.statRec * 'a)
                            | MS of (unit S.thread * 'a * int)


  (* Copies frames from given an offset from bottom of stack to the previous frame's top *)
  val printFrames = _import "GC_printFrames" : unit -> unit;
  val copyFrames = _import "GC_copyFrames" : int -> threadlet;
  val copiedSize = _import "GC_getCopiedSize" : unit -> int;

  datatype thread_state = datatype RepTypes.thread_state
  datatype statType = datatype RepTypes.statType
  datatype 'a asyncChan = ACHAN of {inQ : ('a recvThreadlet) Q.t,
                                    outQ : ('a sendThreadlet) Q.t,
                                    lock : L.cmlLock}

  fun newAChan () = ACHAN {inQ = Q.new (), outQ = Q.new (), lock = L.initCmlLock ()}

  val dontInline = Primitive.dontInline


  val async = S.async

  fun aSend (ACHAN {inQ, outQ, lock}, msg) =
    let
      val () = Assert.assertNonAtomic' "Threadlet.aSend"
      val () = Assert.assertNonAtomic' "Threadlet.aSend(1)"
      val () = debug' "Threadlet.aSend(1)"
      val () = S.atomicBegin ()
      val () = L.getCmlLock lock (S.tidNum())
      val () = Assert.assertAtomic' ("Threadlet.aSend(2)", SOME 1)
      val () = debug' "Threadlet.aSend(2)"
      val () =
        (case Q.deque (inQ) of
              SOME (e) => ((case e of
                                 AR (thlet, stats, handoffFun) =>
                                    let
                                      val _ = L.releaseCmlLock lock (S.tidNum ())
                                      val _ = handoffFun := (fn () => msg)
                                      val _ = debug' "aSend-SOME-AR"
                                      val _ = S.atomicPrefixAndSwitchTo (thlet, stats) (* Implicit atomicEnd *)
                                    in
                                      ()
                                    end
                               | MR (thrd, procNum) =>
                                   let
                                     val _ = L.releaseCmlLock lock (S.tidNum ())
                                     val _ = debug' "aSend-SOME-MR"
                                     val rdyThrd = S.prepVal (thrd, msg)
                                     val _ = S.readyOnProc (rdyThrd, procNum)
                                     val _ = S.atomicEnd ()
                                   in
                                     ()
                                   end);
                            S.incStat (NBSEND)
                           (* SOME ends *))
            | NONE =>
                let
                  val state = S.getThreadletType ()
                  val _ = S.incStat (BSEND)
                  val _ = (case state of
                                MAIN =>
                                  let
                                    val _ = debug' "aSend-NONE-MAIN"
                                    val procNum = B.processorNumber ()
                                    val () =
                                        S.atomicSwitchToNext (
                                              fn st =>
                                                (Q.enque (outQ, MS (st, msg, procNum))
                                                ; L.releaseCmlLock lock (S.tidNum ())))
                                  in
                                    ()
                                  end
                              | ASYNC =>
                                  let
                                    val _ = debug' "aSend-NONE-ASYNC"
                                    (* sandBox will be executed only during the first time.
                                      * when then Async resumes, this is not executed. *)
                                    fun sandBox () =
                                      let
                                        val _ = debug' "aSend sandBox"
                                        val thlet = copyFrames (S.getNextPointer ())
                                        val _ = S.statCopyParasite (copiedSize ())
                                        val _ = Q.enque (outQ, AS (thlet, S.getStats (), msg))
                                        val _ = L.releaseCmlLock lock (S.tidNum ())
                                        val _ = Prim.jumpDown (S.getNextPointer ())  (* Implicit atomicEnd *)
                                        val _ = print "\naSend : Should not see this"
                                      in
                                        ()
                                      end
                                    val _ = dontInline (sandBox)
                                  in
                                    ()
                                  end)

                in
                  () (* NONE ends *)
                end
        )
    in
      () (* aSend ends *)
    end


  fun aRecv (ACHAN {inQ, outQ, lock}) =
    let
      val () = Assert.assertNonAtomic' "Threadlet.aRecv"
      val () = Assert.assertNonAtomic' "Threadlet.aRecv(1)"
      val () = debug' "Threadlet.aRecv(1)"
      val () = S.atomicBegin ()
      val () = L.getCmlLock lock (S.tidNum())
      val () = Assert.assertAtomic' ("Threadlet.aRecv(2)", SOME 1)
      val () = debug' "Threadlet.aRecv(2)"
      val msg = (case Q.deque (outQ) of
                    SOME (e) => (S.incStat (NBRECV);
                                 (case e of
                                      AS (thlet, stats, msg) =>
                                        let
                                          val _ = debug' "aRecv-SOME-AS"
                                          val _ = L.releaseCmlLock lock (S.tidNum ())
                                          val _ = S.atomicPrefixAndSwitchTo (thlet, stats)
                                          (* Atomic 0 *)
                                        in
                                          msg
                                        end
                                    | MS (thrd, msg, procNum) =>
                                        let
                                          val _ = L.releaseCmlLock lock (S.tidNum ())
                                          val _ = debug' "aRecv-SOME-MS"
                                          val rdyThrd = S.prepVal (thrd, ())
                                          val _ = S.readyOnProc (rdyThrd, procNum)
                                          val _ = S.atomicEnd ()
                                        in
                                          msg
                                        end)
                                      (* SOME ends *))
                  | NONE =>
                      let
                        val state = S.getThreadletType ()
                        val _ = S.incStat (BRECV)
                        val msg = (case state of
                                       MAIN =>
                                        let
                                          val _ = debug' "aRecv-NONE-MAIN"
                                          val procNum = B.processorNumber ()
                                          val msg =
                                            S.atomicSwitchToNext (
                                                fn rt =>
                                                  (Q.enque (inQ, MR (rt, procNum))
                                                  ; L.releaseCmlLock lock (S.tidNum ())))
                                        in
                                          msg
                                        end
                                     | ASYNC =>
                                        let
                                          val _ = debug' "aRecv-NONE-ASYNC"
                                          val handoffFun = ref (fn () => Primitive.MLton.bogus ())
                                          (* sandBox will be executed only during the first time.
                                            * when then Async resumes, this is not executed. *)
                                          fun sandBox () =
                                            let
                                              val _ = debug' "aRecv sandBox"
                                              val thlet = copyFrames (S.getNextPointer ())
                                              val _ = S.statCopyParasite (copiedSize ())
                                              val _ = Q.enque (inQ, AR (thlet, S.getStats (), handoffFun))
                                              val _ = L.releaseCmlLock lock (S.tidNum ())
                                              val _ = Prim.jumpDown (S.getNextPointer ()) (* Implicit atomicEnd () *)
                                              (* Atomic 0 *)
                                              val _ = print "\naRecv : Should not see this"
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
    in
      msg
    end
end
