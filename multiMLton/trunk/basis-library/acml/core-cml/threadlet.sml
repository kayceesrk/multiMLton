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

  datatype 'a recvThreadlet = AR of (threadlet * (unit -> 'a) ref)
                            | MR of ('a S.thread * int)

  datatype 'a sendThreadlet = AS of (threadlet * 'a)
                            | MS of (unit S.thread * 'a * int)


  (* Copies frames from given an offset from bottom of stack to the previous frame's top *)
  val printFrames = _import "GC_printFrames" : unit -> unit;
  val copyFrames = _import "GC_copyFrames" : int -> threadlet;

  datatype thread_state = datatype RepTypes.thread_state
  datatype 'a asyncChan = ACHAN of {inQ : ('a recvThreadlet) Q.t,
                                    outQ : ('a sendThreadlet) Q.t,
                                    lock : L.cmlLock}

  fun newAChan () = ACHAN {inQ = Q.new (), outQ = Q.new (), lock = L.initCmlLock ()}

  val dontInline = Primitive.dontInline

  val parasite = S.async

  fun pSend (ACHAN {inQ, outQ, lock}, msg) =
    let
      val () = Assert.assertNonAtomic' "Threadlet.pSend"
      val () = Assert.assertNonAtomic' "Threadlet.pSend(1)"
      val () = debug' "Threadlet.pSend(1)"
      val () = S.atomicBegin ()
      val () = L.getCmlLock lock (S.tidNum())
      val () = Assert.assertAtomic' ("Threadlet.pSend(2)", SOME 1)
      val () = debug' "Threadlet.pSend(2)"
      val () =
        (case Q.deque (inQ) of
              SOME (e) => (case e of
                                 AR (thlet, handoffFun) =>
                                    let
                                      val _ = L.releaseCmlLock lock (S.tidNum ())
                                      val _ = handoffFun := (fn () => msg)
                                      val _ = debug' "pSend-SOME-AR"
                                      val _ = S.atomicPrefixAndSwitchTo (thlet) (* Implicit atomicEnd *)
                                    in
                                      ()
                                    end
                               | MR (thrd, procNum) =>
                                   let
                                     val _ = L.releaseCmlLock lock (S.tidNum ())
                                     val _ = debug' "pSend-SOME-MR"
                                     val rdyThrd = S.prepVal (thrd, msg)
                                     val _ = S.readyOnProc (rdyThrd, procNum)
                                     val _ = S.atomicEnd ()
                                   in
                                     ()
                                   end
                           (* SOME ends *))
            | NONE =>
                let
                  val state = S.getThreadType ()
                  val _ = (case state of
                                HOST =>
                                  let
                                    val _ = debug' "pSend-NONE-HOST"
                                    val procNum = B.processorNumber ()
                                    val () =
                                        S.atomicSwitchToNext (
                                              fn st =>
                                                (Q.enque (outQ, MS (st, msg, procNum))
                                                ; L.releaseCmlLock lock (S.tidNum ())))
                                  in
                                    ()
                                  end
                              | PARASITE =>
                                  let
                                    val _ = debug' "pSend-NONE-PARASITE"
                                    (* sandBox will be executed only during the first time.
                                      * when then Async resumes, this is not executed. *)
                                    fun sandBox () =
                                      let
                                        val _ = debug' "pSend sandBox"
                                        val thlet = copyFrames (S.getNextPointer ())
                                        val _ = Q.enque (outQ, AS (thlet, msg))
                                        val _ = L.releaseCmlLock lock (S.tidNum ())
                                        val _ = Prim.jumpDown (S.getNextPointer ())  (* Implicit atomicEnd *)
                                        val _ = print "\npSend : Should not see this"
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
      () (* pSend ends *)
    end


  fun pRecv (ACHAN {inQ, outQ, lock}) =
    let
      val () = Assert.assertNonAtomic' "Threadlet.pRecv"
      val () = Assert.assertNonAtomic' "Threadlet.pRecv(1)"
      val () = debug' "Threadlet.pRecv(1)"
      val () = S.atomicBegin ()
      val () = L.getCmlLock lock (S.tidNum())
      val () = Assert.assertAtomic' ("Threadlet.pRecv(2)", SOME 1)
      val () = debug' "Threadlet.pRecv(2)"
      val msg = (case Q.deque (outQ) of
                    SOME (e) => (case e of
                                      AS (thlet, msg) =>
                                        let
                                          val _ = debug' "pRecv-SOME-AS"
                                          val _ = L.releaseCmlLock lock (S.tidNum ())
                                          val _ = S.atomicPrefixAndSwitchTo (thlet)
                                          (* Atomic 0 *)
                                        in
                                          msg
                                        end
                                    | MS (thrd, msg, procNum) =>
                                        let
                                          val _ = L.releaseCmlLock lock (S.tidNum ())
                                          val _ = debug' "pRecv-SOME-MS"
                                          val rdyThrd = S.prepVal (thrd, ())
                                          val _ = S.readyOnProc (rdyThrd, procNum)
                                          val _ = S.atomicEnd ()
                                        in
                                          msg
                                        end
                                      (* SOME ends *))
                  | NONE =>
                      let
                        val state = S.getThreadType ()
                        val msg = (case state of
                                       HOST =>
                                        let
                                          val _ = debug' "pRecv-NONE-HOST"
                                          val procNum = B.processorNumber ()
                                          val msg =
                                            S.atomicSwitchToNext (
                                                fn rt =>
                                                  (Q.enque (inQ, MR (rt, procNum))
                                                  ; L.releaseCmlLock lock (S.tidNum ())))
                                        in
                                          msg
                                        end
                                     | PARASITE =>
                                        let
                                          val _ = debug' "pRecv-NONE-PARASITE"
                                          val handoffFun = ref (fn () => Primitive.MLton.bogus ())
                                          (* sandBox will be executed only during the first time.
                                            * when then Async resumes, this is not executed. *)
                                          fun sandBox () =
                                            let
                                              val _ = debug' "pRecv sandBox"
                                              val thlet = copyFrames (S.getNextPointer ())
                                              val _ = Q.enque (inQ, AR (thlet, handoffFun))
                                              val _ = L.releaseCmlLock lock (S.tidNum ())
                                              val _ = Prim.jumpDown (S.getNextPointer ()) (* Implicit atomicEnd () *)
                                              (* Atomic 0 *)
                                              val _ = print "\npRecv : Should not see this"
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
