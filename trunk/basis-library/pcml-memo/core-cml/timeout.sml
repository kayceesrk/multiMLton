(* timeout.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* timeout.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Events for synchronizing on timeouts.
 *)

(* Modified by KC
 * --------------
 * Only processor number 0 would do anything useful with preempt (). All other
 * processors return NONE.
 *)

structure TimeOut : TIME_OUT_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure S = Scheduler
      structure E = Event
      structure B = Basic
      structure L = Lock

      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg^" : "
                                    ^Int.toString(B.processorNumber()))

      datatype trans_id = datatype TransID.trans_id
      datatype trans_id_state = datatype TransID.trans_id_state


      (* this holds an approximation of the current time of day.  It is
       * cleared at each pre-emption, and initialized on demand (by getTime).
       *)
      val clock = ref NONE

      (* returns an approximation of the current time of day
       * (this is at least as accurate as the time quantum).
       *)
      fun getTime () =
         case !clock of
            NONE => let val t = Time.now()
                    in clock := SOME t;  t
                    end
          | SOME t => t
      fun preemptTime () = clock := NONE

      (* The queue of threads waiting for timeouts.
       * It is sorted in increasing order of time value.
       *)
      structure TQ = FunPriorityQueue(structure Key = struct open Time type t = time end)
      type item = trans_id * (unit -> unit) * S.rdy_thread * int (* processor number *)
      val timeQ : item TQ.t ref = ref (TQ.new ())
      val lock = L.initCmlLock ()

      fun cleaner (readied: unit -> unit) elt =
         let
            val now = getTime ()
            val (TXID {txst, cas}, cleanUp: unit -> unit, t, procNum) = TQ.Elt.value elt
           fun matchLp () =
            case cas (txst, WAITING, CLAIMED) of
               SYNCHED => true
             | WAITING => if Time.<=(TQ.Elt.key elt, now)
                       then (readied ()
                             ; S.doAtomic (cleanUp)
                             ; txst := SYNCHED
                             ; S.readyOnProc (t, procNum)
                             ; true)
                       else (txst := WAITING;
                             false)
             | CLAIMED => matchLp ()
         in
           matchLp ()
         end

      fun pN () = B.processorNumber ()

      fun timeWait (time, txid, cleanUp, t) =
         (Assert.assertAtomic' ("TimeOut.timeWait", NONE)
         ; let
             val curProcNum = pN ()
           in
             timeQ := TQ.enqueAndClean(!timeQ, time, (txid, cleanUp, t, curProcNum), cleaner (fn () => ()))
           end
          )

      (** NOTE: unlike for most base events, the block functions of time-out
       ** events do not have to exit the atomic region or execute the clean-up
       ** operation.  This is done when they are removed from the waiting queue.
       **)
      fun timeOutEvt time =
         let
            fun blockFn {transId, cleanUp, next} =
               let
                  val () = debug' "timeOutEvt(3.2.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("TimeOut.timeOutEvt(3.2.1)", SOME 1)
                  val _ = L.getCmlLock lock (S.tidNum())
                  val () =
                     S.atomicSwitch
                     (fn t =>
                      (timeWait (Time.+(time, getTime ()), transId, cleanUp, S.prep t)
                       ; L.releaseCmlLock lock (S.tidNum ())
                       ; next ()))
                  val () = debug' "timeOutEvt(3.2.3)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "TimeOut.timeOutEvt(3.2.3)"
               in
                  ()
               end
            fun pollFn () =
               let
                  val () = Assert.assertAtomic' ("TimeOut.timeOutEvt.pollFn", NONE)
                  val () = debug' "timeOutEvt(2)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("TimeOut.timeOutEvt(2)", SOME 1)
               in
                  if Time.<=(time, Time.zeroTime)
                     then E.enabled {prio = ~1, doitFn = fn () => SOME
                     (S.atomicEnd ())}
                     else E.blocked blockFn
               end
         in
            E.bevt pollFn
         end

      fun atTimeEvt time =
         let
            fun doitFn () =
            let
                  val () = debug' "atTimeEvt(3.1.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("TimeOut.atTimeEvt(3.2.1)", SOME 1)
                  val () = S.atomicEnd ()
            in
              SOME ()
            end
            fun blockFn {transId, cleanUp, next} =
               let
                  val () = debug' "atTimeEvt(3.2.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("TimeOut.atTimeEvt(3.2.1)", SOME 1)
                  val _ = L.getCmlLock lock (S.tidNum())
                  val () =
                     S.atomicSwitch
                     (fn t =>
                      (timeWait (time, transId, cleanUp, S.prep t)
                       ; L.releaseCmlLock lock (S.tidNum ())
                       ; next ()))
                  val () = debug' "atTimeEvt(3.2.3)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "TimeOut.atTimeEvt(3.2.3)"
               in
                  ()
               end
            fun pollFn () =
               let
                  val () = debug' "atTimeEvt(2)" (* Atomic 1 *)
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
      fun reset () = timeQ := TQ.new ()

      (* what to do at a preemption *)
      fun preempt () : Time.time option option =
         let
            val () = Assert.assertAtomic' ("TimeOut.preempt", SOME 1)
            val timeQ' = !timeQ
         in
           if not (B.processorNumber () = 0) then
             NONE
           else
             let
               val _ = L.getCmlLock lock (S.tidNum())
               val _ = preemptTime ()
               val res = if TQ.empty timeQ'
                          then NONE
                          else let
                                  val readied = ref false
                                  val timeQ' = TQ.clean (timeQ', cleaner (fn () => readied := true))
                                  val () = timeQ := timeQ'
                                in
                                  if !readied
                                      then SOME NONE
                                      else case TQ.peek timeQ' of
                                              NONE => NONE
                                            | SOME elt => SOME(SOME(Time.-(TQ.Elt.key elt, getTime ())))
                                end
               val _ = L.releaseCmlLock lock (S.tidNum())
             in
               res
             end

         end
   end
