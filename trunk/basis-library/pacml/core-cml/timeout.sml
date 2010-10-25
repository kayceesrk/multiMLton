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
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))

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
  type item = int ref * S.rdy_thread
  val timeQ : item TQ.t ref = ref (TQ.new ())
  val lock = L.initCmlLock ()

  val cas = PacmlFFI.vCompareAndSwap

  fun cleaner (readied: unit -> unit) elt =
    let
      val now = getTime ()
      val (txid, t) = TQ.Elt.value elt
      fun matchLp () =
        let
          val res = cas (txid, 0, 1) (* Try to claim it *)
        in
          (if (res = 0) then (* Was WAITING.. we got it *)
            (if Time.<=(TQ.Elt.key elt, now) then
              (readied ()
              ; txid := 2 (* SYNCHED *)
              ; S.ready (t)
              ; true)
            else
              (txid := 0; (* Reset to WAITING *)
              false))
          else if (res = 1) then
            matchLp ()
          else
            true)
        end
    in
      matchLp ()
    end

  fun timeWait (time, txid, t) =
    (Assert.assertAtomic' ("TimeOut.timeWait", NONE);
     timeQ := TQ.enqueAndClean (!timeQ, time, (txid, t), cleaner (fn () => ()));
     PacmlFFI.wakeUp (0, 1))

  fun timeOutEvt time =
  let
    fun blockFn (txid) =
      let
        val () = debug' "timeOutEvt(3.2.1)" (* Atomic 1 *)
        val () = Assert.assertAtomic' ("TimeOut.timeOutEvt(3.2.1)", SOME 1)
        val _ = L.getCmlLock lock TID.tidNum
        val () =
          S.atomicSwitchToNext
          (fn t =>
             (timeWait (Time.+(time, getTime ()), txid, PT.prep t)
              ; L.releaseCmlLock lock (TID.tidNum ())))
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
        if Time.<=(time, Time.zeroTime) then
          E.enabled {prio = ~1, doitFn = fn () => (atomicEnd ())}
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
          val () = atomicEnd ()
        in
          ()
        end
      fun blockFn (txid) =
        let
          val () = debug' "atTimeEvt(3.2.1)" (* Atomic 1 *)
          val () = Assert.assertAtomic' ("TimeOut.atTimeEvt(3.2.1)", SOME 1)
          val _ = L.getCmlLock lock TID.tidNum
          val () =
              S.atomicSwitchToNext
              (fn t =>
              (timeWait (time, txid, PT.prep t)
                ; L.releaseCmlLock lock (TID.tidNum ())))
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
      if not (PacmlFFI.processorNumber () = 0) then
        NONE
      else
        if TQ.empty timeQ' then NONE
        else
          let
            val _ = L.getCmlLock lock TID.tidNum
            val readied = ref false
            val timeQ' = TQ.clean (timeQ', cleaner (fn () => readied := true))
            val () = timeQ := timeQ'
            val res =
              if !readied
              then SOME NONE
              else case TQ.peek timeQ' of
                        NONE => NONE
                      | SOME elt => SOME(SOME(Time.-(TQ.Elt.key elt, getTime ())))

            val _ = L.releaseCmlLock lock (TID.tidNum())
          in
            res
          end
    end







end
