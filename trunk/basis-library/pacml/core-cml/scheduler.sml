structure Scheduler : SCHEDULER =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Critical

  structure SQ = SchedulerQueues
  structure TID = ThreadID
  structure GlobalDebug = Debug
  structure MT = MLtonThread
  structure PT = ProtoThread
  structure R = RepTypes
  structure SH = SchedulerHooks

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => (msg())^" : "^Int.toString(PacmlFFI.processorNumber()))

  datatype thread_type = datatype RepTypes.thread_type
  datatype thread = datatype RepTypes.thread
  datatype rdy_thread = datatype RepTypes.rdy_thread
  type parasite = RepTypes.parasite
  datatype runnable_host = datatype RepTypes.runnable_host

  fun enque1 thrd =
   (Assert.assertAtomic' ("Scheduler.enque1", NONE)
    ; SQ.enqueHost (thrd, R.PRI))

  fun enque2 thrd =
   (Assert.assertAtomic' ("Scheduler.enque2", NONE)
    ; SQ.enqueHost (thrd, R.SEC))

  fun enque (thrd as R.RHOST (tid, _)) =
    if TID.isMarked (tid) then
      enque1 thrd
    else
      enque2 thrd

  fun deque1 () =
   (Assert.assertAtomic' ("Scheduler.deque1", NONE)
    ; SQ.dequeHost (R.ANY))

  fun deque2 () =
   (Assert.assertAtomic' ("Scheduler.deque2", NONE)
   ; SQ.dequeHost (R.SEC))

  val deque = deque1

  fun promote () =
   (Assert.assertAtomic' ("Scheduler.promote", NONE)
   ; case deque2 () of
          NONE => ()
        | SOME t => enque1 t)

  fun atomicReady (rt : rdy_thread) =
    (Assert.assertAtomic' ("Scheduler.atomicReady(1)[tid:"^(TID.tidMsg())^"]", SOME 1)
    ; case rt of
      H_RTHRD (rhost) => (SQ.enqueHost (rhost, R.PRI); atomicEnd ())
    | P_RTHRD (lockId, par) => PT.atomicPrefixAndSwitchTo (lockId, par) (* Implicit atomic end *)
    ; Assert.assertNonAtomic (fn () => "Scheduler.atomicReady(2)[tid:"^(TID.tidMsg())^"]"))

  fun atomicReadyHost (rhost: runnable_host) =
    (Assert.assertAtomic' ("Scheduler.atomicReadyHost(1)[tid:"^(TID.tidMsg())^"]", SOME 1)
    ; SQ.enqueHost (rhost, R.PRI)
    ; Assert.assertAtomic' ("Scheduler.atomicReadyHost(2)[tid:"^(TID.tidMsg())^"]", SOME 1))


  fun ready (rt : rdy_thread) =
    (atomicBegin ();
     atomicReady (rt))

  fun reset running =
      (if running then (debug' (fn () => "Scheduler.reset true"))
        else (debug' (fn () => "Scheduler.reset false"))
      ; SQ.clean ())

  fun readyForSpawn (t as RHOST (tid, rt) : runnable_host) =
    (ignore (Config.incrementNumLiveThreads ())
    ; SQ.enqueHost (t, R.PRI))


  fun canPreemptParasite () =
  let
    val pBottom = PT.getParasiteBottom ()
  in
    not ((pBottom=0) orelse (not (PT.toPreemptParasite ())))
  end


  fun unwrap (f : runnable_host -> runnable_host) (reify : (int * parasite) -> runnable_host) (host: MT.Runnable.t) : MT.Runnable.t =
    let
      val () = debug' (fn () => "Scheduler.unwrap")
      val () = Assert.assertAtomic' ("Scheduler.unwrap", NONE)
      val thrdType = PT.getThreadType ()
      val pBottom = PT.getParasiteBottom ()
      val primHost = MT.toPrimitive host
      val host = MT.fromPrimitive primHost
      val host' = case thrdType of
                    PARASITE => if ((not (PT.proceedToExtractParasite (primHost, pBottom)))
                                     orelse (pBottom=0)
                                     orelse (not (PT.toPreemptParasite ()))) then
                                  let
                                    val _ = debug' (fn () => "Scheduler.unwrap.PARASITE(1)")
                                    val tid = TID.getCurThreadId ()
                                    val RHOST (tid', host') = f (RHOST (tid, host))
                                    val () = TID.setCurThreadId tid'
                                  in
                                    host'
                                  end
                                else
                                  let
                                    val _ = debug' (fn () => "Scheduler.unwrap.PARASITE(2)")
                                    val host' = MT.toPrimitive host
                                    val lockId = PT.getLockId ()
                                    val thlet = PT.extractParasiteFromHost (host', pBottom)
                                    val newHost = reify (lockId, thlet)
                                    val _ = readyForSpawn newHost
                                    val host'' = MT.fromPrimitive host'
                                    val _ = PT.disableParasitePreemption ()
                                  in
                                    host''
                                  end
                  | HOST =>
                      let
                        val _ = debug' (fn () => "Scheduler.unwrap.HOST(1)")
                        val tid = TID.getCurThreadId ()
                        val RHOST (tid', host') = f (RHOST (tid, host))
                        val _ = debug' (fn () => "Scheduler.unwrap.HOST(2)")
                        val () = TID.setCurThreadId tid'
                      in
                        host'
                      end
    in
      host'
    end

  fun nextWithCounter (iter, to) =
    if SQ.emptyHostQ () then
      (* Now check the parasitic queue *)
      (if SQ.emptyParasiteQ () then
        (!SH.pauseHook(iter, to))
       else
         let (* run the parasite on this host *)
           val (lockId, par) = SQ.dequeParasite ()

           (* save atomic state *)
           val atomicState = getAtomicState ()
           val () = setAtomicState (1)

           val () = PT.atomicPrefixAndSwitchTo (lockId, par)

           (* restore atomic state *)
           val () = setAtomicState (atomicState)
         in
           (PacmlFFI.maybeWaitForGC ();
           nextWithCounter (iter, to))
         end)
    else
      (let
        val () = Assert.assertAtomic' ("Scheduler.nextWithCounter", NONE)
        val () = debug' (fn () => "Scheduler.nextWithCounter")
        val thrd =
            case deque1 () of
              NONE => nextWithCounter (iter, to)
            | SOME thrd => thrd
      in
        thrd
      end)

  fun next () = nextWithCounter (0, NONE)

  (* what to do at a preemption (with the current thread) *)
  fun preempt (thrd as RHOST (tid, _)) =
      let
        val () = Assert.assertAtomic' ("Scheduler.preempt", NONE)
        val () =
          if TID.isMarked tid
          then (TID.unmark tid
                ; promote ()
                ; enque1 (thrd))
          else (enque2 (thrd))
      in
        ()
      end

  fun atomicSwitchAux msg (f : 'a thread -> runnable_host) : 'a =
    (Assert.assertAtomic (fn () => "Scheduler."^msg, NONE);
    debug' (fn () => "Scheduler."^msg);
    case PT.getThreadType () of
         HOST =>
           MT.atomicSwitchAux (fn t =>
           let
             val tid = TID.getCurThreadId ()
             val _ = TID.mark tid
             val RHOST (tid', t') = f (H_THRD(tid, t))
           in
             (t', fn () => TID.setCurThreadId tid')
           end)
       | PARASITE =>
           let
             val r : (unit -> 'a) ref = ref (fn () => raise Fail "atomicSwitch : Switching to a unprepared thread")
             fun dummyFrame () =
             let
               val tid = TID.getCurThreadId ()
               val _ = TID.mark tid
               val bottom = PT.getParasiteBottom ()
               val parasite = PT.copyParasite (bottom)
               val lockId = PT.getLockId ()
               val thrd = P_THRD (lockId, parasite, fn x => r := x)
               val rt = f (thrd)
               val () = Assert.assert' ("atomicSwitchAux : state corrupted. Unintended inflation??",
                                        fn () => case PT.getThreadType () of
                                                      HOST => false
                                                    | _ => true)
               val _ = enque (rt) (* ready the given thread *)
               val _ = PT.disableParasitePreemption ()
               val _ = PT.jumpDown (bottom) (* Implicit atomic end *)
             in
               print "Should not see this\n"
             end
             val _ = Primitive.dontInline (dummyFrame)
             val _ = (atomicBegin (); atomicEnd ())
           in
             !r()
           end)

  fun atomicSwitch (f) = atomicSwitchAux "atomicSwitch" f

  fun switch (f) = (atomicBegin(); atomicSwitch(f))

  fun atomicSwitchToNext (f : 'a thread -> unit) =
    (Assert.assertAtomic (fn () => "Scheduler.atomicSwitchToNext", NONE);
    case PT.getThreadType () of
         HOST => atomicSwitchAux "atomicSwitchToNext" (fn thrd => (f thrd; next ()))
       | PARASITE =>
           let
             val r : (unit -> 'a) ref = ref (fn () => raise Fail "atomicSwitchToNext : Switching to a unprepared thread")
             val _ = debug' (fn () => "Scheduler.atomicSwitchToNext on "^(PT.getThreadTypeString ()))
             fun dummyFrame () =
             let
               val tid = TID.getCurThreadId ()
               val _ = TID.mark tid
               val bottom = PT.getParasiteBottom ()
               val parasite = PT.copyParasite (bottom)
               val lockId = PT.getLockId ()
               val thrd = P_THRD (lockId, parasite, fn x => r := x)
               val () = f (thrd)
               val () = Assert.assert' ("atomicSwitchToNext : state corrupted. Unintended inflation??",
                                        fn () => case PT.getThreadType () of
                                                      HOST => false
                                                    | _ => true)
               val _ = PT.disableParasitePreemption ()
               val _ = PT.jumpDown (bottom) (* Implicit atomic end *)
             in
               print "Should not see this\n"
             end
             val _ = Primitive.dontInline (dummyFrame)
             val _ = (atomicBegin (); atomicEnd ())
           in
             !r()
           end)

  fun switchToNext (f : 'a thread -> unit) = (atomicBegin (); atomicSwitchToNext (f))

  fun atomicSwitchToNextHostForWB f =
    MT.atomicSwitchForWB
    (fn (t: MT.Runnable.t) =>
      let
        val _ = MT.threadStatus t
        (* val _ = print "WHY?" *)
        val tid = TID.getCurThreadId ()
        val _ = TID.mark tid
        val RHOST (tid', t') = (ignore (f (H_RTHRD (RHOST (tid, t)))); next ())
        val _ = MT.threadStatus t'
      in
        (t', fn () => TID.setCurThreadId (tid'))
      end)

  fun atomicSwitchToNextParasiteForWB f =
    let
      val _ = debug' (fn () => "Scheduler.atomicSwitchToNextParasiteForWB on "^(PT.getThreadTypeString ()))
      fun dummyFrame () =
      let
        val tid = TID.getCurThreadId ()
        val _ = TID.mark tid
        val bottom = PT.getParasiteBottom ()
        val parasite = PT.copyParasite (bottom)
        val lockId = PT.getLockId ()
        val thrd = P_RTHRD (lockId, parasite)
        val () = f (thrd)
        val () = Assert.assert' ("atomicSwitchToNext : state corrupted. Unintended inflation??",
                                fn () => case PT.getThreadType () of
                                              HOST => false
                                            | _ => true)
        val _ = PT.disableParasitePreemption ()
        val _ = PT.jumpDown (bottom) (* Implicit atomic end *)
      in
        print "Should not see this\n"
      end
      val _ = Primitive.dontInline (dummyFrame)
      val _ = (atomicBegin (); atomicEnd ())
    in
      ()
    end

  fun preemptOnWriteBarrier () =
  let
    val () = debug' (fn () => "preemptOnWriteBarrier")
    val tid = TID.getCurThreadId ()
    val atomicState = getAtomicState ()
    val () = setAtomicState (1)
    val () = TID.mark tid
    val () = case PT.getThreadType () of
                  PARASITE =>
                  let
                    val pbottom = PT.getParasiteBottom ()
                  in
                    (if canPreemptParasite () then
                      atomicSwitchToNextParasiteForWB (fn rt => PacmlPrim.addToPreemptOnWBA (rt, PARASITE))
                    else
                      atomicSwitchToNextHostForWB (fn rt => PacmlPrim.addToPreemptOnWBA (rt, HOST)))
                  end
                | HOST => atomicSwitchToNextHostForWB (fn rt => PacmlPrim.addToPreemptOnWBA (rt, HOST))

    val () = setAtomicState (atomicState)
    val tid' = TID.getCurThreadId ()
    val _ = Assert.assert' ("preemptOnWriteBarreier: TIDs dont match ("
                            ^(TID.tidToString tid)^", "^(TID.tidToString tid')^")"
                            , fn () => TID.sameTid (tid, tid'))
  in
      ()
  end




end
