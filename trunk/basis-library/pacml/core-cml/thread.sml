structure Thread : THREAD_EXTRA =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Critical
  open ThreadID
  structure MT = MLtonThread
  structure S = Scheduler
  structure SH = SchedulerHooks
  structure TID = ThreadID
  structure PT = ProtoThread

  datatype thread_id = datatype RepTypes.thread_id
  datatype runnable_host = datatype RepTypes.runnable_host
  datatype thread = datatype RepTypes.thread
  datatype rdy_thread = datatype RepTypes.rdy_thread

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => (msg ())^"."^(PT.getThreadTypeString())
                                   ^" : "^Int.toString(PacmlFFI.processorNumber()))

  val getTid = TID.getCurThreadId

  fun generalExit (tid', clr') =
    let
      val () = Assert.assertNonAtomic' "Thread.generalExit"
      val () = debug' (fn () => "Thread.generalExit(1)") (* NonAtomic *)
      val () = Assert.assertNonAtomic' "Thread.generalExit"
      val () = Assert.assert' ("Thread.generalExit", fn () => case PT.getThreadType () of
                                                                   RepTypes.HOST => true
                                                                 | _ => false)
      val () = atomicBegin ()
      val tid as TID {dead, props, ...} = TID.getCurThreadId ()
      val () = Assert.assert ([], fn () =>
                              concat ["Thread.generalExit ",
                                      Option.getOpt (Option.map TID.tidToString tid', "NONE"),
                                      " <> ",
                                      TID.tidToString tid], fn () =>
                                case tid' of NONE => true
                                | SOME tid' => TID.sameTid (tid', tid))
      val () = if clr' then props := [] else ()
      (* val () = Event.atomicCVarSet dead XXX *)
      val () = debug' (fn () => "Thread.generalExit(2)")
    in
      if Config.decrementNumLiveThreads () then
        S.atomicSwitch
        (fn t =>
        let
          val _ = debug' (fn () => "Quiting")
          val _ = PacmlFFI.disablePreemption ()
          val shutdownRhost = PT.getRunnableHost (PT.prepFn (!SH.shutdownHook, fn () => OS.Process.success))
        in
          if (PacmlFFI.processorNumber () = 0) then
            (SH.pauseHook := (fn _ => (ignore (SH.deathTrap ());
                                       raise Fail "generalExit.Should not reach here"));
             shutdownRhost)
          else
            (S.atomicReadyHost shutdownRhost;
             S.next ())
        end)
      else
      S.atomicSwitchToNext
      (fn t => ())
    end

  fun doHandler (TID {exnHandler, id,...}, exn) =
    (print (concat ["Exception in [",Int.toString (id),"] : ", exnName exn, " : ", exnMessage exn,"\n"])
    ; ignore (OS.Process.exit OS.Process.failure)
    ; ((!exnHandler) exn) handle _ => ())

  fun wrapFunction f tid =
    let
      fun thread () =
          ((f ()) handle ex => doHandler (tid, ex)
          ; generalExit (SOME tid, false))
    in
      thread
    end

  fun exit () =
      let
        val () = Assert.assertNonAtomic' "Thread.exit"
        val () = debug' (fn () => "exit") (* NonAtomic *)
        val () = Assert.assertNonAtomic' "Thread.exit"
      in
        generalExit (NONE, true)
      end

  (* This will be assigned by Timeout *)
  val timeoutCleanup = ref (fn () => raise Fail "Thread.timeoutCleanUp not set")

  fun reifyHostFromParasite (lockId, parasite) =
  let
    val _ = Assert.assertAtomic' ("Scheduler.reifyHostFromParasite", SOME 1)
    val wf = wrapFunction
    val tid = TID.newWithTid (lockId)
    val _ = debug' (fn () => "Reifying host from parasite. "^
                      "NumThreads = "^(Int.toString(!Config.numLiveThreads))^
                      ". curtid = "^(Int.toString(TID.tidNum()))^
                      ". newtid = "^(TID.tidToString(tid)))

    (* creating a container for parasite to run *)
    fun container () =
    let
      val _ = atomicBegin ()
      val _ = PT.atomicPrefixAndSwitchToSpecial (lockId, parasite)
    in
      PacmlFFI.noop ()
    end

    val nT = MT.new (wf container tid)
    val nRt = MT.prepare (nT, ())
  in
    RHOST (tid, nRt)
  end


  fun reifyCurrent () =
  let
    val () = Assert.assertAtomic' ("Thread.reifyCurrent", SOME 1)
    val () = Assert.assert' ("Thread.reifyCurrent: Must be PARASITE",
                             fn () => case PT.getThreadType () of
                                           RepTypes.PARASITE => true
                                         | _ => false)
  in
   S.atomicSwitchToNext
    (fn t =>
        let
          val rt = PT.prep t
          val (lockId, par) = case rt of
                         P_RTHRD (lockId, p) => (lockId, p)
                       | _ => raise Fail "Thread.reifyCurrent: Impossible"
          val rhost = reifyHostFromParasite (lockId, par)
        in
          S.readyForSpawn (rhost)
        end)
  end

  fun reifyCurrentIfParasite () =
  let
    val () = Assert.assertAtomic' ("Thread.reifyCurrentIfParasite", SOME 1)
  in
    case PT.getThreadType () of
         RepTypes.PARASITE => reifyCurrent ()
       | _ => atomicEnd ()
  end


  fun yield () =
      let
        val () = Assert.assertNonAtomic' "Thread.yield"
        val () = atomicBegin ()
        (* Clean up for timeouts *)
        val () = !timeoutCleanup ()
      in
        case PT.getThreadType () of
             RepTypes.PARASITE => reifyCurrent ()
           | _ =>
              S.atomicSwitchToNext
              (fn t =>
                let
                  val RHOST (tid,mt) = PT.getRunnableHost (PT.prep (t))
                  (* Force this thread into the secondary scheduler queue *)
                  val _ = TID.unmark (tid)
                  val rhost = RHOST (tid, mt)
                in
                  S.preempt (rhost)
                end)
      end

  datatype proc_spec = ANY_PROC | ON_PROC of int

  fun spawnHostHelperEager (f, ps) =
  let
    val () = atomicBegin ()
    val tid = case ps of
                   ANY_PROC => TID.new ()
                 | ON_PROC n => TID.newOnProc (n)

    (* XXX dummy *)
    val _ = if Primitive.MLton.equal (tid, TID.getCurThreadId ()) then
              print "I should not see this\n"
            else ()

    fun thrdFun () = ((f ()) handle ex => doHandler (tid, ex);
                     generalExit (SOME tid, false))
    val thrd = H_THRD (tid, PT.new thrdFun)
    val rhost = PT.getRunnableHost (PT.prep (thrd))
    val proc = TID.getProcId (tid)


    val () =
      if proc <> PacmlFFI.processorNumber () then
          (debug' (fn () => "spawnHostHelper.lift(1): tid="^(TID.tidToString tid));
          Primitive.Lwtgc.addToMoveOnWBA (rhost);
          S.preemptOnWriteBarrier ();
          debug' (fn () => "spawnHostHelper.lift(2): tid="^(TID.tidToString tid))) (* ) *)
      else
        ()

    val () = S.readyForSpawn (rhost)

    (* If this thread was spawned on an IO processor, then decrement the
    * numLiveThreads as the IO worker threads never die *)
    val _ = case ps of
                 ANY_PROC => ()
               | ON_PROC n => if (n > (PacmlFFI.numComputeProcessors - 1)) then
                                ignore (Config.decrementNumLiveThreads ())
                              else ()
    val () = atomicEnd ()
  in
    tid
  end

  fun spawnHostHelper (f, ps) =
  let
    val _ = debug' (fn () => "spawnHostHelper")
    val () = atomicBegin ()
    val tid = case ps of
                   ANY_PROC => TID.new ()
                 | ON_PROC n => TID.newOnProc (n)
    fun thrdFun () = ((f ()) handle ex => doHandler (tid, ex);
                     generalExit (SOME tid, false))
    val thrd = H_THRD (tid, PT.new thrdFun)
    val rhost = PT.getRunnableHost (PT.prep (thrd))

    val () = S.readyForSpawn (rhost)

    (* If this thread was spawned on an IO processor, then decrement the
    * numLiveThreads as the IO worker threads never die *)
    val _ = case ps of
                 ANY_PROC => ()
               | ON_PROC n => if (n > (PacmlFFI.numComputeProcessors - 1)) then
                                ignore (Config.decrementNumLiveThreads ())
                              else ()
    val () = atomicEnd ()
  in
    tid
  end

  fun spawnHostHelperLazy (f, ps) =
  let
    val () = atomicBegin ()
    val tid = case ps of
                   ANY_PROC => TID.new ()
                 | ON_PROC n => TID.newOnProc (n)

    (* XXX dummy *)
    val _ = if Primitive.MLton.equal (tid, TID.getCurThreadId ()) then
              print "I should not see this\n"
            else ()

    fun thrdFun () = ((f ()) handle ex => doHandler (tid, ex);
                     generalExit (SOME tid, false))

    val thrd = H_THRD (tid, PT.new thrdFun)
    val rhost = PT.getRunnableHost (PT.prep (thrd))

    (* XXX dummy *)
    val _ = case rhost of
                 RHOST (tid, t) => let
                                     val _ = TID.tidToString tid
                                   in
                                    MLtonThread.threadStatus t
                                   end

    val proc = TID.getProcId (tid)
    val _ = debug' (fn () => "spawnHostHelperLazy: TID="^(TID.tidToString tid)^
                             " procId="^(Int.toString proc))
    val () =
      if proc <> PacmlFFI.processorNumber () then
        (Config.incrementNumLiveThreads ();
         PacmlPrim.addToSpawnOnWBA (H_RTHRD rhost, proc))
      else
        S.readyForSpawn (rhost)

    (* If this thread was spawned on an IO processor, then decrement the
    * numLiveThreads as the IO worker threads never die *)
    val _ = case ps of
                 ANY_PROC => ()
               | ON_PROC n => if (n > (PacmlFFI.numComputeProcessors - 1)) then
                                ignore (Config.decrementNumLiveThreads ())
                              else ()
    val () = atomicEnd ()
  in
    tid
  end

  fun spawnHost f = if (Primitive.Controls.readBarrier) then
                      spawnHostHelper (f, ANY_PROC)
                    else if (Primitive.Controls.lazySpawn) then
                      spawnHostHelperLazy (f, ANY_PROC)
                    else
                      spawnHostHelperEager (f, ANY_PROC)

  fun spawn f = spawnHost f

  fun spawnOnProc (f, n) = if (Primitive.Controls.readBarrier) then
                             spawnHostHelper (f, ON_PROC n)
                           else if (Primitive.Controls.lazySpawn) then
                             spawnHostHelperLazy (f, ON_PROC n)
                           else
                             spawnHostHelperEager (f, ON_PROC n)

  fun createHost f =
  let
    val () = atomicBegin ()
    val () = debug' (fn () => "Thread.createHost")
    val tid = TID.newOnProc (PacmlFFI.processorNumber ())
    fun thrdFun () = ((f ()) handle ex => doHandler (tid, ex);
                    generalExit (SOME tid, false))
    val thrd = H_THRD (tid, PT.new thrdFun)
    val rhost = PT.getRunnableHost (PT.prep (thrd))
    val _ = Config.incrementNumLiveThreads ()
    val () = atomicEnd ()
  in
    rhost (* NOTE: This thread must be readied and not readySpawned as numLiveThreads has already been incremented *)
  end

  fun spawnParasite f =
    let
      val numPenaltySpawnsSpawned = PT.getNumPenaltySpawns ()
    in
      if numPenaltySpawnsSpawned > 0 then
        (PT.setNumPenaltySpawns (numPenaltySpawnsSpawned - 1);
        ignore (spawnHost (f)))
      else
        (let
          val ts = Time.now ()
          val _ = PT.spawnParasite f
          val te = Time.now ()
          val d = Time.toMicroseconds (Time.-(te, ts))
          val _ = if LargeInt.>(d, Config.maxTime) then (* XXX Should take into account that parasites can be suspended *)
                    PT.setNumPenaltySpawns (Config.penalty)
                  else ()
        in
          ()
        end)
    end


end
