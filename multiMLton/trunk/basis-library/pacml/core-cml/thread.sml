structure Thread : THREAD_EXTRA =
struct

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  open Critical
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
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))

  fun generalExit (tid', clr') =
    let
      val () = Assert.assertNonAtomic' "Thread.generalExit"
      val () = debug' "Thread.generalExit(1)" (* NonAtomic *)
      val () = Assert.assertNonAtomic' "Thread.generalExit"
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
      val () = debug' "Thread.generalExit(2)"
    in
      if Config.decrementNumLiveThreads () then
        S.atomicSwitch
        (fn t =>
        let
          val _ = debug' "Quiting"
          val _ = SH.pauseHook := (fn _ => (ignore (SH.deathTrap ()); !SH.pauseHook(0)))
        in
          PT.getRunnableHost (PT.prepFn (!SH.shutdownHook, fn () => OS.Process.success))
        end)
      else
      S.atomicSwitchToNext
      (fn t => ())
    end

  fun doHandler (TID {exnHandler, ...}, exn) =
    (print (concat ["\nException: ", exnName exn, " : ", exnMessage exn])
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
        val () = debug' "exit" (* NonAtomic *)
        val () = Assert.assertNonAtomic' "Thread.exit"
      in
        generalExit (NONE, true)
      end

  fun yield () =
      let
        val () = Assert.assertNonAtomic' "Thread.yield"
        val () = debug' "yield" (* NonAtomic *)
        val () = Assert.assertNonAtomic' "Thread.yield"
      in
        raise Fail "Thread.yield not implemented"
      end

  fun reifyHostFromParasite (parasite) =
  let
    val _ = Assert.assertAtomic' ("Scheduler.reifyHostFromParasite", SOME 1)
    val wf = wrapFunction
    val tid = TID.new ()
    val _ = debug' ("Reifying host from parasite. "^
                      "NumThreads = "^(Int.toString(!Config.numLiveThreads))^
                      ". curtid = "^(Int.toString(TID.tidNum()))^
                      ". newtid = "^(TID.tidToString(tid)))

    (* creating a container for parasite to run *)
    fun container () =
    let
      val _ = atomicBegin ()
      val _ = PT.atomicPrefixAndSwitchToSpecial (parasite)
    in
      PacmlFFI.noop ()
    end

    val nT = MT.new (wf container tid)
    val nRt = MT.prepare (nT, ())
  in
    RHOST (tid, nRt)
  end


  fun spawnHost f =
  let
    val () = atomicBegin ()
    val tid = TID.new ()
    fun thrdFun () = ((f ()) handle ex => doHandler (tid, ex);
                    generalExit (SOME tid, false))
    val thrd = H_THRD (tid, MT.new thrdFun)
    val rhost = PT.getRunnableHost (PT.prep (thrd))
    val () = S.readyForSpawn (rhost)
    val () = atomicEnd ()
  in
    tid
  end

  val spawnParasite = PT.spawnParasite
end
