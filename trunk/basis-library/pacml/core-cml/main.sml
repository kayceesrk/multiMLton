structure Main : MAIN =
struct

  open Critical

  structure S = Scheduler
  structure SQ = SchedulerQueues
  structure SH = SchedulerHooks
  structure TO = Timeout
  structure TID = ThreadID
  structure MT = MLtonThread
  structure PT = ProtoThread

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))



  (* Enabling preemption for processor 0
   * For other processors, preemprtion is enabled when they are scheduled
   *)
  val _ = PacmlFFI.enablePreemption ()

  (* Dummy signal handler *)
  (* Since signals are handled by a separate pthread, the working threads are
  * not interrupted and thus failing to interrupt the syscalls that could be
  * restarted. Hence, the signal handler thread sends SIGUSR2 to each worker
  * thread, which corresponds to this handler *)

  val h = MLtonSignal.Handler.handler (fn t => t)
  (* Install handler for processor 0*)
  val _ = MLtonSignal.setHandler (Posix.Signal.usr2, h)

  fun thread_main () =
  let
    val _ = MLtonProfile.init ()
    val _ = PacmlFFI.disablePreemption ()
    val _ = MLtonSignal.setHandler (Posix.Signal.usr2, h)
    fun loop procNum =
    let
      val _ = PacmlFFI.maybeWaitForGC ()
    in
      case SQ.deque (RepTypes.PRI) of
           NONE => (PacmlFFI.wait (); loop procNum)
         | SOME (t) =>
             let
               val _ = if !Config.isRunning then ()
                       else (loop procNum)
               val _ = atomicBegin ()
               val _ = PacmlFFI.enablePreemption ()
             in
               S.atomicSwitch (fn _ => t)
             end
    end
  in
    loop (PacmlFFI.processorNumber ())
  end

  val () = (_export "Parallel_run": (unit -> unit) -> unit;) thread_main

  fun alrmHandler thrd =
    let
      val () = Assert.assertAtomic' ("RunCML.alrmHandler", NONE)
      val () = debug' "alrmHandler" (* Atomic 1 *)
      val () = Assert.assertAtomic' ("RunCML.alrmHandler", SOME 1)
      val () = S.preempt thrd
      val () = TO.preemptTime ()
      val _ = TO.preempt ()
      val nextThrd = S.next()
    in
      nextThrd
    end

  fun pause () =
  let
    fun tightLoop2 n =
      if n=0 then ()
      else tightLoop2 (n-1)

    fun tightLoop n = (* tightLoop (1000) ~= 1ms on 1.8Ghz core *)
      if n=0 then ()
      else (tightLoop2 300; tightLoop (n-1))
  in
    tightLoop (Config.pauseToken)
  end


  fun pauseHook (iter) =
    let
      (* If there are waiting time events, then make proc 0 spin *)
      val to = TO.preempt ()
      val iter = case to of
                    NONE => if (iter > Config.maxIter) then (PacmlFFI.wait (); iter-1) else iter
                  | _ => if (iter > Config.maxIter) then (TO.preemptTime (); 0) else iter
      val () = if not (!Config.isRunning) then (atomicEnd ();ignore (SchedulerHooks.deathTrap())) else ()
    in
      S.nextWithCounter (iter + 1)
    end

  fun reset running =
      (S.reset running
      ; SH.reset ()
      ; TID.reset ()
      ; TO.reset ())


  val numIOThreads = PacmlFFI.numIOProcessors

  fun run (initialProc : unit -> unit) =
  let
    val installAlrmHandler = fn (h) => MLtonSignal.setHandler (Posix.Signal.alrm, h)
    val status =
        S.switchToNext
        (fn thrd =>
        let
          val () = reset true
          val () = SH.shutdownHook := PT.prepend (thrd, fn arg => (atomicBegin (); arg))
          val () = SH.pauseHook := pauseHook
          val () = ignore (Thread.spawnHost (fn ()=> (Config.isRunning := true;initialProc ())))
          val handler = MLtonSignal.Handler.handler (S.unwrap alrmHandler Thread.reifyHostFromParasite)
          val () = installAlrmHandler handler
          (* Spawn the Non-blocking worker threads *)
          val _ = List.tabulate (numIOThreads * 5, fn _ => NonBlocking.mkNBThread ())
        in
            ()
        end)
      val () = reset false
      val () = Config.isRunning := false
      val () = atomicEnd ()
    in
      status
    end

   fun shutdown status =
         if (!Config.isRunning)
            then S.switch (fn _ => PT.getRunnableHost(PT.prepVal (!SH.shutdownHook, status)))
            else raise Fail "CML is not running"

  (* init MUST come after waitForWorkLoop has been exported *)
  val () = (_import "Parallel_init": unit -> unit;) ()

end
