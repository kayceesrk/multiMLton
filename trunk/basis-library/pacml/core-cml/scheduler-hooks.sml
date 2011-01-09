structure SchedulerHooks : SCHEDULER_HOOKS =
struct
  datatype thread = datatype RepTypes.thread
  datatype runnable_host = datatype RepTypes.runnable_host

  fun deathTrap () = (PacmlFFI.maybeWaitForGC (); deathTrap ())

  val pauseHookDefault : int * Time.time option option -> runnable_host =
    if not (!Config.isRunning) then
      fn _ =>
      (let
        val _ = deathTrap ()
      in
        raise Fail "SchedulerHooks: Should not reach here"
      end)
    else
      fn _ => raise Fail "SchedulerHooks.pauseHook"
  val pauseHook = ref pauseHookDefault

  val shutdownHookDefault : (OS.Process.status) thread =
    H_THRD (ThreadID.bogus "shutdownHook" 0, MLtonThread.new (fn _ =>
          raise Fail "SchedulerHooks.shutdownHook"))
  val shutdownHook = ref shutdownHookDefault

  fun reset () =
  (pauseHook := pauseHookDefault
  ; shutdownHook := shutdownHookDefault
  ; ())
end
