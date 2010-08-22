(* scheduler-hooks.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* scheduler.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure SchedulerHooks: SCHEDULER_HOOKS =
   struct
      structure B = Basic
      structure R = Running
      datatype thread = datatype RepTypes.thread
      type rdy_thread = RepTypes.rdy_thread

      val pauseHookDefault : int -> rdy_thread =
        if not (!R.isRunning) then
          fn _ =>
          (let
            val _ = B.return ()
          in
            raise Fail "SchedulerHooks: Should not reach here"
          end)
        else
	      fn _ => raise Fail "SchedulerHooks.pauseHook"
      val pauseHook = ref pauseHookDefault

      val shutdownHookDefault : (bool * OS.Process.status) thread =
	 THRD (ThreadID.bogus "shutdownHook", MLtonThread.new (fn _ =>
	       raise Fail "SchedulerHooks.shutdownHook"))
      val shutdownHook = ref shutdownHookDefault

      fun reset () =
	 (pauseHook := pauseHookDefault
	  ; shutdownHook := shutdownHookDefault
	  ; ())
   end
