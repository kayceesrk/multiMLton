signature SCHEDULER_HOOKS =
sig
    type 'a thread = 'a RepTypes.thread
    type runnable_host = RepTypes.runnable_host

    (* this hook gets invoked when the scheduler has nothing else to do;
      * it is invoked in an atomic region
      *)
    val pauseHook : (int * Time.time option option -> runnable_host) ref

    (* this hook points to a thread that gets invoked when
      * the system is otherwise deadlocked.  It takes two arguments:
      * the first is a boolean flag that says weather to do clean-up,
      * and the second is the exit status.
      *)
    val shutdownHook : (OS.Process.status) thread ref

    val reset : unit -> unit
    val deathTrap : unit -> unit
end
