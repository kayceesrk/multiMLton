structure Basic :> BASIC =
struct

  type void = unit
  type rdy_thread = RepTypes.rdy_thread
  type queue_prio = RepTypes.queue_prio

  val numberOfProcessors = ParallelInternal.numberOfProcessors

  structure TID = ThreadID
  type thread_id = TID.thread_id
  type threadlet = Primitive.MLton.Thread.thread


  structure R = Running
  structure RT = RepTypes
  structure Q = WorkQueue (struct
                             type work = rdy_thread
                             val numberOfProcessors = fn () => numberOfProcessors
                           end)
    :> WORKQUEUE where type work = rdy_thread

  val processorNumber = ParallelInternal.processorNumber

  val disablePreemption = _import "Parallel_disablePreemption": unit -> unit;
  val enablePreemption = _import "Parallel_enablePreemption": unit -> unit;
  val maybeWaitForGC = _import "Parallel_maybeWaitForGC": unit -> unit;

  exception Parallel of string

  structure T = MLtonThread
  type 'a t = 'a T.t

  (* the dummy thread Id; this is used when an ID is needed to get
  * the types right
  *)
  val dummyTid = TID.bogus "dummy"

  val curTid : thread_id array = Array.tabulate(numberOfProcessors, fn _ => dummyTid)

  fun getCurThreadId () =
    let
        val tid = Array.sub(curTid, processorNumber ())
    in
        tid
    end
  fun setCurThreadId tid =
    let
        val () = Assert.assertAtomic' ("Basic.setCurThreadId", NONE)
    in
        Array.update(curTid, processorNumber (), tid)
    end

  fun clearThreadIds () =
  let
    fun loop n =
      if n < 0 then ()
      else
        (Array.update(curTid, n, dummyTid);
         loop (n-1))
  in
    loop (numberOfProcessors-1)
  end

  (* Enabling preemption for processor 0
   * For other processors, preemprtion is enabled when they are scheduled
   *)
  val _ = enablePreemption ()

  (* Dummy signal handler *)
  (* Since signals are handled by a separate pthread, the working threads are
  * not interrupted and thus failing to interrupt the syscalls that could be
  * restarted. Hence, the signal handler thread sends SIGUSR2 to each worker
  * thread, which corresponds to this handler *)

  (* XXX DEBUG *)
  val h = MLtonSignal.Handler.handler (fn t => t)
  (* Install handler for processor 0*)
  val _ = MLtonSignal.setHandler (Posix.Signal.usr2, h)

  fun getInitWork p =
    (case Q.getWork p RT.PRI of
         NONE => Q.getWork p RT.SEC
       | w => w)

  fun waitForWork () =
    let
      fun loop p =
          let
            val _ = maybeWaitForGC ()
            val _ = if !R.isRunning
                    then ()
                    else loop p
          in
            case getInitWork p
             of NONE =>
                  ParallelInternal.wait ()
              | SOME (RepTypes.RTHRD(tid,t)) =>
                let in
                  if !R.isRunning then
                     let
                        val _ = MLtonSignal.setHandler (Posix.Signal.usr2, h)
                        val _ = T.atomicBegin ()
                        val _ = enablePreemption ()
                        (*val _ = endSpinning ()*)
                        val _ = setCurThreadId (tid)
                      in
                        T.atomicSwitch(fn _ => t)
                        handle e => TextIO.output (TextIO.stdErr,
                                             ("WARNING: Caught exception \""
                                              ^ (Primitive.Exn.name e)
                                            ^ "\" in parallel CML!\n"))
                      end
                  else
                    ()
                   end;
            (* NB we call processorNumber again here in case that this job has
             been split across two processors *)
            loop (processorNumber ())
          end

      val p = processorNumber ()
    in
      loop p
    end

  val return = (MLtonProfile.init ()
                ; disablePreemption ()
                ; MLtonSignal.setHandler (Posix.Signal.usr2, h)
                ; waitForWork)

  val finishWork = Q.finishWork

  fun empty () =
    (maybeWaitForGC ();
    Q.empty (processorNumber ()))

  fun addWork ws forceSame prio =
      let
        val p = processorNumber ()
      in
           Q.addWork p ws forceSame prio
      end

  fun addWorkTo ws procNum prio =
      (*Q.addWorkTo procNum ws*)
      Q.addWorkTo procNum ws prio

  fun getWork prio =
      let
        val p = processorNumber ()
      in
           Q.getWork p prio
      end

  fun clearWork f = Q.clearWork ()

  val () = (_export "Parallel_run": (unit -> void) -> unit;) return
  (* init MUST come after waitForWorkLoop has been exported *)
  val () = (_import "Parallel_init": unit -> unit;) ()

  val policyName = Q.policyName
  val maxBytesLive = _import "Parallel_maxBytesLive": unit -> Word64.word;
  val resetBytesLive = _import "Parallel_resetBytesLive": unit -> unit;

  val numThreadsLive = Q.numThreadsLive
end
