functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) : WORKQUEUE =
struct

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  fun debug msg = Debug.sayDebug ([], (msg))
  fun debug' msg = debug (fn () => msg^" : "
                                    ^Int.toString(ParallelInternal.processorNumber()))



  type proc = int
  type work = W.work

  val fetchAndAdd = ParallelInternal.fetchAndAdd
  val numThreadsLive = ref 0

  local
    exception Impossible
    open TextIO
  in
  fun die n = (output (stdErr,
                       "CMLWorkQueue: die at " ^ (Int.toString n) ^ "\n");
               flushOut stdErr;
               (* XX unlock (); *)
               raise Impossible)
  end

  structure A = Array
  structure T = MLtonThread
  structure L = Lock
  structure R = RepTypes

  val numberOfProcessors = W.numberOfProcessors ()
  val pN = ParallelInternal.processorNumber

  (* These processors are used only to run IO CML thread *)
  val numIOProcs = ParallelInternal.numIOThreads

  (* Only these processors are used to run general CML threads *)
  val numComputeProcessors = numberOfProcessors - numIOProcs

  structure Q = ImpQueue

  (* private state *)
  (* Create separate queues for each processor. Each processor has a
  * primary and a secondary queue *)
  val mainQ = A.tabulate (numberOfProcessors, fn _ => (Q.new (), Q.new ()))
  val locks = A.tabulate (numberOfProcessors, fn _ => L.initCmlLock ())
  (* thread count *)
  (* val threadCount = A.tabulate (numComputeProcessors, fn x => 0) *)

  (* For spawning in a round robin fashion *)
  val curPtr = ref 0

  fun lock p = L.getCmlLock (A.unsafeSub (locks, p)) (pN)
  fun unlock p = L.releaseCmlLock (A.unsafeSub (locks, p)) (pN ())

  fun finishWork () =
    fetchAndAdd (numThreadsLive, ~1) = 1

  fun addWorkTo p ws prio =
      (let
        val () = T.atomicBegin ()
        val () = lock p
        val (lqp, lqs) = A.unsafeSub (mainQ, p)
        val lq = case prio of
                    R.PRI => lqp
                  | R.SEC => lqs
        (* val _ = if (p < numComputeProcessors) then
                 A.update (threadCount, p,  A.sub (threadCount, p)+1)
                else () *)
      in
        Q.enque(lq, ws);
        unlock p;
        ParallelInternal.wakeUp (p, 1);
        T.atomicEnd()
      end)

  fun addWork p ws forceSame prio =
    if forceSame then
      addWorkTo p ws prio
      (* If not spawn, Add it to the current processor *)
   else (* spawn *)
      (let
        val _ = fetchAndAdd (numThreadsLive, 1)
        val _ = T.atomicBegin ()
        val r = fetchAndAdd (curPtr,1)
        val target = r mod numComputeProcessors
        val () = lock (target)
        val (lqp, lqs) = A.unsafeSub (mainQ, target)
        val _ = case prio of
                     R.PRI => Q.enque (lqp, ws)
                   | R.SEC => Q.enque (lqs, ws)
        val _ = ParallelInternal.compareAndSwap (curPtr, r+1, (r+1) mod numComputeProcessors)
      in
        unlock target;
        ParallelInternal.wakeUp (target, 1);
        T.atomicEnd ()
      end)

  fun getWork p prio =
    let
      val _ = T.atomicBegin ()
      val () = lock p
      val (lqp, lqs) = A.unsafeSub (mainQ, p)
      val lq = case prio of
                    R.PRI => lqp
                  | R.SEC => lqs
      val res = case Q.deque(lq) of
                     SOME w => (let
                                  (* val _ = if (p < numComputeProcessors) then
                                          A.update (threadCount, p,  A.sub (threadCount, p) - 1)
                                          else () *)
                                  val _ = unlock p
                                in
                                  SOME w
                                end)
                    | NONE => (unlock p;
                          NONE)
      val _ = T.atomicEnd ()
    in
      res
    end

  (* XXX KC should it call mayGC?? *)
  fun empty p =
  let
    val (x,y) = (A.unsafeSub (mainQ, p))
  in
    (Q.empty x) andalso (Q.empty y)
  end

  fun clearWork () =
    ( (* Array.modify (fn r => 0) threadCount
    ; *) Array.app (fn (x,y) => (Q.reset x;Q.reset y))  mainQ)

  val policyName = "cml"

end
