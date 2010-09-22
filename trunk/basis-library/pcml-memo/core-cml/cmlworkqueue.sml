functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) : WORKQUEUE =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  fun debug msg = Debug.sayDebug ([], (msg))
  fun debug' msg = debug (fn () => msg)

  type proc = int
  type work = W.work

  val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;
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

  val numberOfProcessors = W.numberOfProcessors ()
  val pN = ParallelInternal.processorNumber

  structure Q = ImpQueue

  (* private state *)
  val mainQ = A.tabulate (numberOfProcessors, fn _ => Q.new ())
  val locks = A.tabulate (numberOfProcessors, fn _ => L.initCmlLock ())
  (* For spawning in a round robin fashion *)
  val curPtr = ref 0

  fun lock p = L.getCmlLock (A.unsafeSub (locks, p)) (pN ())
  fun unlock p = L.releaseCmlLock (A.unsafeSub (locks, p)) (pN ())

  fun finishWork () =
    fetchAndAdd (numThreadsLive, ~1)=1

  fun addWorkTo p ws =
      (let
        val () = T.atomicBegin ()
        val () = lock p
        val lq = A.unsafeSub (mainQ, p)
      in
        Q.enque(lq, ws);
        unlock p;
        T.atomicEnd()
      end)

  fun addWork p ws forceSame =
    if forceSame then
      addWorkTo p ws
      (* If not spawn, Add it to the current processor *)
   else
      (let
        val _ = fetchAndAdd (numThreadsLive, 1)
        val _ = T.atomicBegin ()
        val target = !curPtr
        val () = lock (target)
        val lq = A.unsafeSub (mainQ, target)
        val _ = Q.enque(lq, ws)
        val _ = curPtr := (target + 1) mod numberOfProcessors
      in
        unlock target;
        T.atomicEnd ()
      end)

  fun getWork p =
    let
      val _ = T.atomicBegin ()
      val () = lock p
      val lq = A.unsafeSub (mainQ, p)
      val res = case Q.deque(lq) of
                  SOME w => ( SOME w
                            before (unlock p))
                | NONE => (unlock p;
                          NONE)
      val _ = T.atomicEnd ()
    in
      res
    end

  (* XXX KC should it call mayGC *)
  fun empty p = Q.empty (A.unsafeSub (mainQ, p))

  fun clearWork () =
    (Array.app Q.reset mainQ)

  val policyName = "cml"

end
