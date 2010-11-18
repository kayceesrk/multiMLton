structure NonBlocking =
struct

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  fun debug msg = Debug.sayDebug ([Scheduler.atomicMsg, Scheduler.tidMsg], msg)
  fun debug' msg = debug (fn () => msg^" : "
                                ^Int.toString(Basic.processorNumber()))


  open Channel

  type proc = ((unit -> exn) * exn chan) chan

  val inputChan : ((unit -> exn) * exn chan) chan = channel ()

  val compareAndSwap = ParallelInternal.compareAndSwap
  val fetchAndAdd = ParallelInternal.fetchAndAdd

  val numIOThreads = ParallelInternal.numIOThreads
  val numberOfProcessors = ParallelInternal.numberOfProcessors
  val processorNumber = ParallelInternal.processorNumber
  val numComputeThreads = numberOfProcessors - numIOThreads

  (* counter for round-robin spawning *)
  val curIOProc = ref 0

  (* counter for dedicated resource *)
  val numDedicated = ref 0
  val dedicatedChannels = Array.tabulate (numIOThreads, fn _ => channel ())

  fun main () =
  let
    val pn = processorNumber ()
    val myDedicatedChan = Array.sub (dedicatedChannels, pn - numComputeThreads)
    val myDedStr = Int.toString (pn - numComputeThreads)
    fun loop () =
    let
      val iChan = if pn < (numComputeThreads + !numDedicated)
                  then (debug' ("choosing dedicated chan"^myDedStr)
                        ; myDedicatedChan)
                  else (debug' ("choosing global inputChan"^(Int.toString (pn - numComputeThreads)))
                        ; inputChan)
      val (f, outputChan) = recv (iChan)
      val _ = debug' "NB.mainLoop : got task. Executing..."
      val res = f () handle x => x
      val _ = send (outputChan, res)
    in
      loop ()
    end
  in
    loop ()
  end

  fun mkNBThread () =
    if numIOThreads = 0 then
        raise Fail "mkNBThread: No IO threads. Specify io-threads @MLton runtime argument"
    else
      let
        val r = fetchAndAdd(curIOProc, 1)
        (* Get the processor we would spawn. This value is always between [numComputeThreads, numberOfProcessors) *)
        val p = (r mod numIOThreads) + numComputeThreads
        (* attempt to increment counter. If we fail, someone else will fix it *)
        val _ = compareAndSwap(curIOProc, r+1, (r+1) mod numIOThreads)
        val _ = debug' ("Spawning NB Thread on "^(Int.toString(p)))
      in
        ignore (Thread.spawnOnProc (main, p))
      end

  fun executeOn ch f =
  let
    val _ = if numIOThreads = 0 then raise Fail "NonBlocking.execute : no io-threads" else ()
    val _ = if sameChannel (ch, inputChan) andalso !numDedicated = numIOThreads then
                raise Fail "NonBlocking.execute : All io threads have been grabbed by createProcessor ()s"
            else ()
    exception R of 'a
    fun executeAndWrap (foo) =
    let
      val res = foo ()
    in
      R (res)
    end
    val outputChan : exn chan = channel ()
    val _ = send (ch, (fn () => executeAndWrap (f), outputChan))
    val r = recv (outputChan)
  in
    case r of
         R (res) => res
       | x => raise x
  end

  fun execute f = executeOn inputChan f

  fun createProcessor () : proc option =
  let
    val _ = if numIOThreads = 0 then raise Fail "NonBlocking.execute : no io-threads" else ()
    val myChannelIdx = fetchAndAdd (numDedicated, 1)
  in
    if myChannelIdx < numIOThreads then
      (ignore (List.tabulate (5, fn _ => Thread.spawnOnProc (main, numComputeThreads + myChannelIdx)));
      SOME (Array.sub (dedicatedChannels, myChannelIdx)))
    else
      NONE
  end

end
