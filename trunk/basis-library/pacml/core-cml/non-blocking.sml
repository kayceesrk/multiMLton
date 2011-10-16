structure NonBlocking =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Channel
  open Critical

  fun debug msg = Debug.sayDebug ([atomicMsg, ThreadID.tidMsg], msg)
  fun debug' msg = debug (fn () => (msg())^" : " ^Int.toString(PacmlFFI.processorNumber()))

  type proc = ((unit -> exn) * exn chan option) chan

  val inputChan : ((unit -> exn) * exn chan option) chan = channel ()

  val compareAndSwap = PacmlFFI.compareAndSwap
  val fetchAndAdd = PacmlFFI.fetchAndAdd

  val numIOProcessors = PacmlFFI.numIOProcessors
  val numberOfProcessors = PacmlFFI.numberOfProcessors
  val processorNumber = PacmlFFI.processorNumber
  val numComputeProcessors = PacmlFFI.numComputeProcessors

  (* counter for round-robin spawning *)
  val curIOProc = ref 0

  (* counter for dedicated resource *)
  val numDedicated = ref 0
  val dedicatedChannels = Array.tabulate (numIOProcessors, fn _ => channel ())

  fun main () =
  let
    val pn = processorNumber ()
    val myDedicatedChan = Array.unsafeSub (dedicatedChannels, pn - numComputeProcessors)
    val myDedStr = Int.toString (pn - numComputeProcessors)
    fun loop () =
    let
      val iChan = if pn < (numComputeProcessors + !numDedicated)
                  then myDedicatedChan
                  else inputChan
      val (f, outputChan) = recv (iChan)
      val res = f () handle x => (debug' (fn () => "got exception");x)
      val _ = case outputChan of
                   SOME c => send (c, res)
                 | NONE => ()
    in
      loop ()
    end
  in
    loop ()
  end

  fun mkNBThread () =
    if numIOProcessors = 0 then
        raise Fail "mkNBThread: No IO threads. Specify io-threads @MLton runtime argument"
    else
      let
        val r = fetchAndAdd(curIOProc, 1)
        (* Get the processor we would spawn. This value is always between [numComputeProcessors, numberOfProcessors) *)
        val p = (r mod numIOProcessors) + numComputeProcessors
        (* attempt to increment counter. If we fail, someone else will fix it *)
        val _ = compareAndSwap(curIOProc, r+1, (r+1) mod numIOProcessors)
        val _ = debug' (fn () => "Spawning NB Thread on "^(Int.toString(p)))
      in
        ignore (Thread.spawnOnProc (main, p))
      end


  datatype exec_type = RESULT | SPAWN

  fun executionHelper ch f et =
  let
    val _ = debug' (fn () => "executionHelper")
    val _ = if numIOProcessors = 0 then raise Fail "NonBlocking.execute : no io-threads" else ()
    val _ = if sameChannel (ch, inputChan) andalso !numDedicated = numIOProcessors then
              raise Fail "NonBlocking.execute : All io threads have been grabbed by createProcessor ()s"
            else ()

    exception R of 'a

    fun executeAndWrap (foo) =
      let
        val res = foo ()
      in
        R (res)
      end

    val outputChan : exn chan option = case et of
                                              RESULT => SOME (channel ())
                                            | _ => NONE
    val _ = aSend (ch, (fn () => executeAndWrap (f), outputChan))
  in
    case et of
         RESULT => (case recv (valOf outputChan) of
                         R (res) => (debug' (fn () => "executionHelper: got result");
                                     SOME (res))
                       | x => (debug' (fn () => "executionHelper: got exception");
                               raise x))
       | SPAWN => (debug' (fn () => "executionHelper: SPAWN exiting");
                   NONE)
  end

  fun executeOn ch f = valOf (executionHelper ch f RESULT)

  fun execute f = valOf (executionHelper inputChan f RESULT)

  fun spawnOn ch f = ignore (executionHelper ch f SPAWN)

  fun createProcessor () : proc option =
  let
    val _ = debug' (fn () => "createProcessor")
    val _ = if numIOProcessors = 0 then raise Fail "NonBlocking.execute : no io-threads" else ()
    val myChannelIdx = fetchAndAdd (numDedicated, 1)
  in
    if myChannelIdx < numIOProcessors then
      (ignore (List.tabulate (5, fn _ => Thread.spawnOnProc (main, numComputeProcessors + myChannelIdx)));
      debug' (fn () => "createProcessor: created new processor");
      SOME (Array.unsafeSub (dedicatedChannels, myChannelIdx)))
    else
      (debug' (fn () => "createProcessor: could not create new processor");
      NONE)
  end

end
