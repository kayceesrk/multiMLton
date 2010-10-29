structure NonBlocking =
struct

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  open Channel
  open Critical

  fun debug msg = Debug.sayDebug ([atomicMsg, ThreadID.tidMsg], msg)
  fun debug' msg = print msg (*debug (fn () => msg^" : "
                                ^Int.toString(PacmlFFI.processorNumber()))*)




  type proc = ((unit -> exn) * exn chan) chan

  val inputChan : ((unit -> exn) * exn chan) chan = channel ()

  val compareAndSwap = PacmlFFI.compareAndSwap
  val fetchAndAdd = PacmlFFI.fetchAndAdd

  val numIOProcessors = PacmlFFI.numIOProcessors
  val numberOfProcessors = PacmlFFI.numberOfProcessors
  val processorNumber = PacmlFFI.processorNumber
  val numComputeThreads = numberOfProcessors - numIOProcessors

  (* counter for round-robin spawning *)
  val curIOProc = ref 0

  (* counter for dedicated resource *)
  val numDedicated = ref 0
  val dedicatedChannels = Array.tabulate (numIOProcessors, fn _ => channel ())

  fun main () =
  let
    val _ = debug' ("Executing Main\n")
    val pn = processorNumber ()
    val _ = debug' ("I think I am on proc num: " ^ Int.toString(pn) ^ "requesting channel num: " ^ Int.toString(pn - numComputeThreads) ^ "\n")
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
      val res = f () handle x => (print "got exception";x)
      val _ = send (outputChan, res)
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
        (* Get the processor we would spawn. This value is always between [numComputeThreads, numberOfProcessors) *)
        val p = (r mod numIOProcessors) + numComputeThreads
        (* attempt to increment counter. If we fail, someone else will fix it *)
        val _ = compareAndSwap(curIOProc, r+1, (r+1) mod numIOProcessors)
        val _ = debug' ("Spawning NB Thread on "^(Int.toString(p)))
      in
        ignore (Thread.spawnOnProc (main, p))
      end


  fun executeOn ch f = let
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
    val outputChan : exn chan = channel ()
    val _ = print "NB send"
    val _ = send (ch, (fn () => executeAndWrap (f), outputChan))
    val _ = print "NB post send"
    val r = recv (outputChan)
    val _ = print "NB post recv"
  in
    case r of
         R (res) => res
       | x => raise x
  end 

  fun execute f = executeOn inputChan f

  fun createProcessor () : proc option =
  let
    val _ = if numIOProcessors = 0 then raise Fail "NonBlocking.execute : no io-threads" else ()
    val myChannelIdx = fetchAndAdd (numDedicated, 1)
  in
    if myChannelIdx < numIOProcessors then
      (ignore (List.tabulate (5, fn _ => Thread.spawnOnProc (main, numComputeThreads + myChannelIdx)));
      SOME (Array.sub (dedicatedChannels, myChannelIdx)))
    else
      NONE
  end

end
