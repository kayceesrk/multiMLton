structure Config =
struct

  open Critical

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  fun debug msg = Debug.sayDebug ([atomicMsg], msg)
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))

  val maxIter = 5000
  val isRunning = ref false
  val maxTime : LargeInt.int = 100
  val penalty = 100
  val pauseToken = 5000 (* 1000 ~= 1ms pause *)

  val numLiveThreads = ref 0
  fun decrementNumLiveThreads () =
    (debug' ("Config.decrementNumLiveThreads = "^(Int.toString (!numLiveThreads)))
    ; PacmlFFI.fetchAndAdd (numLiveThreads, ~1) = 1)
  fun incrementNumLiveThreads () = PacmlFFI.fetchAndAdd (numLiveThreads, 1)
end
