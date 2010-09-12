structure Config =
struct
  val maxIter = 5000
  val isRunning = ref false

  val numLiveThreads = ref 0
  fun decrementNumLiveThreads () = PacmlFFI.fetchAndAdd (numLiveThreads, ~1) = 1
  fun incrementNumLiveThreads () = PacmlFFI.fetchAndAdd (numLiveThreads, 1)
end
