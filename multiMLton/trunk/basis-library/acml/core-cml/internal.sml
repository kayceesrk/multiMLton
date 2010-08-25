structure ParallelInternal =
struct

  val numberOfProcessors = Int32.toInt ((_import "Parallel_numberOfProcessors": unit -> Int32.int;) ())
  val processorNumber = _import "Parallel_processorNumber": unit -> Int32.int;
  val numIOThreads = Int32.toInt ((_import "Parallel_numIOThreads": unit -> Int32.int;) ())
  val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;
  val compareAndSwap = _import "Parallel_compareAndSwap": Int32.int ref * Int32.int * Int32.int -> bool;
  val vCompareAndSwap = _import "Parallel_vCompareAndSwap": Int32.int ref * Int32.int * Int32.int -> Int32.int;
  val wait = _import "Parallel_wait": unit -> unit;
  val wakeUp = _import "Parallel_wakeUpThread": Int32.int * Int32.int -> unit;

  val PARASITE_ENABLED = false
  val maxIter = 5000

end
