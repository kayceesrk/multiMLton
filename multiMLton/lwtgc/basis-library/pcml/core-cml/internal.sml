structure ParallelInternal =
struct

  val numberOfProcessors = Int32.toInt ((_import "Parallel_numberOfProcessors": unit -> Int32.int;) ())
  val processorNumber = _import "Parallel_processorNumber": unit -> Int32.int;
  val numIOThreads = Int32.toInt ((_import "Parallel_numIOThreads": unit -> Int32.int;) ())
  val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;
  val compareAndSwap = _import "Parallel_compareAndSwap": int ref * int * int -> bool;

end
