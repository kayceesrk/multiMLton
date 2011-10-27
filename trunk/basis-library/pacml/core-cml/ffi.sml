structure PacmlFFI =
struct

  val commEvent = _import "GC_commEvent": unit -> unit;
  val compareAndSwap = _import "Parallel_compareAndSwap": Int32.int ref * Int32.int * Int32.int -> bool;
  val disablePreemption = _import "Parallel_disablePreemption": unit -> unit;
  val enablePreemption = _import "Parallel_enablePreemption": unit -> unit;
  val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;
  val ffiPrint = _import "GC_print": Int32.int -> unit;
  val parasiteCreatedEvent = _import "GC_parasiteCreatedEvent": unit -> unit;
  val maybeWaitForGC = _import "Parallel_maybeWaitForGC": unit -> unit;
  val noop = _import "GC_noop": unit -> unit;
  val numberOfProcessors = Int32.toInt ((_import "Parallel_numberOfProcessors": unit -> Int32.int;) ())
  val numIOProcessors = Int32.toInt ((_import "Parallel_numIOThreads": unit -> Int32.int;) ())
  val numComputeProcessors = numberOfProcessors - numIOProcessors
  val processorNumber = _import "Parallel_processorNumber": unit -> Int32.int;
  val vCompareAndSwap = _import "Parallel_vCompareAndSwap": Int32.int ref * Int32.int * Int32.int -> Int32.int;
  val wait = _import "Parallel_wait": unit -> unit;
  val wakeUp = _import "Parallel_wakeUpThread": Int32.int * Int32.int -> unit;
  val summaryWrite = _import "GC_summaryWrite": unit -> unit;

  datatype array_kind = SEND_INTENT | RECV_INTENT
  local
    val manipulateIntentArray = _import "GC_manipulateIntentArray": Primitive.MLton.GCState.t * Int32.t * Int32.t * Int32.t * Int32.t * Int32.t -> Int32.t;

    fun arrayKindToInt SEND_INTENT = 0
      | arrayKindToInt RECV_INTENT = 1

    val gcState = Primitive.MLton.GCState.getCurrentGCState
  in
    fun readIntentArray (ak, coreId) =
      manipulateIntentArray (gcState (), arrayKindToInt ak, 0, coreId, 0, 0)

    fun writeIntentArray (ak, coreId, oldValue, newValue) =
      manipulateIntentArray (gcState (), arrayKindToInt ak, 1, coreId, oldValue, newValue)
  end
end
