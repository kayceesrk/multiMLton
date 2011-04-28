structure PacmlPrim =
struct
  datatype runnable_host = datatype RepTypes.runnable_host
  datatype thread_id = datatype RepTypes.thread_id

  fun move (x, forceStackLifting, skipFixingForwaringPointers) =
    Primitive.MLton.move (x, forceStackLifting, skipFixingForwaringPointers)
  fun initRefUpdate f = Primitive.Ref.preemptFn := f
  fun addToPreemptOnWBA (t as RHOST (TID {id, ...}, rt) : runnable_host) =
  let
    val _ = MLtonThread.threadStatus rt
  in
    Primitive.Lwtgc.addToPreemptOnWBA (t)
  end

  fun addToSpawnOnWBA (t as RHOST (TID {id, ...}, rt) : runnable_host, proc) =
  let
    val _ = MLtonThread.threadStatus rt
  in
    Primitive.Lwtgc.addToSpawnOnWBA (t, proc)
  end

  structure SchedulerQueue =
  struct
    structure PrimSQ = Primitive.MLton.SchedulerQueue
    open PrimSQ

    fun enque (t: runnable_host, proc: int, prio: int) =
      PrimSQ.enque (t, proc, prio)
    fun deque (prio: int) : runnable_host option =
      PrimSQ.deque (prio)

  end

end
