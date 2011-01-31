structure PacmlPrim =
struct
  type runnable_host = RepTypes.runnable_host

  fun move (x, forceStackLifting, skipFixingForwaringPointers) =
    Primitive.MLton.move (x, forceStackLifting, skipFixingForwaringPointers)
  fun initRefUpdate f = Primitive.Ref.preemptFn := f
  fun addToPreemptOnWBA (t : runnable_host) =
    Primitive.Ref.addToPreemptOnWBA (t)

  fun addToSpawnOnWBA (t : runnable_host, proc) =
    Primitive.Ref.addToSpawnOnWBA (t, proc)

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
