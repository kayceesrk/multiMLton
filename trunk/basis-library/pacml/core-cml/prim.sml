structure PacmlPrim =
struct
  type runnable_host = RepTypes.runnable_host

  fun move (x, force) = Primitive.MLton.move (ref x, force)
  fun initRefUpdate f = Primitive.Ref.preemptFn := f
  fun addToPreemptOnWBA (t : runnable_host) =
    Primitive.Ref.addToPreemptOnWBA (t)

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
