structure PacmlPrim =
struct

  type rdy_thread = RepTypes.rdy_thread
  datatype thread_type = datatype RepTypes.thread_type

  fun move (x, forceStackLifting, skipFixingForwaringPointers) =
    Primitive.MLton.move (x, forceStackLifting, skipFixingForwaringPointers)
  fun initRefUpdate f = Primitive.Ref.preemptFn := f

  fun addToPreemptOnWBA (t : rdy_thread, threadType) =
    case threadType of
         HOST =>Primitive.Lwtgc.addToPreemptOnWBA (t, 0)
       | PARASITE => Primitive.Lwtgc.addToPreemptOnWBA (t, 1)

  fun addToSpawnOnWBA (t : rdy_thread, proc) =
    Primitive.Lwtgc.addToSpawnOnWBA (t, proc)

  structure SchedulerQueue =
  struct
    structure PrimSQ = Primitive.MLton.SchedulerQueue
    open PrimSQ

    fun enque (t: rdy_thread, proc: int, prio: int) =
      PrimSQ.enque (t, proc, prio)
    fun deque (prio: int) : rdy_thread option =
      PrimSQ.deque (prio)

  end

  val unsafeAssign = Primitive.Ref.unsafeAssign

end
