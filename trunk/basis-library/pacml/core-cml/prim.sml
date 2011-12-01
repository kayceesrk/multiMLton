structure PacmlPrim =
struct

  datatype rdy_thread = datatype RepTypes.rdy_thread
  datatype thread_type = datatype RepTypes.thread_type

  fun move (x, forceStackLifting, skipFixingForwaringPointers) =
    Primitive.Lwtgc.move (x, forceStackLifting, skipFixingForwaringPointers)

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

    (* XXX dummy *)
    fun enque (t: rdy_thread, proc: int, prio: int) =
      Primitive.dontInline (fn () => PrimSQ.enque (t, proc, prio))

    fun deque (prio: int) : rdy_thread option =
      Primitive.dontInline (fn () => PrimSQ.deque (prio))

  end

  val unsafeAssign = Primitive.Ref.unsafeAssign


end
