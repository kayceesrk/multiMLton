(* deferred.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * A flag to tell us if CML is running.  This gets set and cleared in the
 * RunCMLFn functor, but other modules need to test it.
 *)

structure Deferred : DEFERRED =
  struct
    structure Q = ImpQueue
    structure A = Array

    val pN = ParallelInternal.processorNumber
    val numberOfProcessors = ParallelInternal.numberOfProcessors

    (* XXX KC - Must be ref to properly walk it in the GC. MLton optimizes
     * the queues to unit on MLton.size/MLton.walk otherwise *)
    val spawnQs = A.tabulate(numberOfProcessors, fn _ => ref (Q.new ()))

    fun deferSpawn f =
      let
        val q = A.unsafeSub (spawnQs, pN ())
        val _ = print (Int.toString (C_Size.toInt (Primitive.MLton.size (q))))
        val _ = Primitive.MLton.walk (q)
      in
        Q.enque(!q, f)
      end

    fun getSpawn () =
      let
        val q = A.unsafeSub (spawnQs, pN ())
      in
        Q.deque (!q)
      end

  end
