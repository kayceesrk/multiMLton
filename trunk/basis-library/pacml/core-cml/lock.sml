structure Lock :> LOCK_EXTRA =
struct

  open Critical

    val pN = PacmlFFI.processorNumber
    val cas = PacmlFFI.compareAndSwap

    type cmlLock = RepTypes.cmlLock

    fun initCmlLock () =
      let
        val l = RepTypes.LOCK (ref ~1, ref 0)
      in
        l
      end

    val yieldForSpin = ref (fn () => ())

    fun maybePreempt () = ((if not (MLtonThread.amSwitching (pN ())) then
                            (let
                              val atomicState = getAtomicState ()
                              (* Unmark this thread id so that if this thread is preempted,
                               * it goes to the secondary scheduler queue *)
                              val () = ThreadID.unmark (ThreadID.getCurThreadId ())
                              (* The following 2 step process is required to
                               * trigger the signal checks implemented in
                               * atomicEnd.
                               *)
                              val () = setAtomicState (1)
                              val () = atomicEnd ()
                              val () = (!yieldForSpin) ()
                              val () = setAtomicState (atomicState)
                            in ()
                            end)
                          else ());
                          PacmlFFI.maybeWaitForGC ())

    fun getCmlLock (lock as RepTypes.LOCK (l, count)) ftid =
    let
      val tid = ftid () (* Has to be this way to account for parasite reification at maybePreempt *)
    in
      if !l = tid then
        count := !count +1
      else
        (* Don't bang on CAS. spin on conditional *)
        (if !l < 0 then
          (if cas (l, ~1, tid) then
            ThreadID.mark (ThreadID.getCurThreadId ())
          else
            (maybePreempt ();
             getCmlLock lock ftid))
        else
          (maybePreempt ();
           getCmlLock lock ftid))
    end

    fun releaseCmlLock (RepTypes.LOCK (l,count)) ftid =
    let
      val tid = ftid ()
    in
      if !l = tid andalso !count > 0 then
          count := !count - 1
      else
        if cas (l, tid, ~1) then
          ()
        else
          let
            val holder = Int.toString(!l)
            val attempt = Int.toString(tid)
            val msg = ("Parallel.Lock : Can't unlock if you dont hold the lock"
                      ^". Currently held by "^holder
                      ^". Failed attempt by "^attempt)
          in
            raise Fail msg
          end
    end
end
