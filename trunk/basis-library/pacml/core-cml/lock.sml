structure Lock :> LOCK =
struct

  open Critical

    val pN = PacmlFFI.processorNumber
    val cas = PacmlFFI.compareAndSwap

    type cmlLock = (int ref * int ref)

    fun initCmlLock () =
      let
        val l = (ref ~1, ref 0)
      in
        l
      end

    fun maybePreempt () = if not (MLtonThread.amSwitching (pN ())) then
                            (let
                              val atomicState = getAtomicState ()
                              (* The following 2 step process is required to
                               * trigger the signal checks implemented in
                               * atomicEnd.
                               *)
                              val () = setAtomicState (1)
                              val () = atomicEnd ()
                            in
                              setAtomicState (atomicState)
                            end)
                          else ()

    fun getCmlLock (l, count) ftid =
    let
      val tid = ftid () (* Has to be this way to account for parasite reification at maybePreempt *)
    in
      if !l = tid then
        count := !count +1
      else
        (* Don't bang on CAS. spin on conditional *)
        (if !l < 0 then
          (if cas (l, ~1, tid) then
            ()
          else
            (maybePreempt ();
             PacmlFFI.maybeWaitForGC ();
             getCmlLock (l, count) ftid))
        else
          (maybePreempt ();
           PacmlFFI.maybeWaitForGC ();
           getCmlLock (l, count) ftid))
    end

    fun releaseCmlLock (l,count) tid =
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
