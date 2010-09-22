structure Lock :> LOCK =
struct

    val pN = ParallelInternal.processorNumber

    val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;
    val cas = _import "Parallel_compareAndSwap": int ref * int * int -> bool;
    val mayBeWaitForGC = _import "Parallel_maybeWaitForGC": unit -> unit;

    type lock = int ref
    type cmlLock = (int ref * int ref)

    fun initLock () =
      let
        val l = ref ~1
      in
        l
      end

    (* Dont bang on cas. spin on conditional *)
    fun getLock l =
      (if !l < 0 then
        (if cas (l, ~1, pN()) then
          ()
        else
          (mayBeWaitForGC ();
          getLock l))
      else
          (mayBeWaitForGC ();
          getLock l))

    fun releaseLock l =
        if cas (l, pN(), ~1) then
          ()
        else
          let
            val holder = Int.toString(!l)
            val attempt = Int.toString(pN())
            val msg = ("Parallel.Lock : Can't unlock if you dont hold the lock"
                      ^"\nCurrently held by "^holder
                      ^"\nFailed attempt by "^attempt)
          in
            raise Fail msg
          end

    fun initCmlLock () =
      let
        val l = (ref ~1, ref 0)
      in
        l
      end

    fun getCmlLock (l, count) tid =
    let
      val _ =
      mayBeWaitForGC ()
    in
      if !l = tid then
        count := !count +1
      else
        (* Don't bang on CAS. spin on conditional *)
        (if !l < 0 then
          (if cas (l, ~1, tid) then
            ()
          else
            getCmlLock (l, count) tid)
        else
          getCmlLock (l, count) tid)
    end

    fun releaseCmlLock (l,count) tid =
      if !l = tid andalso !count > 0 then
          count := !count - 1
      else
        if cas (l, tid, ~1) then
          (mayBeWaitForGC ())
        else
          let
            val holder = Int.toString(!l)
            val attempt = Int.toString(tid)
            val msg = ("Parallel.Lock : Can't unlock if you dont hold the lock"
                      ^"\nCurrently held by "^holder
                      ^"\nFailed attempt by "^attempt)
          in
            raise Fail msg
          end

end
