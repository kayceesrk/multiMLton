structure ImpQueue =
   struct
      open MLton.Pacml
      datatype 'a t = T of {front: 'a list ref, back: 'a list ref, lock: MutexLock.mutexlock}

      fun deque (T {front, back, lock}) =
        let
          val () = MutexLock.getLock (lock)
          val res =
            (case !front of
                [] => (case !back of
                          [] => NONE
                        | l => let val l = List.rev l
                                in case l of
                                      [] => raise Fail "ImpQueue.deque:impossible"
                                    | x :: front' =>
                                        (front := front'
                                          ; back := []
                                          ; SOME x)
                                end)
              | x::front' => (front := front'; SOME x)) handle ex => (print "ImpQueue.deque"; raise ex)
          val () = MutexLock.releaseLock (lock)
        in
          res
        end

      fun empty (T {front, back, lock}) =
      let
        val () = MutexLock.getLock (lock)
        val res =
         (case !front of
               [] => (case !back of
                         [] => true
                       | _ => false)
             | _ => false)
        val () = MutexLock.releaseLock (lock)
      in
        res
      end

      fun enque (T {back, lock, ...}, x) =
         (MutexLock.getLock (lock)
          ; back := x::(!back)
          ; MutexLock.releaseLock (lock)) handle ex => (print "ImpQueue.enque"; raise ex)

      fun new () = T {front = ref [], back = ref [], lock = MutexLock.initLock ()}

      fun peek (T {front, back, lock}) =
      let
        val () = MutexLock.getLock (lock)
        val res =
         (case !front of
               [] => (case !back of
                         [] => NONE
                       | l => let val l = List.rev l
                              in case l of
                                    [] => raise Fail "ImpQueue.peek:impossible"
                                  | x::front' =>
                                       (front := x::front'
                                        ; back := []
                                        ; SOME x)
                              end)
             | x::_ => SOME x) handle ex => (print "ImpQueue.peek"; raise ex)
        val () = MutexLock.releaseLock (lock)
      in
        res
      end

      fun reset (T {front, back, lock}) =
         (MutexLock.getLock lock;
          front := [];
          back := [];
          MutexLock.releaseLock lock)

   end

structure Main =
struct
  open MLton.Pacml

  datatype work = WORK | DONE

  fun worker workSize iters =
  let
    fun tightLoop2 n =
      if n=0 then ()
      else tightLoop2 (n-1)

    fun tightLoop n =
      if n=0 then ()
      else (tightLoop2 300; tightLoop (n-1))

    (* tightloop (1000) ~= 1 ms *)
  in
    if iters = 0 then ()
    else (tightLoop (workSize);
          worker workSize (iters-1))
  end

  fun doit (numThreads, numTasks, workSize) =
    (MLton.Pacml.run
    (fn () =>
      let
        val numQueues = 1 + (numThreads mod 100)
        val queues = Array.tabulate (numQueues, fn _ => ImpQueue.new ())
        val iters = Int.quot (numTasks, numThreads)
        val _ = List.tabulate (numThreads, fn _ => spawn (fn () => worker workSize iters))
      in
        ()
      end))
end

val (n1, n2, n3) =
   case CommandLine.arguments () of
      [] => raise Fail "Need 3 arguments"
    | s1::s2::s3::_ => (case (Int.fromString s1,
                              Int.fromString s2,
                              Int.fromString s3) of
                         (SOME n1, SOME n2, SOME n3) => (n1, n2, n3)
                       | (_,_,_) => raise Fail "Need 3 arguments")

val ts = Time.now ()
val _ = print (concat ["Starting synthetic numThreads=", Int.toString n1,
                       " numTasks=", Int.toString n2,
                       " workSize=", Int.toString n3, "\n"])
val _ = Main.doit (n1, n2, n3)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
