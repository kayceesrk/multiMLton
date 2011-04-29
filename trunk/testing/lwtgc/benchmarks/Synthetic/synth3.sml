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

  fun worker writes q =
  let
    fun tightLoop2 n =
      if n=0 then ()
      else
        let
          val _ = case (ImpQueue.peek q) of
                       NONE => ()
                     | SOME i => if (i=(~1)) then print "Hello\n" else ()
        in
          tightLoop2 (n-1)
        end

    fun tightLoop n =
      if n=0 then ()
      else (tightLoop2 300;
            ImpQueue.enque (q, writes);
            tightLoop (n-1))

  in
    (tightLoop (writes);
     print "Worker done\n")
  end

  fun doit (numThreads, numWrites) =
    (MLton.Pacml.run
    (fn () =>
      let
        val _ = print "Running\n"
        val numQueues = Int.quot (numThreads, 10) + 1
        val queues = Array.tabulate (numQueues, fn _ => ImpQueue.new ())
        val _ = List.tabulate (numThreads,
                  fn i =>
                    let
                      val q = Array.sub (queues, Int.mod (i, numQueues))
                    in
                      spawn (fn () => worker numWrites q)
                    end)
      in
        ()
      end))
end

val (n1, n2) =
   case CommandLine.arguments () of
      [] => raise Fail "Need 2 arguments"
    | s1::s2::_ => (case (Int.fromString s1,
                          Int.fromString s2) of
                         (SOME n1, SOME n2) => (n1, n2)
                       | (_,_) => raise Fail "Need 2 arguments")

val ts = Time.now ()
val _ = print (concat ["Starting synthetic numThreads=", Int.toString n1,
                       " numTasks=", Int.toString n2, "\n"])
val _ = Main.doit (n1, n2)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
