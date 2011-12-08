structure Main =
struct
  structure T = MLton.Pacml
  structure L = T.MutexLock

  fun print' s () = ()

  val barrierCount1 = ref 0
  val barrierCount2 = ref 0


  fun get(M, x, y) =
  let
    (*val _ = print("get: x = "^Int.toString(x)^" y = "^Int.toString(y))*)
    val xRow = Array.sub(M, x)
    val element = Array.sub(xRow, y)
    val ret = !element
    (*val ret = 0*)
  in
    ret
  end

  fun update(M, x, y, newVal) =
  let
    val xRow = Array.sub(M, x)
    val element = Array.sub(xRow, y)
    val _ = element := newVal

    (*val _ = print("writing newVal = \n"^Int.toString(newVal)^"on
    * ["^Int.toString(x)^"]["^Int.toString(y)^"\n")*)
  in
    ()
  end


  fun printMatrix (M,size) =
(*  let val i = ref 0
  in
  while !i < size do
  (let val j = ref 0
   in
   (while !j < size do
     (print("["^Int.toString(!i)^"]["^Int.toString(!j)^"] = "^Int.toString(get(M,!i,!j))^"\t"); j := (!j)+1);
     print("\n");
     i := (!i)+1)
   end)
  end*)
    Array.appi (fn (i, row) => (Array.appi (fn (j, elem) =>
    print("["^Int.toString(i)^"]["^Int.toString(j)^"] = "^Int.toString(get(M, i,
    j)))) row; print("\n"))) M

  fun floydWarshall (M, size, n) =
  let
    fun innerLoop(i, j, k) =
      (print'(fn () => "innerloop1: i = "^Int.toString(i)^" j = "^Int.toString(j)
        ^" k = "^Int.toString(k)^"\n");
      if (j < size) then
        let
          val newPath = get(M, i, k) + get(M, k, j)
        in if(newPath < get(M, i, j))
           then (update(M, i, j, newPath);
                innerLoop(i, j+1, k))
           else innerLoop(i, j+1, k)
        end
      else ()
      )

    fun loop(tid, i, j, k) =
      if (k < size)
      then(
      print'(fn () => "Starting loop: tid = "^Int.toString(tid)^" i = "^Int.toString(i)^" j = "^Int.toString(j)
        ^" k = "^Int.toString(k)^"\n");
      if(i*n < (tid+1)*size)
      then (innerLoop(i, j, k);
           loop(tid, i+1, j, k))
      else(
          (* barrier *)
          let
            val _ = L.fetchAndAdd(barrierCount1, k)
            fun spin count =
              if (2*(!count) < n*k*(k+1))
              then (T.yield(); spin count) else ()
            val _ = spin barrierCount1
          in
            (
             loop (tid, floor(Real.fromInt(tid*size)/Real.fromInt(n)), 0, k+1)
            )
          end
          )
     )
     else ()

    fun spawnThreads count =
      if (count = n) then print("spawnThread: "^Int.toString(count)^" threads spawned\n")
      else (print("Thread Spawned !!\n");
            T.spawnHost (fn () => (loop(count, floor(Real.fromInt(count*size)/Real.fromInt(n)), 0, 0); ()));
            spawnThreads(count+1))
  in
    spawnThreads 0
  end

  fun initMatrix size =
  let
    val _ = print("initMatrix: size = "^Int.toString(size)^"\n")
    val M = Array.tabulate(size,
              fn i => Array.tabulate(size, fn j => if (i = j)
                                                   then (ref 0)
                                                   else
                                                     if(i = 0)
                                                     then (ref 1)
                                                     else
                                                       if(j = size-1)
                                                       then
                                                         (ref 1)
                                                       else
                                                         if(i = size-1 andalso j = 0)
                                                         then
                                                           (ref 1)
                                                         else
                                                           (ref 1000)))
    (*val _ = printMatrix(M,size)*)
  in
    M
  end
  fun doit (size,n,M) =
  let
    val x = 0
  in
    T.run
    (fn () =>
      (print("Floyd-Warshall starts\n");
      floydWarshall(M, size, n);
      print("Floyd-Warshall ends\n")
      (*printMatrix(M,size)*)
      )
    )
  end
end

(*1st argument -> size of the matrix*)
(*2nd argument -> number of threads*)
val (size, n) =
   case CommandLine.arguments () of
      [] => (10,1)
    | s::xs => (case Int.fromString s of
                  NONE => (10,1)
                | SOME n => (n,case xs of
                                 [] => 1
                               | s'::xs' =>
                                 (case Int.fromString s' of
                                   NONE => 1
                                 | SOME n' => n')
                                ))

val M = Main.initMatrix size
val ts = Time.now ()
val _ = Main.doit (size,n,M)
val te = Time.now ()
(*val _ = Main.printMatrix(M,size)*)
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
