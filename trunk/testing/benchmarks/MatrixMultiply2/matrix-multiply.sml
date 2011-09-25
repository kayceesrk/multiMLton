(* Written by Stephen Weeks (sweeks@sweeks.com). *)
structure Array = Array2

open MLton.Pacml

fun 'a fold (n : int, b : 'a, f : int * 'a -> 'a) =
   let
      fun loop (i : int, b : 'a) : 'a =
         if i = n
            then b
         else loop (i + 1, f (i, b))
   in loop (0, b)
   end

fun foreach (n : int, f : int -> unit) : unit =
   fold (n, (), f o #1)

fun mult (a1 : int Array.array, a2 : int Array.array, numThreads) : int Array.array =
   let
      val r1 = Array.nRows a1
      val c1 = Array.nCols a1
      val r2 = Array.nRows a2
      val c2 = Array.nCols a2
   in if c1 <> r2
         then raise Fail "mult"
      else
         let val a = Array2.array (r1, c2, 0)
            fun dot (r, c) =
               fold (c1, 0, fn (i, sum) =>
                    sum + Array.sub (a1, r, i) * Array.sub (a2, i, c))
            fun thread tid ch =
              (foreach (r1 div numThreads, fn r =>
                    foreach (c2, fn c =>
                            Array.update (a, r + tid, c, dot (r + tid, c))));
               send (ch, ()))
            val chList = List.tabulate (numThreads, fn tid =>
                                                        let
                                                          val ch = channel ()
                                                          val _ = spawn (fn () => thread tid ch)
                                                        in
                                                          ch
                                                        end)
           val _ = List.app (fn ch => recv ch) chList
         in
            a
         end
   end

structure Main =
   struct
      fun doit (dim, numThreads) =
         let
           val _ = if (dim mod numThreads <> 0) then
                      raise Fail "dim not completely divisible by numThreads"
                   else
                     ()
           val a = Array.tabulate Array.RowMajor (dim, dim, fn (r, c) => (r + c))
         in
           ignore (mult (a, a, numThreads))
         end
   end

 val (n, numThreads) =
  case CommandLine.arguments () of
     s1::s2::_ => (case (Int.fromString s1,
                         Int.fromString s2) of
                        (SOME n1, SOME n2) => (n1, n2)
                      | _ => (1024, 16))
   | _ => (1024, 16)

val ts = Time.now ()
val _ = TextIO.print (concat ["Starting main\n"])
val _ = TextIO.print (concat ["dataSize = ", Int.toString n, "\n"])
val _ = TextIO.print (concat ["numThreads = ", Int.toString numThreads, "\n"])
val _ = MLton.Pacml.run (fn () => Main.doit (n, numThreads))
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
