(* From the SML/NJ benchmark suite. *)

signature BMARK =
  sig
    val doit : int -> unit
    val testit : TextIO.outstream -> unit
  end;
(* mandelbrot.sml *)

structure Main : BMARK =
  struct
    val x_base = 1.5
    val y_base = 1.0
    val side = 2.0

    val sz = 2048
    val maxCount = 50

    val delta = side / (real sz)

    val sum_iterations = ref 0

    fun loop1 i = if (i >= sz)
          then ()
          else let
            val c_im : real = (delta * real i) - y_base
            fun loop2 j = if (j >= sz)
                  then ()
                  else let
                    val c_re = (delta * real j) - x_base
                    fun loop3 (count, z_re : real, z_im : real) = if (count < maxCount)
                          then let
                            val z_re_sq = z_re * z_re
                            val z_im_sq = z_im * z_im
                            in
                              if ((z_re_sq + z_im_sq) > 4.0)
                                then count
                                else let
                                  val z_re_im = (z_re * z_im)
                                  in
                                    loop3 (count+1,
                                      (z_re_sq - z_im_sq) + c_re,
                                       z_re_im + z_re_im + c_im)
                                  end
                            end (* loop3 *)
                          else count
                    val count = loop3 (0, c_re, c_im)
                    in
                      sum_iterations := !sum_iterations + count;
                      loop2 (j+1)
                    end
            in
              loop2 0;
              loop1 (i+1)
            end

    fun doit () = (sum_iterations := 0; loop1 0)

    val doit =
       fn size =>
       let
          fun loop n =
             if n = 0
                then ()
             else (doit();
                   loop(n-1))
       in loop size
       end
    fun testit outstrm = (
          sum_iterations := 0;
          loop1 0;
          TextIO.output (outstrm, Int.toString(!sum_iterations) ^ " iterations\n"))

  end (* Mandelbrot *)

val n =
   case CommandLine.arguments () of
      [] => 1
    | s::_ => (case Int.fromString s of
                  NONE => 1
                | SOME n => n)

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
