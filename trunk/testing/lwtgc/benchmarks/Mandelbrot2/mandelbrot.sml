(* From the SML/NJ benchmark suite. *)

signature BMARK =
  sig
    val doit : int * int -> unit
  end;
(* mandelbrot.sml *)

structure Main : BMARK =
  struct
    val maxCount = 2048

    val spawn = MLton.Pacml.spawn

    val sum_iterations = ref 0

    fun loop1 (i: int) (x_sz: real) (y_sz: real) (x_base: real) (y_base: real) x_side y_side = if (real i >= y_sz)
          then ()
          else let
            val x_delta = x_side / x_sz
            val y_delta = y_side / y_sz
            val c_im : real = y_base - (y_delta * real i)
            fun loop2 (j: int) = if (real j >= x_sz)
                  then ()
                  else let
                    (* val _ = print (concat ["i: ", Int.toString i, " j: ", Int.toString j, "\n"]) *)
                    val c_re = x_base + (x_delta * real j)
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
              loop1 (i+1) x_sz y_sz x_base y_base x_side y_side
            end

    fun doit (sz, numThreads: int) =
    let
      val globalX_base = ~2.0
      val globalY_base = 1.25
      val side = 2.5

      val x_sz = (real sz)/(real numThreads)
      val y_sz = real sz

      val x_side = side / (real numThreads)
      val y_side = side

      fun loop threadId =
        if (threadId = numThreads) then ()
        else
          let
            val x_base = globalX_base + (x_side * (real threadId))
            val y_base = globalY_base
          in
            print "Spawning thread\n";
            print "--------------\n";
            print (concat ["x_sz ", Real.toString x_sz,
                           " y_sz ", Real.toString y_sz,
                           " x_base ", Real.toString x_base,
                           " y_base ", Real.toString y_base,
                           " x_side ", Real.toString x_side,
                           " y_side ", Real.toString y_side, "\n"]);
            spawn (fn () => loop1 0 x_sz y_sz x_base y_base x_side y_side);
            loop (threadId + 1)
          end

    in
      loop 0
    end

  end (* Mandelbrot *)

val (n, numThreads) =
  case CommandLine.arguments () of
     s1::s2::_ => (case (Int.fromString s1,
                         Int.fromString s2) of
                        (SOME n1, SOME n2) => (n1, n2)
                      | _ => (32768, 16))
   | _ => (32768, 16)

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
