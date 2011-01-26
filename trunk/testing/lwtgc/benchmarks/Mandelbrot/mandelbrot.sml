
structure Main =
struct
  open MLton.Pacml
  (*val print = TextIO.print*)

  val x_base = ~2.0
  val y_base = 1.25
  val side = 2.5

  val sz = 32768
  val workChunk = 16
  val maxCount = 2048
  fun print s = ()

  val delta = side / (real sz)
  fun work init =
  let
    val _ = print("\nStarting work : "^Int.toString(init))
    fun loop i =
      if (i >= init + workChunk) then (print("\nFinished : "^Int.toString(i-workChunk));0)
      else
        let
          val c_im : real = y_base - (delta * real i)
          fun loop2 j =
            if (j >= sz) then 0
            else
              let
                val c_re = x_base * (delta + real j)
                fun loop3 (count, z_re : real, z_im : real) =
                  if (count < maxCount)
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
                                (*sum_iterations := !sum_iterations + count;*)
                                (count + loop2 (j+1))
              end
        in
          let val res = loop2 0
          in res + loop (i+1) end
          end

          (* starting work*)
    val finCount = loop init
          in
            finCount
  end

  fun slave1 (workChan,resultChan,id) =
  let
    val workChunk = recv workChan
    val _ = print("\n"^Int.toString(id)^": Recv work = "^Int.toString(workChunk))
    val _ = if workChunk > ~1 then
      let
        val result = work workChunk
        val _ = spawn (fn () => ((* print ("\nslave "^Int.toString(id)^" Sending the result "^(Int.toString workChunk)); *)
        send(resultChan, result)))
      in slave1(workChan,resultChan,id) end
            else print("\nReceived Term.Slave "^Int.toString(id)^" exit")
  in () end



  fun master1 totalSlaves =
  let
    val term = ~1
    val workChan = channel()
    val resultChan :int chan= channel ()
    fun splitWork i =
      if i > sz then (ignore(List.tabulate(totalSlaves, fn(x)=> send(workChan,term))))
      else
        let
          val workLoad = i
          val _ = send(workChan,workLoad)
        in splitWork (i+workChunk) end

    fun recvResult(resultChan, i) =
    let
      val res = if i > sz
                then  0
                else
                  let
                    val resultChunk = recv resultChan
                    val _ = print "\nRecv Result "
                    val _ = print (Int.toString (i))
                  in
                    recvResult(resultChan, i+workChunk)
                  end
    in
      res
                  end
    val _ = List.tabulate(totalSlaves, fn(x) => spawn(fn ()=> slave1(workChan,resultChan,x)))
    val _ = splitWork 0
    val _ = recvResult (resultChan, 0)
    val count = 0
    val _ = TextIO.print("The Counts is :"^Int.toString(count)^"\n")
    val _ = MLton.Pacml.shutdown OS.Process.success
  in
    ()
  end

  fun doit n =
    (MLton.Pacml.run
    (fn () =>
    let
      val _ = master1 n
  in () end)(*;
  print(Int.toString(!sum_iterations)^"\n")*))
  (* n = ANY *)
end (* Mandelbrot *)

val n =
  case CommandLine.arguments () of
       [] => 16
     | s::_ => (case Int.fromString s of
                     NONE => 16
                   | SOME n => n)

val ts = Time.now ()
val _ = TextIO.print (concat ["\nStarting main : numSlaves = ", Int.toString n, "\n"])
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
