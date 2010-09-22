(* Parallel mandelbrot
 * -------------------
 * The workload consists of 32768 rows. There are <numSlaves> slaves and 1 master. The
 * work is split into chunks, each of size <worksize> rows. Master allocates the work to
 * each slave and when slave completes, it is allocated more work if exists.
 *
 * Note: Here, Slaves do not transfer the results to the master.
 *)


structure Main =
  struct
    open MLton.PCML
    fun print s = ()

    val x_base = ~2.0
    val y_base = 1.25
    val side = 2.5

    val sz = 32768
    val worksize = 64
    val numSlaves = 8

    val maxCount = 2048
    val sum_iterations = ref 0

    val delta = side / (real sz)


    fun slave () =
    let
      (* Gets input from master *)
      val inCh = channel()

      (* returns input channel when task completes *)
      (*TODO: Needs to be modified *)
      val outCh = channel()

      (* Worker thread *)
      (* init is the starting row. If no work is left, master sends an init of
       * -1*)
      fun work init = if init=(~1) then ()
                      else
      let
        val _ = print("\nStarting work : "^Int.toString(init))
        fun loop i =
          if (i >= init + worksize)
          then (print("\nFinished : "^Int.toString(i-worksize));
                send(outCh, inCh))
          else
            let
                val c_im : real = y_base - (delta * real i)
                fun loop2 j = if (j >= sz) then ()
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
                      sum_iterations := !sum_iterations + count;
                      loop2 (j+1)
                    end
            in
              loop2 0;
              loop (i+1)
           end

        (* starting work*)
        val _ = loop init
      in
        work (recv inCh)
      end
    in
      spawn(fn()=>work (recv inCh));
      (inCh, outCh)
    end

    fun master () =
    let
      val prog = ref 0
      val _ = prog := !prog - worksize

      (* Create a list of 8 slaves *)
      val slavesList = List.tabulate(numSlaves, fn(x)=>slave())
      (* assign work to each slave*)
      val _=
        (List.map (fn(x)=>(prog := !prog+worksize;
                        (case x of
                             (inCh, outCh) => send(inCh, !prog)))) slavesList)

      (* Number of slaves that received term signal *)
      val countTerm = ref 0

      (* When slave completes work assign more work if any *)
      fun assign x=
        let
          val outCh = case x of (inCh, outCh) => outCh
        in
            wrap(recvEvt outCh, fn(ch) =>
            (if (!prog) >= sz
             then (prog := !prog + worksize; send(ch, ~1); countTerm := !countTerm + 1)
             else (prog := !prog + worksize;send(ch,!prog))))
        end

      fun work ()=
        if !prog < sz then
          (* Select any of the completed slaves *)
            (select (List.map assign slavesList); work())
        else
           if !countTerm = numSlaves then ()
           else
              (select (List.map assign slavesList);
               work ())
    in
      work ()
    end

    fun doit n =
      (sum_iterations := 0;
      MLton.RunPCML.doit(master, NONE);
      print(Int.toString(!sum_iterations)^"\n"))
      (* n = ANY *)
end (* Mandelbrot *)
