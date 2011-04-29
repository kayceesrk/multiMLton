
structure Main =
struct
  open MLton.Pacml

  datatype work = WORK | DONE

  fun worker ch workSize =
  let
    fun tightLoop2 n =
      if n=0 then ()
      else tightLoop2 (n-1)

    fun tightLoop n =
      if n=0 then ()
      else (tightLoop2 300; tightLoop (n-1))

    (* tightloop (1000) ~= 1 ms *)
  in
    case (recv ch) of
         WORK => (tightLoop (workSize);
                  worker ch workSize)
       | DONE => ()
  end

  fun doit (numThreads, numTasks, workSize) =
    (MLton.Pacml.run
    (fn () =>
      let
        val channels = Array.tabulate (numThreads, fn _ => let
                                                             val ch = channel ()
                                                             val _  = spawn (fn () => worker ch workSize)
                                                           in
                                                             ch
                                                           end)
        fun loop i =
          if (i=0) then
            ignore (List.tabulate (numThreads, fn i => send (Array.sub (channels, i), DONE)))
          else
            (send (Array.sub (channels, i mod numThreads), WORK);
            loop (i-1))

      in
        loop numTasks
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
