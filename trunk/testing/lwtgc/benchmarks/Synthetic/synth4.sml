structure Main =
struct
  open MLton.Pacml

  datatype work = WORK | DONE

  fun worker workSize =
  let
    fun tightLoop2 n =
      if n=0 then ()
      else tightLoop2 (n-1)

    fun tightLoop n =
      if n=0 then ()
      else (tightLoop2 300; tightLoop (n-1))

    (* tightloop (1000) ~= 1 ms *)
  in
    tightLoop (workSize)
  end

  fun doit (numThreads, workSize) =
    (MLton.Pacml.run
    (fn () =>
      ignore (List.tabulate (numThreads, fn _ => spawn (fn () => worker workSize)))))
end

val (n1, n2) =
   case CommandLine.arguments () of
      [] => raise Fail "Need 3 arguments"
    | s1::s2::_ => (case (Int.fromString s1,
                          Int.fromString s2) of
                         (SOME n1, SOME n2) => (n1, n2)
                       | (_,_) => raise Fail "Need 2 arguments")

val ts = Time.now ()
val _ = print (concat ["Starting synthetic numThreads=", Int.toString n1,
                       " workSize=", Int.toString n2, "\n"])
val _ = Main.doit (n1, n2)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])

