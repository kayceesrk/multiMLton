structure Main =
struct
  structure T = MLton.Pacml
  structure R = MLton.Rcce

  val send = R.send
  val recv = R.recv

  fun ping count msgSize you =
  let
    val v = Vector.tabulate (msgSize, fn _ => 0)
    val _ = print ("MsgSize = "^(Int.toString (MLton.size (v)))^"\n")
    fun loop n =
        if n=0 then ()
        else
        (send (v, you);
         loop (n-1))
  in
    loop count
  end

  fun pong count msgSize you =
  let
    fun loop n =
      if n=0 then ()
      else
        (ignore (recv you);
         loop (n-1))
  in
    loop count
  end

  fun doit (count, msgSize) =
    T.run
    (fn () =>
    let
      val _ = T.spawnOnProc (fn () => pong count msgSize 1, 0)
      val _ = T.spawnOnProc (fn () => ping count msgSize 0, 1)
    in
      ()
    end)
end

val (count, msgSize) =
   case CommandLine.arguments () of
      s1::s2::_ => (case (Int.fromString s1, Int.fromString s2) of
                        (SOME n1, SOME n2) => (n1, n2)
                       | _ => raise Fail "Needs 2 arguments")
    | _ => raise Fail "Needs 2 arguments"

val ts = Time.now ()
val _ = Main.doit (count, msgSize)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
