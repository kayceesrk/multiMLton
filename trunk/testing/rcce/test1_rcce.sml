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

  fun pong count readCount you =
  let
    fun loop n a =
      if n=0 then print ("Sum: "^Int.toString (a)^"\n")
      else
        let
          val v = recv you
          fun loop2 rc b =
            if rc = 0 then b
            else
              let
                val d = Vector.foldl (fn (i, s) => s+i) b v
              in
                loop2 (rc-1) d
              end
          val c = loop2 readCount a
        in
          loop (n-1) c
        end
  in
    loop count 0
  end

  fun doit (count, msgSize, readCount) =
    T.run
    (fn () =>
    let
      val _ = T.spawnOnProc (fn () => pong count readCount 1, 0)
      val _ = T.spawnOnProc (fn () => ping count msgSize 0, 1)
    in
      ()
    end)
end

val (count, msgSize, readCount) =
   case CommandLine.arguments () of
      s1::s2::s3::_ => (case (Int.fromString s1,
                            Int.fromString s2,
                            Int.fromString s3) of
                        (SOME n1, SOME n2, SOME n3) => (n1, n2, n3)
                       | _ => raise Fail "Needs 3 arguments")
    | _ => raise Fail "Needs 3 arguments"

val ts = Time.now ()
val _ = Main.doit (count, msgSize, readCount)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
