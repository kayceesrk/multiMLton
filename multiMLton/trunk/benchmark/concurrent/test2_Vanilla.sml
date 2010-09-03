structure Main =
struct
  open MLton
  structure T = PCML

  val channel = T.channel
  val send = T.send
  val recv = T.recv

  fun ping ch n1 n2 =
    if n1=0 then ()
    else
      let
        fun loop n = if n=0 then () else  (send (ch, n1);loop (n-1))
        val _ = T.spawn (fn ()=> loop n2)
      in
        ping ch (n1-1) n2
      end

  fun pong ch n1 n2 =
    if n1=0 then ()
    else
      let
        fun loop n = if n=0 then () else  (ignore (recv ch);loop (n-1))
        val _ = T.spawn (fn () => loop n2)
      in
        pong ch (n1-1) n2
      end

  fun doit (n1,n2) =
  let
    val ch = channel ()
  in
    RunPCML.doit
    (fn () =>
    let
      val _ = T.spawn (fn () => pong ch n1 n2)
      val _ = T.spawn (fn () => ping ch n1 n2)
    in
      ()
    end,
    NONE)
  end
end

val (n1, n2) =
   case CommandLine.arguments () of
      [] => raise Fail "Need 2 arguments"
    | s1::s2::_ => (case (Int.fromString s1, Int.fromString s2) of
                         (SOME n1, SOME n2) => (n1, n2)
                       | (_,_) => raise Fail "Need 2 arguments")

val ts = Time.now ()
val _ = Main.doit (n1, n2)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
