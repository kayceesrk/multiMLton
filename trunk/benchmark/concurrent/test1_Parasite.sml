structure Main =
struct
  open MLton
  structure T = PCML
  structure P = PCML.Threadlet

  val channel = P.newAChan
  val send = P.pSend
  val recv = P.pRecv

  fun ping ch n =
    if n=0 then ()
    else
      (send (ch, n);
       ping ch (n-1))

  fun pong ch n =
    if n=0 then ()
    else
      (ignore (recv ch);
       pong ch (n-1))

  fun doit n =
  let
    val ch = channel ()
  in
    RunPCML.doit
    (fn () =>
    let
      val _ = P.parasite (fn () => ping ch n)
      val _ = P.parasite (fn () => pong ch n)
    in
      ()
    end,
    NONE)
  end
end

val n =
   case CommandLine.arguments () of
      [] => 100
    | s::_ => (case Int.fromString s of
                  NONE => 100
                | SOME n => n)

val ts = Time.now ()
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
