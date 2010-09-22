structure Main =
struct
  structure T = MLton.Pacml

  val channel = T.channel
  val send = T.send
  val recv = T.recv
  val sSync = T.sSync
  val sendEvt = T.sendEvt

  fun ping ch n =
    if n=0 then ()
    else
      (sSync (sendEvt (ch, n));
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
    T.run
    (fn () =>
    let
      val _ = T.spawnHost (fn () => pong ch n)
      val _ = T.spawnHost (fn () => ping ch n)
    in
      ()
    end)
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
