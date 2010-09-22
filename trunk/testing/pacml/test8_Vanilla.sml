structure Main =
struct

  open MLton.Pacml


  fun ping ch1 ch2 n =
    if n=0 then ()
    else
      (sSync (choose [sendEvt (ch1, n), sendEvt (ch2,n)]);
       ping ch1 ch2 (n-1))

  fun pong ch n =
    if n=0 then ()
    else
      (ignore (recv ch);
       pong ch (n-1))

  fun doit n =
  let
    val ch1 = channel ()
    val ch2 = channel ()
  in
    run
    (fn () =>
    let
      val _ = spawnHost (fn () => ping ch1 ch2 (2*n))
      val _ = spawnHost (fn () => pong ch1 n)
      val _ = spawnHost (fn () => pong ch2 n)
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
