structure Main =
struct

  open MLton.Pacml

  fun ping ch n =
    if n=0 then ()
    else
      (send (ch, n);
       print (concat ["Ping ", Int.toString n, "\n"]);
       ping ch (n-1))

  fun pong ch n =
    if n=0 then ()
    else
      (ignore (recv ch);
       print (concat ["Pong ", Int.toString n, "\n"]);
       pong ch (n-1))

  fun doit n =
  let
    val ch = channel ()
  in
    run
    (fn () =>
    let
      val _ = print "spawning pong\n"
      val _ = spawnHost (fn () => pong ch n)
      val _ = print "spawning ping\n"
      val _ = spawnHost (fn () => ping ch n)
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

val _ = Main.doit n
