structure Main=
struct
  open MLton.Pacml

  fun fib n ch () =
  let
    val ch1 = channel ()
    val ch2 = channel ()
  in
    if (n=0 orelse n=1) then
      send (ch, 1)
    else
      let
        val  _ = (spawnParasite (fib (n-1) ch1))
        val _ = (spawnParasite (fib (n-2) ch2))
      in
        send (ch, recv (ch1) + recv (ch2))
      end
  end

  fun doit' n =
  let
    val ch = channel ()
    val _ = spawn (fib n ch)
    val v = recv ch
    val _ = print ("Result: "^(Int.toString v)^"\n")
  in
    shutdown OS.Process.success
  end

  val doit = fn n => run (fn () => doit' n)

end

val n =
   case CommandLine.arguments () of
      [] => 10
    | s::_ => (case Int.fromString s of
                  NONE => 10
                | SOME n => n)

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
