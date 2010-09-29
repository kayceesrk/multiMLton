structure Main =
struct

  open MLton.Pacml

  fun doit n =
  let
    val ch = channel ()
  in
    run
    (fn () =>
    let
      fun loop n =
        if n=0 then ()
        else (spawnHost (fn () => ()); loop (n-1))
    in
      loop n
    end)
  end
end

val n =
   case CommandLine.arguments () of
      [] => raise Fail "Needs 1 argument"
    | s::_ => case Int.fromString s of
                   NONE => raise Fail "Needs int argument"
                 | SOME n => n

val ts = Time.now ()
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
