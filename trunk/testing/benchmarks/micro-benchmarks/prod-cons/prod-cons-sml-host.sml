open MLton.Pacml

val c = channel ()

fun prod 0 = OS.Process.exit OS.Process.success
  | prod n = (send (c,1); prod (n-1))

fun cons 0 = ()
  | cons n = (ignore (recv c); cons (n-1))

fun main n =
let
  val _ = spawn (fn () => prod n)
  val _ = spawn (fn () => cons n)
in
  ()
end

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 600
val _ = run (fn () => main n)
