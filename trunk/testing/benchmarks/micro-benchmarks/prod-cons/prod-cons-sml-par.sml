open MLton.Pacml

fun prod c 0 = OS.Process.exit OS.Process.success
  | prod c n = (send (c,1); prod c (n-1))

fun cons c 0 = ()
  | cons c n = (ignore (recv c); cons c (n-1))

fun main n =
let
  val c = channel ()
  val _ = spawnParasite (fn () => prod c n)
  val _ = spawnParasite (fn () => cons c n)
in
  ()
end

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 600
val _ = run (fn () => main n)
