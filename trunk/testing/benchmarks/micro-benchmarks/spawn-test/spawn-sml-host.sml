open MLton.Pacml

fun main 0 = OS.Process.exit OS.Process.success
  | main n = (ignore (spawn (fn () => ()));
              main (n-1))

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 600
val _ = run (fn () => main n)
