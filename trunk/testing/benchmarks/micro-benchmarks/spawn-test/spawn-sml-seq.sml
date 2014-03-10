
structure Main =
struct
   open CML

    fun spawnHelper 0 = RunCML.shutdown OS.Process.success
      | spawnHelper n = (ignore (spawn (fn () => ()));
                         if n mod 64 = 0 then yield () else ();
                         spawnHelper (n-1))

   fun doit n =
      RunCML.doit
      (fn () =>
       let
         val _ = spawnHelper n
       in
          ()
       end,
       SOME (Time.fromMilliseconds 10))
end


val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 600
val _ = Main.doit n
