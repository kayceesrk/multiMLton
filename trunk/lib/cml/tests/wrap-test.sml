structure Main =
struct

  open MLton
  open PCML


  fun foo ch =
  let
    val _ = wrap (sendEvt (ch, 5), fn m => (print "\nFirst comm done"; send (ch, 6)))
  in
    ()
  end

   fun doit n =
   let
     val _ = print "\nIn doit"
   in
      RunPCML.doit
      (fn () =>
       let
         val _ = print "\nStart"
         val ch = channel ()
         val _ = spawn (fn () => foo ch)
         val _ = recv (ch)
       in
         print "\nDone"
       end
        ,
       SOME (Time.fromMilliseconds 10))
       (* n = 2,000,000 *)
   end
end

val n =
   case CommandLine.arguments () of
      [] => 100
    | s::_ => (case Int.fromString s of
                  NONE => 100
                | SOME n => n)

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
