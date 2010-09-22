
structure Main =
struct
  open MLton
   open PCML
   fun loop n = if n=0 then ()
                else
                  loop (n-1)
  fun print' msg =
  let
    val _ = print (msg^" Before "^Int.toString(0))
    val _ = loop 9000000
  in
    print (msg^" After  "^Int.toString(0))
  end


   fun doit' n = if n=0 then ()
                 else
                    (spawn( fn () => print' ("\nThread"^Int.toString(n)))
                    ; doit' (n-1))
   fun doit n =
     (print "\nIn Main.doit";
     RunPCML.doit ( fn () =>
       doit' n, NONE))
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
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
