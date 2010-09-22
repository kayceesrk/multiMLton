structure Main =
struct

  open MLton

  structure A = PCML.Aux
  structure T = PCML.Threadlet
  structure P = PCML

   fun p () : string = Int.toString(A.processorNumber())

   fun println s = print (s^" ["^(p())^"]\n")
   fun println' s = ()

   fun doit n =
   let
     val _ = print "\nIn doit"
   in
      RunPCML.doit
      (fn () =>
       let
         fun spin n = if A.isParasite () then
                        (println' ("AtomicState: "^(Int.toString(A.getAtomicState())));
                         spin n)
                      else println (Int.toString(n)^" Exiting HOST")
         fun loop n =
           if n=0 then ()
           else (T.parasite (fn () => spin n);loop (n-1))
       in
          loop n
       end,
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
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
