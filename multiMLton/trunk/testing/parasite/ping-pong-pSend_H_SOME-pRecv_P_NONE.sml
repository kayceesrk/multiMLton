
structure Main =
struct

  open MLton
  structure T = PCML.Threadlet

   fun p () : string =
        Int.toString(MLton.PCML.Aux.processorNumber())

   fun print' s = ""
   fun println s = (s^"\n")


   fun pong ch n =
      let
         fun loop n =
            let
               val _ = print' ("Before receiving\n")
               val i = (T.pRecv ch)
               val _ = print' ("Received "^Int.toString(i)^" "^(p())^"\n")
            in
              if n>0 then loop (n-1) else ()
            end
         val _ = T.parasite (fn () => if n = 0 then () else loop (n))
      in
         ()
      end

   fun ping ch n =
      let
         fun loop i =
            if i > n then ()
               else let
                       val _ = print' ("aSend "^(Int.toString n)^"\n")
                       val _ = (T.pSend (ch, i))
                    in
                       loop (i + 1)
                    end
         val _ = loop 0
      in
         ()
      end

   fun doit n =
   let
     val _ = println "In doit"
   in
      RunPCML.doit
      (fn () =>
       let
          val _ = println ("Creating channel "^(p()))
          val ch = T.newAChan ()

          val _ = println ("Creating pong "^(p()))
          val _ = pong ch n

          val _ = println ("Creating ping "^(p()))
          val _ = ping ch n

        val _ = println ("Created ping and pong "^(p()))
       in
          ()
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
