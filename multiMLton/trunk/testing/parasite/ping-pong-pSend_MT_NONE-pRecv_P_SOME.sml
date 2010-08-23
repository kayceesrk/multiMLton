

(**
 * Parasitic receive blocks only once because of the eagerness of parasite.
 * All other times, parasitic receive finds a value on the channel. Run it
 * with threadlet debugging turned on to see the behavior in
 * trunk/basis-library/acml/util/debug.sml
 *
 * See trunk/basis-library/acml/core-cml/threadlet.sml for implementation.
 *)

structure Main =
struct

  open MLton
  structure T = PCML.Threadlet
  structure P = PCML

   fun p () : string =
        (*Int.toString(B.processorNumber())*)""

   fun print s = ""


   fun pong ch n =
      let
         fun loop n =
            let
               val _ = print ("Before receiving\n")
               val i = (T.pRecv ch)
               val _ = print ("Received "^Int.toString(i)^" "^(p())^"\n")
            in
              if n>0 then loop (n-1) else ()
            end
         val _ = T.parasite (fn () => if n = 0 then () else loop (n))
      in
         ()
      end

   fun ping ch n signal =
      let
         fun loop i =
            if i > n then ()
               else let
                       val _ = print ("aSend "^(Int.toString n)^"\n")
                       val _ = if i=0 then P.send (signal, ()) else ()
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
     val _ = print "\nIn doit"
   in
      RunPCML.doit
      (fn () =>
       let
          val _ = print ("\nCreating channel "^(p()))
          val ch = T.newAChan ()
          val signal = P.channel ()

          val _ = print ("\nCreating ping "^(p()))
          val _ = P.spawn (fn () => ping ch n signal)

          (* Make sure the main thread has been started *)
         val () = P.recv signal

          val _ = print ("\nCreating pong "^(p()))
          val () = pong ch n

          val _ = print ("\nCreated ping and pong "^(p()))
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
