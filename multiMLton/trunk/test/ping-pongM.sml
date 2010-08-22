
structure Main =
struct

  open MLton
  open PCML
  structure NB = struct fun execute f = f () end

   fun p () : string =
        (*Int.toString(B.processorNumber())*)""


   fun print' str  = ()
   fun pong ch n =
      let
         fun loop n =
            let
               val _ = NB.execute (fn () =>print' ("\nReceiving "^(p())))
               val i = (recv ch)
               val _ = NB.execute (fn () => print ("\nReceived "
                              ^Int.toString(i)
                              ^" "^(p())))
            in
              if n>0 then loop (n-1) else ()
            end
         val _ = print ("\nSpawning pong"^(p()))
         val _ = spawn (fn () => (print ("\nRunning pong "^(p())); if n = 0 then () else loop (n)))
      in
         ()
      end

   fun ping ch n =
      let
         fun loop i =
            if i > n then ()
               else let
                       val _ = print' ("\nSending "^(p())
                                      ^" "^Int.toString(i))
                       val _ = (send (ch, i))
                       val _ = print' ("\nSent "^(p())
                                      ^" "^Int.toString(i))
                    in
                       loop (i + 1)
                    end
         val _ = print ("\nSpawning ping"^(p()))
         val _ = spawn (fn () => (print ("\nRunning ping "^p());loop 0))
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
          val _ = spawn (fn () => print ("\nCreating channel "^(p())))
          val ch : int chan = channel ()
          val ch2 : unit chan = channel ()
          val _ = spawn (fn () => recv ch2)
          val _ = spawn (fn () => recv ch2)
          val _ = spawn (fn () => recv ch2)
          val _ = spawn (fn () => recv ch2)
          val _ = spawn (fn () => recv ch2)
          val _ = spawn (fn () => print ("\nCreating pong "^(p())))
          val () = pong ch n
          val _ = spawn (fn () => print ("\nCreating ping "^(p())))
          val () = ping ch n
         val _ = spawn (fn () => print ("\nCreated ping and pong "^(p())))
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
