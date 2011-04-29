
structure Main =
struct

  open MLton
  open PCML

   fun p () : string =
        (*Int.toString(B.processorNumber())*)""
   fun print str  = ()
   fun pong ch n =
      let
         fun loop i  = 
	    if i > n then () 
	    else 
            	let
               		val _ = print ("\nPong Receiving "^(p()))
               		val t = (recv ch)
            		val _ = print ("\n"^Int.toString(t))
  			in
               		loop (i+1)
            		end
         val _ = print ("\nSpawning pong"^(p()))
         val _ = spawn (fn () => (print ("\nRunning pong "^(p())); loop (0)))
      in
         ()
      end

   fun ping ch n =
      let
         fun loop i =
            if i > n then () 
               else let
                       val _ = print ("\nPing Sending "^(p())
                                      ^" "^Int.toString(i))
                       val _  = (send (ch, i*10))
			 
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
          val _ = print ("\nCreating channel "^(p()))
          val ch : int chan = channel ()
          val _ = print ("\nCreating pong "^(p()))
          val () = pong ch n
          val _ = print ("\nCreating ping "^(p()))
          val () = ping ch n
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
