
structure Main =
struct

  open MLton
  open PCML

   fun p () : string =
        (*Int.toString(B.processorNumber())*)""

  fun bar ch =
  let
              val _ = print ("\nReceiving "^(p()))
               val i = (recv ch)
               val _ = print ("\nReceived "
                              ^Int.toString(i)
                              ^" "^(p()))
  in
    ()
  end

  val bar = Stable.stable bar
   fun pong ch =
      let
         fun loop () =
            let
              val _ = bar ch
             in
               loop ()
            end
         val _ = print ("\nSpawning pong"^(p()))
         val _ = spawn (fn () => (print ("\nRunning pong "^(p())); loop ()))
      in
         ()
      end

      fun foo ch = let val _ = print ("\nSending "^(p())
                          ^" "^Int.toString(5))
            val () = (send (ch, 5))
            val _ = print ("\nSent "^(p())
                          ^" "^Int.toString(5))
      in
        ()
      end

  val foo = Stable.stable foo

   fun ping ch n =
      let
         fun loop i =
            if i > n then RunPCML.shutdown OS.Process.success
               else let
                 val _ = foo ch
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
          val () = pong ch
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
