
structure Main =
struct
  open MLton.Pacml

   fun makeNatStream c =
      let
         val ch = channel ()
         fun count i = (send(ch, i)
                        ; count(i+1))
         val _ = spawn (fn () =>
                        (print (concat ["makeNatStream: ",
                                        tidToString (getTid ()),
                                        "\n"])
                         ; count c))
      in
         ch
      end

   fun makeFilter (p, inCh) =
      let
         val outCh = channel ()
         fun loop () =
            let
               val i = (recv inCh)
            in
               if ((i mod p) <> 0)
                  then (send (outCh, i))
                  else ()
               ; loop ()
            end
         val _ = spawn loop
      in
         outCh
      end

   fun makePrimes () =
      let
         val primes = channel ()
         fun head ch =
            let val p = recv ch
            in
               send(primes, p)
               ; head (makeFilter (p, ch))
            end
         val _ = spawn (fn () =>
                        (print (concat ["makePrimes: ",
                                        tidToString (getTid ()),
                                        "\n"])
                         ; head (makeNatStream 2)))
      in
         primes
      end

   fun makeNatPrinter ch n =
      let
         fun loop i =
            if i > n then shutdown OS.Process.success
               else let
                       val m = recv ch
                       val m' = Int.toString m
                       fun loop' j =
                          if j > m then ()
                          else (print (m' ^ "\n")
                                ; loop' (j + 1))
                    in
                       loop' m
                       ; loop (i + 1)
                    end
         val _ = spawn (fn () =>
                        (print (concat ["makeNatPrinter: ",
                                        tidToString (getTid ()),
                                        "\n"])
                         ; loop 0))
      in
         ()
      end

   fun doit' n =
     run
      (fn () =>
       let
          val ch = makePrimes ()
          val _ = makeNatPrinter ch n
       in
          ()
       end)

   fun doit n =
      let
         val x = doit' n
      in
         x
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
