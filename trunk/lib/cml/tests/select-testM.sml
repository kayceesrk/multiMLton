structure Main =
struct

  open MLton
  open PCML

  fun doit n =
  let
    val ch1 = channel ()
    val ch2 = channel ()
    fun producer1 n = if n=0 then () else
                        (sync (sendEvt (ch1, (1,n)))
                         ; producer1 (n-1))

    fun producer2 n = if n=0 then () else
                        (sync (sendEvt (ch2, (2,n)))
                         ; producer2 (n-1))
    fun consumer n c = if n=0 then () else
                        let
                          val (p,y) = select [recvEvt ch1, recvEvt ch2]
                          val _ = print ("\n"^(Int.toString(y))
                                        ^" P : "^(Int.toString(p))
                                         ^" C : "^(Int.toString(c))
                                        )
                        in
                          consumer (n-1) c
                        end
    fun doit' n =
      let
        val t = spawn (fn ()=> producer1 n)
        val _ = print ("\nproducer1 : "
                        ^(tidToString(t)))
        val t = spawn (fn () => producer2 n)
        val _ = print ("\nproducer2 : "
                        ^(tidToString(t)))
        val t = spawn (fn () => consumer (2*n) 1)
        val _ = print ("\nconsumer1 : "
                        ^(tidToString(t)))
        val t = spawn (fn () => consumer (2*n) 2)
        val _ = print ("\nconsumer2 : "
                        ^(tidToString(t)))
        val _ = consumer (2*n) 3
      in
        ()
      end
  in
    RunPCML.doit( fn() =>
        doit' n, NONE)
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
