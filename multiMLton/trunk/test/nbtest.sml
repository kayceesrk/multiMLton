structure NB = MLton.PCML.NonBlocking

val proc = valOf(NB.createProcessor ())
fun loop n =
    if n = 0 then () else
      let
        val _ = NB.execute (fn () => print ("Hello\n"))
        val _ = NB.executeOn proc (fn () => print "Hello_ded\n")
      in
        loop (n-1)
      end

val _ = MLton.RunPCML.doit (fn () => loop 10, NONE)
