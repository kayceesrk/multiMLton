structure Main =
struct

  open MLton.Pacml

  fun doit () =
    run (fn () =>
            let val _ = print "Testing Starting\n"
                
              val _ = NonBlocking.execute (fn () => print "Success1\n")
              val p = case NonBlocking.createProcessor () of
                           NONE => raise Fail "Cannot create processor. Need more processors"
                         | SOME p => p
              val _ = NonBlocking.executeOn p (fn () => print "Success2\n")
              val p = case NonBlocking.createProcessor () of
                           NONE => raise Fail "Cannot create processor. Need more processors"
                         | SOME p => p
              val _ = NonBlocking.executeOn p (fn () => print "Success3\n") 
              val p = case NonBlocking.createProcessor () of
                           NONE => raise Fail "Cannot create processor. Need more processors"
                         | SOME p => p
              val _ = NonBlocking.executeOn p (fn () => print "Success4\n")
           in
              ()
            end)

end


val ts = Time.now ()
fun wait(n) = 
 if n = 0
 then ()
 else wait(n-1)
val _ = wait(1000000)
val _ = Main.doit ()
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
