open TextIO
open CML

(**
 **)

(***** CML BOILERPLATE *****)
val _ = run (fn() => let
(***** START CODE *****)



val ch1 = channel()
val ch2 = channel()

val ev1 = sendEvt(ch1, ())
val ev2 = recvEvt(ch1)
val ev3 = sendEvt(ch2, "hi")
val ev4 = recvEvt(ch2)

fun f1(b) = 
  let val () = print "f1\n"
  in sync(wrap(ev1, f2))
  end

and f2(x) =
  let val () = print "f2\n"
  in sync(wrap(ev2, f1))
  end


val ev5 = wrap(ev1, f1)
val ev6 = wrap(ev2, f2)

val _ = spawn (fn()=> sync(ev5))
val _ = sync(ev6)


(**** CML BOILERPLATE ****)
in () end)
