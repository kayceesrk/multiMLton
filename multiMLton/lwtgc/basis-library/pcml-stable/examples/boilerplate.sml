(***** BOILERPLATE *****)
structure CML = MLton.PCML

val stabCount = ref 10

(** General-purposee functions *)
val nada = fn () => ()
fun stabCompens(fx, cf) =
  let
    val (stableF, _) = CML.Stable.stableCP (fx, cf)
  in stableF end

fun stab fx = stabCompens(fx, nada)

fun stabilize () =
  let val () = CML.Stable.unmonitoredAssign(stabCount, !stabCount - 1)
      val () = print "STABILIZES TO GO "
      val () = print (Int.toString (!stabCount))
      val () = print "\n"
  in if !stabCount = 0
     then OS.Process.exit OS.Process.success
     else CML.Stable.stabilize() handle Fail (str) => print ("EXN: "^str)
                                        | _ => print "SOMEONE RAISED in stabilize!!!\n"
  end

fun pr msg = print ("test: " ^ msg ^ "\n")


val _ = print("TEST - about to start CML\n")
fun run(f) = MLton.RunPCML.doit(stab f, NONE)

fun error msg = (pr msg; OS.Process.exit OS.Process.success)
