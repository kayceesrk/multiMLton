(***** BOILERPLATE *****)
val stabCount = ref 10

(** General-purposee functions *)
val nada = fn () => ()
(*fun stabCompens(fx, cf) =
  let val (stableF, _) = Stable.stableCP (fx, cf)
  in stableF end*)

fun stab fx = fx (*stabCompens(fx, nada) *)

(*
fun stabilize () =
  let val () = Stable.unmonitoredAssign(stabCount, !stabCount - 1)
      val () = print "STABILIZES TO GO "
      val () = print (Int.toString (!stabCount))
      val () = print "\n"
  in if !stabCount = 0
     then OS.Process.exit OS.Process.success
     else Stable.stabilize() handle _ => print "SOMEONE RAISED in stabilize!!!\n"
  end*)

fun pr msg = print ("test: " ^ msg ^ "\n")

fun run(f) = MLton.Pacml.run (f)

fun error msg = (pr msg; OS.Process.exit OS.Process.success)
