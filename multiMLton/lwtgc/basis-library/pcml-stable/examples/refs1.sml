(**
 * Simple refs test
 **)

(* global ref created outside CML *)
val x = ref 0

(***** CML BOILERPLATE *****)
val _ = run (fn()=> let
(***** START CODE *****)

fun f1() = 
  let val () = print ("x=" ^ Int.toString (!x) ^ "\n")
      val () = x := 1 (* originally 0 *)
      val () = x := 2 (* ensure undo's unroll in rev order *)
      val () = Stable.unmonitoredAssign(x, 3) (* unmonitored assign, so should not error *)
      val _ = stabilize()
  in ()
  end

(* Check that x was restored back to 0. *)
val () = (stabCompens (f1, fn() => if !x = 0
                                   then print ("Test CORRECT! x=" ^ Int.toString(!x) ^ "\n")
                                   else print ("Test FAILED ! x=" ^ Int.toString(!x) ^ "\n"))) ()


(**** CML BOILERPLATE ****)
in () end)

