(**
 * Simple refs test
 **)

datatype 'a ref_2 = MEM of {
	value : 'a ref,
	index : int
}

local
  val count = ref 0
  fun next() =
    let val i = !count
        val () = count := i + 1
    in i end
  fun read (MEM{value, index}) = (print ("MEM:read:" ^ Int.toString index ^ "\n"); ! value)
  fun write (MEM{value, index}, v) = (print ("MEM:write:" ^ Int.toString index ^ "\n"); value := v)
in
  fun ref_2 v = MEM{value = ref v, index = next()}
  val ! = read
  val (op :=) = write
end

(* global ref created outside CML *)
val x = ref_2 0

(***** CML BOILERPLATE *****)
val _ = run (fn()=> let
(***** START CODE *****)

fun f1() = 
  let val () = print ("x=" ^ Int.toString (!x) ^ "\n")
(*      val () = CML.:= (x, 1)
      val () = CML.:= (x, 2) (* ensure undo's unroll in rev order *)
*)
      val () = x := 1 (* unmonitored assign, so should not error *)
      val _ = stabilize()
  in ()
  end

(* val () = (stab f1) ()*)
val () = (stabCompens (f1, fn() => if !x = 0
                                   then print "Test CORRECT!\n"
                                   else print "Test FAILED!\n")) ()


(**** CML BOILERPLATE ****)
in () end)

