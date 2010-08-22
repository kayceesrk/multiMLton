(**
 * Checks that a thread NOT in a stable section will be unrolled
 * to a point before channel communication.
 * 
 *    | tMain  | t1          | t0
 * ---+--------+-------------+-------------
 * 01:           enterSS
 * 02:           printBefore   
 * 03:  send0    recv0         printBefore
 * 04:           send1         recv1
 * 05:                         stabilize
 * ----------------------------------------
 * Now t1 should restore to line 01
 * and t0 should restore to line 04 (never printing "before" again)
 *
 * This means that part of t2's computation does not need to be redone
 **)

(***** CML BOILERPLATE *****)
val _ = run (fn() => let
(***** START CODE *****)

val MAX_COMS = 2
val MAX_THREADS = 1

val c = CML.channel()

(* Reads a val from chan and sends incremented val *)
fun tRec id = (* Read val and send new one *)
  let val x = CML.recv(c)
      val () = print ("tid=" ^ Int.toString id ^ " x=" ^ Int.toString (x) ^ "\n")
  in if x = MAX_COMS
      then stabilize()
      else (CML.send(c, x + 1); tRec id)
  end

fun t id = 
  let val () = print ("tid=" ^ Int.toString id ^ " before receive.\n")
  in tRec(id)
  end


(* spawn all the threads *)
fun spawner(0) = ignore (CML.spawn (fn() => (t 0))) (*not in ss*)
|   spawner(i) = (CML.spawn (fn() => ((stab t) i)); spawner (i - 1))

val () = spawner MAX_THREADS

val () = CML.send(c, 0)

(**** CML BOILERPLATE ****)
in () end)

