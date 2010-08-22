(**
 * Simple refs test
 **)

(***** CML BOILERPLATE *****)
val _ = run (fn() => let
(***** START CODE *****)

val MAX_COMS = 450
val MAX_THREADS = 100

val c = CML.channel()

fun t id = (* Read val and send new one *)
  let val x = CML.recv(c)
      val () = print ("tid=" ^ Int.toString id ^ " x=" ^ Int.toString (x) ^ "\n")
  in if x = MAX_COMS
      then stabilize()
      else (CML.send(c, x + 1); (stab t) id)
  end

(* spawn all the threads *)
fun spawner(i) =
  if i = 0
  then ()
(*  else (CML.spawn (stab t); spawner (i - 1))*)
  else (CML.spawn (fn() => ((stab t) i)); spawner (i - 1))

val () = spawner MAX_THREADS

val () = CML.send(c, 0)

(**** CML BOILERPLATE ****)
in () end)

