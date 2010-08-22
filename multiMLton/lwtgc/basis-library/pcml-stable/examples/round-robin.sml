(** 
 * Round-robin. 
 * tx receives on ch(x-1)
 * tx sends to t(x+1) on chx
 *
 * We stabilize t1.
 *
 * Build all the other threads, then thread1
 **)

(***** CML BOILERPLATE *****)
val _ = run (fn()=> let
(***** START CODE *****)


val MAX_THREADS = 10
val ch1 = CML.channel()

(* Closure for tx's computation *)
fun readWrite threadNum chRead chWrite =
  let val x = CML.recv chRead
      val _ = pr("t" ^ Int.toString threadNum ^ " read " ^ Int.toString x)
  in CML.send (chWrite, (x+1))
  end

fun f() = ignore (CML.spawn f)
fun loop () = loop ()

fun buildThreads threadNum prevChan =
  let val chx = CML.channel()
      fun runnable() = (readWrite threadNum prevChan chx; ()(*ignore (CML.spawn loop)*))
      val _ = pr ("spawning t" ^ Int.toString threadNum)
      val _ = CML.spawn (stab runnable)
  in if threadNum = MAX_THREADS
     then chx
     else buildThreads (threadNum+1) chx
  end

(* Half the threads are created before t1 so they will be restored, not killed*)
val lastChan = buildThreads 2 ch1


(* Build Thread 1 *)
fun f1 () = 
  let val _ = pr "t1 in stable"
      val lastChan = buildThreads (2) lastChan
      val _ = CML.send (ch1, 1)
      val _ = pr ("t1 sent 1")
      val x = CML.recv lastChan
      val _ = pr ("t1 read " ^ Int.toString x)
      val _ = stabilize()
  in () end

val t1 = CML.spawn (stab f1)


(**** BOILERPLATE ****)
in () end)

