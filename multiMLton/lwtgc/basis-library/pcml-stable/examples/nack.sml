(**
 **)

(***** CML BOILERPLATE *****)
val _ = run (fn() => let
(***** START CODE *****)

val correctRunFlag = ref false



exception TestError of string
fun error(msg) = (pr("ERROR:" ^ msg); raise TestError msg)



val ch = CML.channel()

(* Code that just sends on a channel. Not interesting *)
fun forever init f =
  let fun loop s = loop (f s)
  in ignore (CML.spawn (stab (fn() => f init)))
  end
fun sender(ch, v) =
  forever () (fn() =>
    let val () = CML.send (ch, v)
    in pr("sent")
    end)
val _ = sender(ch, 0)


(* From Concurrent Programming in ML Ch 4.2.5 *)



fun mkNackEvent(ev) = 
  let fun doNack(nack) = 
	let fun runNack() = (CML.sync nack; pr "NACK SENT. Stabilizing"; stabilize())
	    fun runAck _ = error "This Ack should NOT have run!"
	    val _ = CML.spawn(runNack)
	in CML.wrap(ev, runAck)
	end
  in CML.withNack(doNack)
  end



fun select() = CML.select [
                mkNackEvent(CML.recvEvt ch),
                CML.wrap (CML.recvEvt ch, fn x => pr "Message received. Yay!!!!!!!!!!!!!!!!!!!!")
              ]

val x =  stab select ()
val () = pr "Done"

(**** CML BOILERPLATE ****)
in () end)
