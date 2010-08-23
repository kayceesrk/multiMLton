datatype dummy = DONE | TIMEOUT | DATA of int

open MLton.PCML

fun fileReader name abortEvt consumer =
let
  val (arIn: dummy chan, arOut : unit chan) = (channel (), channel ())

  fun arbitrator() =
    (print "arbitrator\n";
    select [wrap (recvEvt arIn,
    fn chunk => send (consumer, chunk)),
      wrap (abortEvt,
      fn () => (aSync (aSendEvt(arOut, ()));
        send(consumer, TIMEOUT)))])

  fun sendChunk(chunk) =
    (print "sendChunk\n";
    aSync(aWrap(aSendEvt(arIn, DATA chunk),arbitrator)))

  fun loop strm =
    case strm
      of (chunk::strm) => select [recvEvt arOut,
                                    wrap(alwaysEvt (), fn () => (sendChunk(chunk); loop strm))]
       | [] => aSync (aSendEvt(arIn, DONE))
  val _ = aSync(aWrap(aRecvEvt(arIn), fn chunk => send(consumer, chunk)))
in
  (print "Starting producer\n";
  case name of
       [] => ()
     | _ => (loop name; print "producer Done\n"))
end

fun consumer ch =
    case recv ch of
          DATA i => (print ("Consumer Data "^Int.toString (i)^"\n\n"); consumer ch)
        | TIMEOUT => ()
        | DONE => print "Consumer is done\n"

fun doit () =
let
  val ch = channel ()
  val _ = spawn (fn () => consumer ch)
  val l = [1,2,3]
  val _ = spawn (fn () => fileReader l never ch)
in
  ()
end


val _ = MLton.RunPCML.doit (doit, NONE)
