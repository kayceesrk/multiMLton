structure Main=
struct
  open MLton.PCML
  val ch1 = channel ()
  val ch2 = channel ()
  fun doit () =
  let
    val _ = spawn (fn () => send (ch2, ()))
    val e = aWrap (aRecvEvt(ch1), fn () => print "recv on ch1 completed")
    val e = aTrans (e)
    val e = wrap (e, fn () => (recv (ch2); print "recv on ch2 completed"))
    val _ = sSync (e)
    val _ = spawn (fn () => send (ch1, ()))
  in
    ()
  end

end

val _ = MLton.RunPCML.doit (Main.doit, NONE)
