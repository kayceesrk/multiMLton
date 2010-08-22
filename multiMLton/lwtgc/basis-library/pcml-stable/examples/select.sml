(**
 **)

(***** CML BOILERPLATE *****)
val _ = run (fn() => let
(***** START CODE *****)






fun select() = 
  let val _ = pr "About to create channel"
      val ch = CML.channel()
      val _ = pr "Just created channel"
      fun sendLoop() = 
        let val _ = CML.send(ch, 0)
        in sendLoop()
        end
      val _ = CML.spawn(sendLoop)
  in
     CML.select [
              CML.wrap (CML.recvEvt ch, fn x => (pr "1 selected"; stabilize())),
              CML.wrap (CML.recvEvt ch, fn x => (pr "2 selected"; stabilize()))
              ]
  end

val x =  (stab select) ()
val () = pr "Done"




(**** CML BOILERPLATE ****)
in () end)
