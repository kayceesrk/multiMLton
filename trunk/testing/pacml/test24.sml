structure Main =
struct
  open MLton.Pacml
  fun doit () =
    run (fn () =>
            let val mChan = Multicast.mChannel()
                val port1 = Multicast.port(mChan)
                val port2 = Multicast.port(mChan)
                fun mCaster(x) =
                  if x = 0
                  then ()
                  else (print("sending " ^ Int.toString(x) ^ "\n"); Multicast.multicast(mChan, x); mCaster(x-1))
               fun mReceiver(x, port) =
                 if x = 0
                 then ()
                 else (print("received" ^ Int.toString(Multicast.recv(port)) ^ "\n"); mReceiver(x-1, port))
            in
              (spawn(fn () => mReceiver(100, port1));
               spawn(fn () => mReceiver(100, port2));
               spawn(fn () => mCaster(100));())
            end)

end


val ts = Time.now ()
val _ = Main.doit ()
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
