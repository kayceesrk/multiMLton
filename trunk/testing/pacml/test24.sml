structure Main =
struct
  open MLton.Pacml
  fun doit () =
    run (fn () =>
            let fun print(x) = ()
                val ts = Time.now ()
                val mChan = Multicast.mChannel()
                val port1 = Multicast.port(mChan)
                val port2 = Multicast.port(mChan)
                val tail = channel()
                fun mCaster(x) =
                  if x = 0
                  then ()
                  else (print("sending " ^ Int.toString(x) ^ "\n"); Multicast.multicast(mChan, x); mCaster(x-1))
               fun mReceiver(x, port) =
                 if x = 0
                 then ()
                 else (print("received" ^ Int.toString(Multicast.recv(port)) ^ "\n"); mReceiver(x-1, port))
               val _ = spawn(fn () => mReceiver(10000, port1))
               val _ = spawn(fn () => (mReceiver(10000, port2); send(tail, ())))
               val _ = spawn(fn () => mCaster(10000))
	       val _ = recv(tail)
               val te = Time.now ()
	       val d = Time.-(te, ts)
	       val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
            in (OS.Process.exit OS.Process.success)
            end)
end

val _ = Main.doit ()

