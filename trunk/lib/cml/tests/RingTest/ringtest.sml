
structure Main =
struct
   open MLton.PCML

  val pn = fn _ => 0

  fun worker (inCh, outCh, n) =
  let
    fun loop n = if n=0 then () else
                    (send(outCh, recv(inCh))
                     ;print ("\nDone round : "
                          ^Int.toString(n)
                          ^" P# : "
                          ^Int.toString(pn()))
                     ;loop (n-1))
  in
    loop n
  end


  fun doit n =
     (print "\nIn Main.doit";
     MLton.RunPCML.doit ( fn () =>
     let
       val channelArr = Array.tabulate(8, fn _ => channel ())

       fun init m =
       let
         val inCh = Array.sub(channelArr, (m-1+8) mod 8)
         val outCh = Array.sub(channelArr, m)
       in
         ignore (spawn(fn () => worker(inCh, outCh, n)))
       end

       val _ = Array.tabulate (8, fn x => init x)
       val _ = send(Array.sub(channelArr, 7), ())
     in
       ()
     end
       , NONE))
end

val _ = Main.doit 100
