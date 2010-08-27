structure Main =
struct
   structure A = Array
   open MLton
   structure T = PCML

   val channel = T.channel
   val send = T.send
   val recv = T.recv

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
     (
     MLton.RunPCML.doit ( fn () =>
     let
       val channelArr = A.tabulate(16, fn _ => channel ())

       fun init m =
       let
         val inCh = A.sub(channelArr, (m-1+16) mod 16)
         val outCh = A.sub(channelArr, m)
       in
         ignore (T.spawn(fn () => worker(inCh, outCh, n)))
       end

       val _ = A.tabulate (8, fn x => init x)
       val _ = send(A.sub(channelArr, 15), ())
     in
       ()
     end
       , NONE))
end

val n =
   case CommandLine.arguments () of
      [] => 100
    | s::_ => (case Int.fromString s of
                  NONE => 100
                | SOME n => n)

val ts = Time.now ()
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
