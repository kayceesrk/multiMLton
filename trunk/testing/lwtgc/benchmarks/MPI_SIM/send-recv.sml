structure Main =
struct
  open MLton
  open PCML

  fun p () : string = (*Int.tostring(B.processorNumber())*)""
  fun print str = ()

  val inCh1 = channel ()
  val outCh1 = channel ()

  val inCh2 = channel ()
  val outCh2 = channel ()

  val inCh3 = channel ()
  val outCh3 = channel ()

  val inCh4 = channel ()
  val outCh4 = channel ()

  fun proc (inCh, outCh) =
	let
	val i = recv (inCh)
	val _ = send (outCh,i+1)
	val _ = print ("\nRecv :"^Int.toString(i)^" and Sending : "^Int.toString(i+1))
	in () end

  fun flatloop () =
	let
	val _ = spawn(fn () =>  send(inCh1, 0))
	val _ = spawn(fn () => proc(inCh1,outCh1))
	val _ = spawn(fn () => proc(outCh1,outCh2))
	val _ = spawn(fn () => proc(outCh2,outCh3))
	val _ = spawn(fn () => proc(outCh3,outCh4))
	val d = recv outCh4
	in () end

  fun sendRecv n =
	let
	fun loop i = if i > n then () else (flatloop();loop(i+1))
	val _ = loop 0
	in () end


  (*fun sendRecv ch n =
	let
	fun loop (i,inch) =
		if i > n then let val _ = recv inch val _ = spawn(fn ()=> send(ch, 0)) in () end
		else
			let
			val nch :int chan = channel ()
			val _ = spawn(fn () => send(nch,i))
			val _ = spawn(fn () => loop(i+1,nch))
			val i = recv inch
			val _ = print ("\nRecv  "^Int.toString(i))
			in ()  end
	val _ = spawn ( fn () => loop (0,ch))
	in () end *)



   fun doit n =
   let
     val _ = print "\nIn doit"
   in
      RunPCML.doit
      (fn () =>
       let
          val _ = print ("\nCreating channel "^(p()))
          val ch : int chan = channel ()
          val _ = print ("\nCreating ping "^(p()))
          val () = sendRecv n
          val _ = print ("\nCreated ping ping"^(p()))
       in
          ()
       end,
       SOME (Time.fromMilliseconds 10))
       (* n = 2,000,000 *)
   end
end

val n =
   case CommandLine.arguments () of
      [] => 100
    | s::_ => (case Int.fromString s of
                  NONE => 100
                | SOME n => n)

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
