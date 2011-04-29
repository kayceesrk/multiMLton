structure Main =
struct
  open MLton
  open PCML
  open Threadlet

  fun p () : string = (*Int.tostring(B.processorNumber())*)""
  fun print str = ()

  val ch1 = newAChan()
  val ch2 = newAChan()
  fun ping (n,data) =
  let

    fun loop (i,f) = if i > n then () else (f();loop(i+1,f))
    fun p1 () =
    let
      val _= async(fn () => aSend(ch1,data))
      val j = aRecv(ch2)
    in () end

    fun p2() =
    let
      val _ = async(fn ()=> aSend(ch2,data))
      val j = aRecv(ch1)
    in () end

    val _ = spawn(fn()=>loop(0,p1))
    val _ = spawn(fn()=>loop(0,p2))
  in () end

  (*fun ping ch n =
	let
	fun loop (i,ch) =
		if i > n then let val j = aRecv ch in () end
		else
			let
			val nch :int chan = newAChan ()
			val _ = async(fn () => aSend(nch, i*10))
                        (*val _ = aSend(nch, i*10)*)
			val _ = print ("\nSent\n"^Int.toString(i*10))
			val j = aRecv ch
			val _ = print ("\nRecieved at "^Int.toString(i)^" value"^Int.toString(j))
			in loop(i+1, nch) end
	val _ = print "\nSpawing the Ping\n"
	val _ = spawn(fn () => ((print "\n Running Ping");loop(0,ch)))
	in () end*)

   fun doit n =
   let
     val _ = (*print "\nIn doit"*)()
   in
      RunPCML.doit
      (fn () =>
       let
         val data = List.tabulate(n,fn(x)=>"a")
         val _ = ping (10000,data)
       in
          ()
       end,
       NONE)
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
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
