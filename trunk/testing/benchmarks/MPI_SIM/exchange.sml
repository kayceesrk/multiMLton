structure Main =
struct
  open MLton
  open PCML
  open Threadlet

  fun p () : string = (*Int.tostring(B.processorNumber())*)""
  fun print str = ()
  val lst = List.tabulate(5, fn n=>let val lch = newAChan() val nch= newAChan() in (lch,nch) end)
  val sch = newAChan ()
  val ch = newAChan ()

  val ch1 = newAChan ()
  val ch2 = newAChan ()
  val ch3 = newAChan ()
  val ch4 = newAChan ()

(*  val nch1 = newAChan ()
  val nch2 = newAChan ()
  val nch3 = newAChan ()
  val nch4 = newAChan ()*)
  fun exchange (n,data) =
	let
      fun loop (i,f) = if i > n then () else (f();loop(i+1,f))

      fun p1 () =
      let
        val _ = async(fn () => aSend(ch2,data))
        val v1 = aRecv(ch2)
      in () end

      fun p2 ()=
      let
        val _ = async(fn()=>aSend(ch1,data))
        val _ = async(fn()=>aSend(ch3,data))
        val v1 = aRecv(ch1)
        val v1 = aRecv(ch3)
      in () end

      fun p3 ()=
      let
        val _ = async(fn()=>aSend(ch2,data))
        val _ = async(fn()=>aSend(ch4,data))
        val v1 = aRecv(ch2)
        val v1 = aRecv(ch4)
      in () end

      fun p4 () =
      let
        val _ = async(fn () => aSend(ch3,data))
        val v1 = aRecv(ch3)
      in () end

  val _ = spawn(fn()=> loop(0,p1))
  val _ = spawn(fn()=> loop(0,p2))
  val _ = spawn(fn()=> loop(0,p3))
  val _ = spawn(fn()=> loop(0,p4))
	in () end




   fun doit n =
   let
     val _ = ()
   in
      RunPCML.doit
      (fn () =>
       let
         val data = List.tabulate(n, fn(x)=>"a")
          val () = exchange (10000,data)
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
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
