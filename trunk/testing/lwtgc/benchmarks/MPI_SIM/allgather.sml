structure Main =
struct
  open MLton.Pacml

  fun p () : string = (*Int.tostring(B.processorNumber())*)""
  fun print str = ()

  val inCh1 = channel ()
  val inCh2 = channel ()
  val inCh3 = channel ()
  val inCh4 = channel ()

  val nch1 = channel()
  val nch2 = channel()
  val nch3 = channel()
  val nch4 = channel()

  fun proc1 (outCh) =
	let val _ = spawn(fn () => aSend(outCh,0)) in () end

  fun proc2 (inCh) =
	let val j = recv inCh in print(Int.toString(j)) end

  fun bcast k =
	let
	val _ = aSend (nch1, k)
	val _ = spawn(fn () => proc2(nch1))
	val _ = aSend (nch2, k)
	val _ = spawn(fn () => proc2(nch2))
	val _ = aSend (nch3, k)
	val _ = spawn(fn () => proc2(nch3))
	val _ = aSend (nch4, k)
	val _ = spawn(fn () => proc2(nch4))
	in () end

  fun flatloop () =
	let
	val _ = spawn(fn ()=> proc1(inCh1))
	val _ = spawn(fn ()=> proc1(inCh2))
	val _ = spawn(fn ()=> proc1(inCh3))
	val _ = spawn(fn ()=> proc1(inCh4))
	val n1 = recv inCh1
	val n2 = recv inCh2
	val n3 = recv inCh3
	val n4 = recv inCh4
	val _ = bcast(n1+n2+n3+n4)
	in () end



  fun allgather n =
	let
        fun loop i = if i > n then () else (flatloop();loop(i+1))
	val _ = loop 0
	in () end


   fun doit n =
   let
     val _ = print "\nIn doit"
   in
     run
      (fn () =>
       let
          val _ = print ("\nCreating channel "^(p()))
          val ch : int chan = channel ()
          val _ = print ("\nCreating ping "^(p()))
	        val lst = List.tabulate(16,fn x => 1)
          val () = allgather n
          val _ = print ("\nCreated ping ping"^(p()))
       in
          ()
       end)
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
