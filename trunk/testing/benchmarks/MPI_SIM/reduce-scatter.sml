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

  val nch1:int chan = channel()
  val onch1:int chan = channel()

  val nch2:int chan = channel()
  val onch2:int chan = channel()

  val nch3:int chan = channel()
  val onch3:int chan = channel()

  val nch4:int chan = channel()
  val onch4:int chan = channel()


  fun proc2 (inch,outch) = 
	let val v = recv inch
	    in () end 

  fun scatter n =
        let
                val k = n
                val _ = spawn(fn () => send (nch1, k))
                val _ = spawn( fn() => proc2(nch1,onch1))
                val _ = spawn(fn () => send (nch2, k))
                val _ = spawn( fn() => proc2(nch2,onch2))
                val _ = spawn(fn () => send (nch3, k))
                val _ = spawn( fn() => proc2(nch3,onch3))
                val _ = spawn(fn () => send (nch4, k))
                val _ = spawn( fn() => proc2(nch4,onch4))
                in () end

  fun sum ls = 
	(case ls of
	[] => 0 
  	| e::rest => (print ("\n"^Int.toString(e)); e + sum(rest)))

  fun proc1 (inCh, outCh) = 
	let
	val lst  = recv inCh
	val _ = spawn(fn () => send (outCh, sum(lst)))
	in () end

  fun flatloop (lst,n) = 
	let
	val _ = spawn(fn () => proc1(inCh1,outCh1))
	val _ = spawn(fn () => send(inCh1, List.take(lst,n)))
	val _ = spawn(fn () => proc1(inCh2,outCh2))
  	val _ = spawn(fn () => send(inCh2, List.drop(List.take(lst,n*2),n)))
	val _ = spawn(fn () => proc1(inCh3,outCh3))
  	val _ = spawn(fn () => send(inCh3, List.drop(List.take(lst,n*3),n*2)))
	val _ = spawn(fn () => proc1(inCh4,outCh4))
  	val _ = spawn(fn () => send(inCh4, List.drop(List.take(lst,n*4),n*3)))
	val v1 = (recv outCh1) + (recv outCh2) + (recv outCh3) + (recv outCh4)
	val _ = print ("\nthe sum is:"^Int.toString(v1))
	val _ = scatter v1
        in () end	
		
  fun reduce(lst,np,n) =  
	let 
	val l = List.length(lst)
	val index = 0 
	val ne = if (l mod np) = 0 then l div np else l mod np
        fun loop i = if i > n then () else (flatloop(lst,ne);loop(i+1))
	val _ = loop 0 
	in () end
	 
			
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
	  val lst = List.tabulate(16,fn x => 1) 
          val () = reduce(lst,4,n)
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
