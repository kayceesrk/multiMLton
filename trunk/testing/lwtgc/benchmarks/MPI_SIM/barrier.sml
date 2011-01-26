structure Main = 
struct 
  open MLton
  open PCML 
 

  fun p () : string = (*Int.tostring(B.processorNumber())*)""
  fun print str = ()
  val np = Aux.numberOfProcessors

  val bCh = channel() 
  fun procs(bCh)  = (spawn(fn () => send(bCh,~1));()) 	
  
  fun barrier(n , np) =
	let
	fun loop1 i = 
		if i = 0 then ()
		else (print "Sending\n" ; spawn(fn()=>procs(bCh));loop1(i-1))
	 
	fun loop j = 
		if j = 0 then () 
		else (print "recv\n" ; recv (bCh);loop(j-1))
	
        fun loop2 n = 	
		if n = 0 then () 
		else ((let
			val _ = loop1 (np-1)
			val _ = loop (np-1)
			in () end);loop2 (n-1))
	
	val _  = loop2 n
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
          val () = barrier(n,4)
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
