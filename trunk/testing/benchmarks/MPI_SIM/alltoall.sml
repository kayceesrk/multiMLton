structure Main = 
struct 
  open MLton
  open PCML 
 

  fun p () : string = (*Int.tostring(B.processorNumber())*)""
  fun print str = ()
  val np1 = Aux.numberOfProcessors
  
  fun procs(id,clst,np) = 
	let
	fun loopS i = 
		if i = 0 then () 
		else (print ("\n"^Int.toString(id)^" Send..");spawn(fn ()=> send(List.nth(clst,i),0));loopS(i-1))  
 	
	fun loopR j =	
		if j = 0 then ()
		else (recv(List.nth(clst,id));print ("\n"^Int.toString(id)^" Recv..");loopR(j-1))
	
	val _ = loopS (np-1)
	val _ = spawn(fn () => loopR(np-1)) 
	in () end 
		
  fun alltoall (n,np) = 
	let 
	val clst = List.tabulate(np, fn(x)=> let val ch = channel() in ch end)
        fun loop i =
		if i = 0 then ()
		else (print("\n"^Int.toString(i)^" slave id...");spawn(fn () => procs(i,clst,np));loop(i-1))
	
	fun loop1 j =
		if j = 0 then ()
		else (loop(np-1);loop1(j-1))
	val _ = loop1(n)  
	in () end 	 
  
			
   fun doit n =
   let
     val _ = print "\nIn doit"
   in
      RunPCML.doit
      (fn () =>
       let
          val () = alltoall(n,np1)
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
