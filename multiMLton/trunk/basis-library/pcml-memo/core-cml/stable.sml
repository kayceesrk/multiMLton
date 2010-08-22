structure Stable : STABLE =
  struct
    open Thread (* getTid *)
    open Channel (* mergeChannel *)
    structure Tid = ThreadID
    structure S = Scheduler
    structure C = MLtonCont
    structure T = Thread

    val DEBUG = false
    val MaxCache = 20
    fun debug(msg) =
      if DEBUG
      then print msg
      else ()
    fun addConstraint(f) =  (* manually add a constaint, useful for debugging
    			       this opperation is typically NOT safe*)
      let
          val (taking_memo, memo_cache) = (T.getTakingMemo (), T.getMemoCache ())
      	  val _ = if !taking_memo
	          then memo_cache := (f o !memo_cache)
		  else ();
      in ()
      end

    (* exceptions can be raised in one of two ways
       1: while capturing memo information
       	  we have 2 options
	     1: capture the memo normally and add a final constraint that throws the exception
	     2: consider the memo information captured thus far faulty for this call --- we opt for this below

       2: during regular control flow AFTER a failed constraints has throw to a continuations
       	  we must catch the exception, then throw to the correct continuations then reraise *)

    fun stable(f:'a -> 'b): ('a -> 'b) =
      let val memoCache = ref []
          val return = ref NONE
          val partialMem = ref false
	      val exnFn = ref NONE
          val cacheSize = ref 0
          val con = ref (fn x => (print(" I shouldnt see this! [STABLE]\n");x))
          val cpyF = fn h =>C.callcc(fn z =>(con:= (fn x => (ignore (C.throw(z, x)); x)); h))
          fun newFun (arg) =
            let
                val Tid.TID {taking_memo, memo_cache, ...} = getTid ()
                val memo_cache_reflist = memo_cache
                val _ = memo_cache_reflist := (ref (fn () => ()))::(!memo_cache_reflist)
                val memo' = !memoCache
                fun runSection() = f(arg)
		        fun findMemo(l)=
                  case l
                    of (a,r,cl)::xs =>
                       if(Primitive.MLton.equal(a,arg))
                       then (cpyF(); debug "\nUsing memo"; partialMem:= true; ignore (cl()); partialMem:= false; r)
                       else findMemo(xs)
                     | [] => let
                                 val _ = taking_memo := true
                                  val ret = runSection()
                                  val _ = if !partialMem
                                          then (return := (SOME ret); !con(); ())
                                          else
                                             let
                                               val  memo_cache::rest = (!memo_cache_reflist)
                                               val mem = !memo_cache
                                  	       	   val _ = taking_memo := false (* no nested memo capture right now*)
                                               val _ = memo_cache_reflist := rest
                                  		       val _ = if !cacheSize < MaxCache  (* insert cache flushing mechanism here *)
                                                           then (memoCache := ((arg, ret, mem)::memo');
                                                                 debug ("\nSaving memo "^(Int.toString(!cacheSize)));
                                                                 (case rest of
                                                                      [] => memo_cache_reflist := []
                                                                    | prev::tail => (prev := (mem o !prev);
                                                                                     memo_cache_reflist := (prev::tail)));
						                                         cacheSize := (!cacheSize +1))
						                                    else debug "\nNot using memo"
                                               in ()
                                               end

                              in ret
                              end
                 val ret =
                       if !partialMem
                       then let val _ = partialMem := false
		       	        val _ = case !exnFn
				          of SOME f => f() (*set partial Mem to false and raise exn,
					                     this puts us back in the handler*)
					   | NONE => ()
                                val ret = Option.valOf(!return)
                                val _ = return := NONE

                            in ret
                            end
                       else findMemo(memo')

             in ret
             end handle e => if !partialMem
                             then (exnFn := SOME(fn x => raise e); !con();raise e) (*in the handler but with wrong cont, throw to con*)
 			     else case !exnFn
				    of SOME f => (exnFn:= NONE; raise e)
                                     | NONE => let
                                                val taking_memo = T.getTakingMemo ()
                                                val _ = taking_memo := false
                                       	       	    (* val _ = memo_cache := (fn() => ()(*print "reset\n"*) *) (* clean up memo state *)
				               in raise e
					       end

      in (newFun)
      end

    val memoFix : (('a -> 'b) -> ('a -> 'b)) -> 'a -> 'b
        = fn (f) =>
                let
                  val r = ref NONE
                  fun mf x = case !r of
                                  NONE => let
                                            val g = (stable (f mf))
                                            val _ = r := SOME (g)
                                          in
                                            g (x)
                                          end
                                | SOME g => g (x)
                in
                  mf
                end
end
