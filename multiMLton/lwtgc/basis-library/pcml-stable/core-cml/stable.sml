structure Stable : STABLE =
  struct

    type checkpoint =(
        unit GCAbleGraph.Node.t option ref
       )

    open Thread (* getTid *)
    open Channel (* mergeChannel *)

    val beginGlobalSync = _import "Proc_beginGlobalSync": unit -> unit;
    val endGlobalSync = _import "Proc_endGlobalSync": unit -> unit;

    fun stableCP(f:'a -> 'b, comp: unit -> unit): ('a -> 'b) *  unit GCAbleGraph.Node.t option ref  =
      let val nodeRef = ref NONE
          fun newFun(arg) =
            let val con = ref (fn () => ())
                val _ = MLtonCont.callcc(fn z =>
                                   (con:= (fn () =>
                                             (ignore(MLtonCont.throw(z, ()))
                                             ; ())
                                          ); ()
                                   )
                                 )
                val _ = StableGraph.schedThread (con, getTid())
                val _ = StableGraph.enterSS(getTid (), comp, nodeRef)
                val ret = f(arg)
                val _ = StableGraph.exitSS(getTid ())
            in ret
            end
      in (newFun, nodeRef)
      end

    fun stable(f) =
      let val (f, _(*cp*)) = stableCP(f, fn()=>())
      in f
      end

    fun stabilize'(node) =
      let val _ = MLtonThread.atomicBegin()
          val _ = beginGlobalSync ()
          val (threads2Restore, threads2Kill) =
            StableGraph.stabilizeGraph(Scheduler.getCurThreadId(), node)
          val _ = Scheduler.stabilizeQs(threads2Restore, threads2Kill)
          val _ = endGlobalSync ()
          val _ = MLtonThread.atomicEnd()
          val _ = StableGraph.debug("API - finished stabalize\n")
      in ()
      end

    fun stabilize() = (stabilize'(NONE); exit())
    fun stabilizeCP(cp) =
      case !cp
        of NONE => ()
         | SOME cp => stabilize'(SOME(cp))

    fun rebindReferences(arg) =
      let val a = !arg
          val b = fn() => arg:=a
          val tid = getTid()
      in StableGraph.addReference(tid, b)
      end

    val unmonitoredAssign = (op :=)
    fun monitoredAssign(r: 'a ref, l: 'a) = (print("new assign\n");rebindReferences(r); r:=l)

end
