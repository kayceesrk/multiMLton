(* scheduler.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* scheduler.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * This module implements the scheduling queues and preemption
 * mechanisms.
 *)

structure Scheduler : SCHEDULER =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure GlobalDebug = Debug
      structure Debug = LocalDebug(val debug = false)
      structure Pointer = Primitive.MLton.Pointer

      open Critical

      structure T = MLtonThread
      structure TID = ThreadID
      structure SH = SchedulerHooks
      structure G = StableGraph
      structure B = Basic
      structure Prim = Primitive.MLton.Threadlet
      structure Q = ImpQueue

      type thread_id = ThreadID.thread_id
      type primThread = Primitive.MLton.Thread.thread
      type threadlet = Primitive.MLton.Thread.thread

      datatype thread = datatype RepTypes.thread
      datatype rdy_thread = datatype RepTypes.rdy_thread
      datatype thread_state = datatype RepTypes.thread_state


      fun prep (THRD (tid, t)) = RTHRD (tid, T.prepare (t, ()))
      fun prepVal (THRD (tid, t), v) = RTHRD (tid, T.prepare (t, v))
      fun prepFn (THRD (tid, t), f) = RTHRD (tid, T.prepare (T.prepend (t, f), ()))

      val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;
      val printPointerAtOffset = _import "GC_printPointerAtOffset" : int -> unit;
      val preemptAsync = _import "GC_preemptAsync" : primThread * int -> threadlet;
      val getFrameBottomAsOffset = _import "GC_getFrameBottomAsOffset" : unit -> int;
      val noop = _import "GC_noop" : unit -> unit;

      (* the error thread.  This thread is used to trap attempts to run CML
       * without proper initialization (i.e., via RunCML).  This thread is
       * enqueued by reset.
       *)

     (* disable prints *)
     fun print str = ()
     fun printPointerAtOffset str = ()
     val numAsyncs = ref 0

     val doesAsyncPreempt = false

      val errorTid = TID.bogus "error"
      fun errorThrd () : unit thread =
         THRD (errorTid, T.new (fn () =>
               (GlobalDebug.sayDebug
                ([fn () => "CML"], fn () => "**** Use RunCML.doit to run CML ****")
                ; raise Fail "CML not initialized")))

      fun getThreadId (THRD (tid, _)) = tid
      val getCurThreadId = B.getCurThreadId
      fun tidMsg () = TID.tidToString (B.getCurThreadId ())
      fun tidNum () = TID.tidToInt (B.getCurThreadId ())

      fun debug msg = Debug.sayDebug ([atomicMsg, tidMsg], msg)
      fun debug' msg = debug (fn () => msg^" : "
                                    ^Int.toString(B.processorNumber()))
      fun debug'' msg = ()


      (* This is setup in thread.sml *)
      val wrapFunction = ref NONE

      (* contains preempted asyncs *)
      val asyncQ = Array.tabulate (B.numberOfProcessors, fn _ => Q.new ())

      fun getThreadletState () =
        let
          val TID.TID {state, next, ...} = getCurThreadId ()
          val _ = print "\ngetThreadletState\n\tnext = "
          val _ = printPointerAtOffset (!next)
          val _ = case !state of
                       MAIN => print "\n\tstate = MAIN"
                     | ASYNC => print "\n\tstate = ASYNC"
          val _ = print "\n"
        in
          (!state, !next)
        end

     fun setThreadletState (s, n) =
        let
          val TID.TID {state, next, ...} = getCurThreadId ()
          val _ = state := s
          val _ = next := n
          val _ = print "\nsetThreadletState\n\tnext = "
          val _ = printPointerAtOffset (n)
          val _ = case !state of
                       MAIN => print "\n\tstate = MAIN"
                     | ASYNC => print "\n\tstate = ASYNC"
          val _ = print "\n"
        in
          ()
        end


     fun getThreadletType () =
       let
         val TID.TID {state, ...} = getCurThreadId ()
       in
         !state
       end

     fun getNextPointer () =
       let
         val TID.TID {next, ...} = getCurThreadId ()
         val _ = print "\ngetNextPointer = "
         val _ = printPointerAtOffset (!next)
         val _ = print "\n"
       in
         !next
       end


     fun setThreadletType (s) =
       let
         val TID.TID {state, ...} = getCurThreadId ()
       in
         state := s
       end

     fun setNextPointer (p) =
       let
         val TID.TID {next, ...} = getCurThreadId ()
         val _ = print "\nsetNextPointer = "
         val _ = printPointerAtOffset (p)
         val _ = print "\n"
       in
         next := p
       end

      fun atomicPrefixAndSwitchTo (thlet) =
      let
        val state = getThreadletState ()
        val _ = setThreadletType (ASYNC)
        fun doit () =
        let
          val _ = setNextPointer (getFrameBottomAsOffset ())
          val _ = Prim.prefixAndSwitchTo (thlet) (* Implicit atomic End *)
        in
          print "\natomicPrefixAndSwitchTo : Should not see this"
        end
        val _ = Primitive.dontInline doit
        val _ = setThreadletState (state)
      in
        ()
      end


      fun async f =
      let
        fun doit () =
        let
          val _ = setNextPointer (getFrameBottomAsOffset ())
          val _ = f ()
        in
          noop () (* Needed to prevent inlining f () *)
        end
        val state = getThreadletState ()
        val _ = setThreadletType (ASYNC)
        val _ = Primitive.dontInline (doit)
        val _ = atomicBegin ()
        val _ = setThreadletState (state)
        val _ = atomicEnd ()
      in
        ()
      end

      fun enqueA x =
      let
        val _ = debug' "Enque async"
        val q = Array.sub (asyncQ, B.processorNumber ())
        val _ = Q.enque (q, x)
        val n = fetchAndAdd (numAsyncs, 1)
      in
        ()
      end

      fun dequeA () =
      let
        val _ = debug' "Deque async"
        val q = Array.sub (asyncQ, B.processorNumber ())
        val async = Q.deque (q)
        val _ = case async of
                     SOME _ => ignore (fetchAndAdd (numAsyncs , ~1))
                   | NONE => ()
      in
        async
      end

     fun emptyA () =
      let
        val q = Array.sub (asyncQ, B.processorNumber ())
      in
        Q.empty (q)
      end


      (* enqueue a thread in the primary queue *)
      fun enque1 thrd forceSame =
         (Assert.assertAtomic' ("Scheduler.enque1", NONE)
          ; B.addWork thrd forceSame)
      (* dequeue a thread from the primary queue *)
      fun deque1 () =
         (Assert.assertAtomic' ("Scheduler.deque1", NONE)
          ;  B.getWork ())

      fun next () =
        if B.empty () andalso emptyA () then
          (print "Empty\n"
          ; !SH.pauseHook())
        else
         (let
            val () = Assert.assertAtomic' ("Scheduler.next", NONE)
            val wf = valOf (!wrapFunction)
            val thrd =
               case deque1 () of
                  NONE => if (emptyA ()) then (!SH.pauseHook ())
                           else (* we are going to reify async into a thread *)
                           (let
                             (* This must be done before deque to avoid race
                              * causing premature termination of program *)
                             val _ = debug' "Reifying async"
                             val _ = fetchAndAdd (B.numThreadsLive, 1)
                             (* creating a container for async to run *)
                             val tid = TID.new ()
                             val nT = T.new (wf (fn () => debug' "Dummy thread") tid)
                             val nRt = T.prepare (nT, ())
                            in
                              RTHRD (tid, nRt)
                            end)
                | SOME thrd => thrd
           val async = dequeA ()
           fun spliceAnM (a, RTHRD (tid, t)) =
               let
                 val newT = T.new (wf (fn () => (T.atomicBegin ();
                                    atomicPrefixAndSwitchTo (a);
                                    ignore (T.switch (fn _ => t));
                                    noop ())) tid)
                 val newRT = T.prepare (newT, ())
               in
                 RTHRD (tid, newRT)
               end
           val thrd' = case async of
                           NONE => thrd
                         | SOME a => spliceAnM (a, thrd)
         in
           thrd'
         end)

      fun finishWork () =
        let
          val r = B.finishWork ()
          val  res =
            r andalso (!numAsyncs = 0)
          val _ = debug' ("FinishWork : NumAsyncs = "^(Int.toString(!numAsyncs)))
        in
          res
        end

      fun readySpawn thrd forceSame =
         let
            val () = Assert.assertAtomic' ("Scheduler.ready", NONE)
            val () = enque1 thrd forceSame
         in
            ()
         end

      fun ready thrd = readySpawn thrd true (* forceSame = true *)

      fun readyOnProc (thrd, procNum) = B.addWorkTo thrd procNum

      local
         fun atomicSwitchAux msg f =
            (Assert.assertAtomic (fn () => "Scheduler." ^ msg, NONE)
             ; T.atomicSwitch (fn t =>
                               let
                                  val tid = B.getCurThreadId ()
                                  val () = TID.mark tid
                                  val RTHRD (tid',t') = f (THRD (tid, t))
                                  val () = B.setCurThreadId tid'
                                  val () = debug' "Scheduler.atomicSwitchAux"
                               in
                                  t'
                               end))
      in
         fun atomicSwitch (f: 'a thread -> rdy_thread) =
            atomicSwitchAux "atomicSwitch" f
         fun switch (f: 'a thread -> rdy_thread) =
            (atomicBegin (); atomicSwitch f)
         fun atomicSwitchToNext (f: 'a thread -> unit) =
            atomicSwitchAux "atomicSwitchToNext" (fn thrd => (f thrd; next ()))
         fun switchToNext (f: 'a thread -> unit) =
            (atomicBegin (); atomicSwitchToNext f)
         fun atomicReadyAndSwitch (f: unit -> rdy_thread) =
            atomicSwitchAux "atomicReadyAndSwitch" (fn thrd => (ready (prep thrd); f ()))
         fun readyAndSwitch (f: unit -> rdy_thread) =
            (atomicBegin (); atomicReadyAndSwitch f)
         fun atomicReadyAndSwitchToNext (f: unit -> unit) =
            atomicSwitchAux "atomicReadyAndSwitchToNext" (fn thrd => (ready (prep thrd); f (); next ()))
         fun readyAndSwitchToNext (f: unit -> unit) =
            (atomicBegin (); atomicReadyAndSwitchToNext f)
      end

      fun stabilizeQs(threads, threads2Kill) =
        let val _ = T.atomicBegin()
	    (* assume the hashTable should be double the size of what to restore *)
	    (* Bad heuristic and should be optimized later. *)
	    val tableSize = 2 * List.length threads + 1
            val _ = G.debug("API - scheduler using HashTable of size=" ^ Int.toString tableSize ^ "\n")
	    val threadTable = HashTable.new (tableSize, TID.hashTid, TID.sameTid)
            val _ = () (*print("i have allocated my array\n")*)
(**ORIG
            val (_,rS) = G.getAndSet(restoreThreads)
**)
            val _ = () (*print("i have set my array\n")*)

	    fun addToRestoreTable (tid, t) = HashTable.put threadTable (tid, RTHRD(tid, t))

            fun processQ(p) =
              case B.getWorkFrom p
(**ORIG
                of SOME (RTHRD(tid, x)) => (rS(TID.toNum(tid), (true, (tid, x))); processQ(rdyQA))
**)
		of SOME (RTHRD(tid, x)) => (addToRestoreTable (tid, x); processQ(p))

                 | NONE =>  ()

(**ORIG
            fun checkQ(tid, t) = rS(TID.toNum(tid), (true, (tid, MT.prepare (t, ()))))
*)
	        fun checkQ(tid, t) = addToRestoreTable (tid, T.prepare (t, ()))

            val numThreadsLive = ref 0 (* Keeping track of numThreadsLive after restore *)

            fun processQNum (pnum) =
            let
              val _ = processQ (pnum)
              val _ = ()(* print(" i have processed my Qs\n")*)
              val _ = app (fn (tid, x) => checkQ(tid,x)) threads
              val _ =() (* print("API - about to process queues\n")*)
              val _ = G.debug ("API - about to remove threads2kill\n")
              val _ = G.debug ("API - threads2Kill len=" ^ Int.toString (List.length threads2Kill) ^ "\n")
              val _ = G.debug ("API - threadTable size before=" ^ Int.toString (HashTable.size threadTable) ^ "\n")
              fun rem tid = HashTable.remove threadTable tid
              val _ = List.app (fn tid => rem tid) threads2Kill
              val _ = G.debug ("API - threadTable size after =" ^ Int.toString (HashTable.size threadTable) ^ "\n")

              val _ = G.debug("API - about to enqueue new threads\n")
(**ORIG
            val rdyQ1' = G.fold(G.toVector(restoreThreads), [], fn(x,xs) => if (#1 x)
                                                                    then let val (tid, t) = #2 x
                                                                         in RTHRD(tid, t)::xs
                                                                         end
                                                                  else xs)
         val _ = app (fn thrd => enque1(thrd)) rdyQ1'
**)
	          val rdyQ1' = HashTable.getValues threadTable
              val _ = app (fn thrd => (let val RTHRD (x,_) = thrd in G.debug ("API - enqueue " ^ TID.tidToString x  ^ "\n") end
                                       ; readyOnProc(thrd, pnum)
                                       ; numThreadsLive := !numThreadsLive + 1)) rdyQ1'
            in
              ()
            end

            val _ = List.tabulate (B.numberOfProcessors, fn pnum => processQNum pnum)
            val _ = B.numThreadsLive := !numThreadsLive + 1 (* +1 since the current thread will be killed *)
            val _ = G.debug("API - numThreadsLive = "^(Int.toString(!numThreadsLive))^"\n")
            val _ = G.debug("API - finished scheduling\n")
            val _ = T.atomicEnd()
        in  ()
        end

      fun new (f : thread_id -> ('a -> unit)) : 'a thread =
         let
            val () = Assert.assertAtomic' ("Scheduler.new", NONE)
            val tid = TID.new ()
            val _ = G.spawnThread(getCurThreadId (), tid)
            val t = T.new (f tid)
         in
            THRD (tid, t)
         end

      (* Same as new except creates a bidi edge *)
      fun new2 f =
        let
            val () = Assert.assertAtomic' ("Scheduler.new", NONE)
                val _ = ()(*print "I do not handle this right here\n"*)
            val tid = TID.new ()
                val _ = G.spawnThread2(getCurThreadId (), tid)
            val t = T.new (f tid)
        in
            THRD (tid, t)
        end



      fun prepend (thrd : 'a thread, f : 'b -> 'a) : 'b thread =
         let
            val () = Assert.assertAtomic' ("Scheduler.prepend", NONE)
            val THRD (tid, t) = thrd
            val t = T.prepend (t, f)
         in
            THRD (tid, t)
         end

      fun unwrap (f : rdy_thread -> rdy_thread) (t: T.Runnable.t) : T.Runnable.t =
         let
            val () = Assert.assertAtomic' ("Scheduler.unwrap", NONE)
            val state = getThreadletType ()
            val t' = case (state, doesAsyncPreempt) of
                         (MAIN, true) =>
                            let
                              val tid = B.getCurThreadId ()
                              val RTHRD (tid', t') = f (RTHRD (tid, t))
                              val () = B.setCurThreadId tid'
                            in
                              t'
                            end
                       | (ASYNC, true) =>
                           let
                             val t' = T.toPrimitive t
                             val thlet = preemptAsync (t', getNextPointer ())
                             val _ = enqueA thlet
                             val t'' = T.fromPrimitive t'
                           in
                             t''
                           end
                        | _ =>
                            let
                              val tid = B.getCurThreadId ()
                              val RTHRD (tid', t') = f (RTHRD (tid, t))
                              val () = B.setCurThreadId tid'
                            in
                              t'
                            end

         in
            t'
         end


      (* reset various pieces of state *)
      fun reset running =
         (atomicBegin ()
          ; if running then debug' "Scheduler.reset true"
            else  debug' "Scheduler.reset false"
          ; B.clearWork ()
          ; if not running then ready (prep (errorThrd ())) else ()
          ; atomicEnd ())
      (* what to do at a preemption (with the current thread) *)
      fun preempt (thrd) =
         let
            val () = Assert.assertAtomic' ("Scheduler.preempt", NONE)
            val () = debug'' "Scheduler.preempt" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Scheduler.preempt", SOME 1)
            val () = B.addWorkTo thrd (B.processorNumber())
         in
            ()
         end

      val _ = reset false
   end
