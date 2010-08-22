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
      structure Assert = LocalAssert(val assert = true)
      structure GlobalDebug = Debug
      structure Debug = LocalDebug(val debug = false)
      structure Pointer = Primitive.MLton.Pointer

      open Critical

      structure T = MLtonThread
      structure TID = ThreadID
      structure SH = SchedulerHooks
      structure B = Basic
      structure Prim = Primitive.MLton.Threadlet
      structure Q = ImpQueue

      type thread_id = ThreadID.thread_id
      type primThread = Primitive.MLton.Thread.thread
      type threadlet = Primitive.MLton.Thread.thread

      datatype statType = datatype RepTypes.statType
      datatype thread = datatype RepTypes.thread
      datatype rdy_thread = datatype RepTypes.rdy_thread
      datatype thread_state = datatype RepTypes.thread_state

      type statRec = {preempt : int ref, bsend : int ref, brecv : int ref,
                      nbsend : int ref, nbrecv : int ref}
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

     val doesAsyncPreempt = true

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
          val TID.TID {state, next, statistics,...} = getCurThreadId ()
          val _ = print "\ngetThreadletState\n\tnext = "
          val _ = printPointerAtOffset (!next)
          val _ = case !state of
                       MAIN => print "\n\tstate = MAIN"
                     | ASYNC => print "\n\tstate = ASYNC"
          val _ = print "\n"
        in
          (!state, !next, !statistics)
        end

     fun setThreadletState (s, n, stat) =
        let
          val TID.TID {state, next, statistics, ...} = getCurThreadId ()
          val _ = state := s
          val _ = next := n
          val _ = statistics := stat
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

      fun getStats () =
       let
         val TID.TID {statistics, ...} = getCurThreadId ()
       in
         !statistics
       end

     fun setStats (stats) =
       let
         val TID.TID {statistics, ...} = getCurThreadId ()
       in
         statistics := stats
       end


      val outstrm = ref NONE

      (* global stats *)
      val gapreempt = ref 0
      val gpreempt = ref 0
      val gnbsend = ref 0
      val gnbrecv = ref 0
      val gbsend = ref 0
      val gbrecv = ref 0

      val sizeOfParasite = ref 0.0
      val copyCounter = ref 0

      fun statCopyParasite (size) =
      let
        val r = Real./(real (!copyCounter), real (!copyCounter + 1))
        val a = Real.*(r,  (!sizeOfParasite))
        val b = Real./(real size, real (!copyCounter + 1))
        val _ = sizeOfParasite := Real.+(a, b)
        val _ = copyCounter := !copyCounter + 1
      in
        ()
      end

      fun incStat (s) =
      let
        val _ = atomicBegin ()
        val (st as {preempt, bsend, brecv, nbsend, nbrecv}) = getStats ()
      in
        ((case s of
             APREEMPT => gapreempt := !gapreempt + 1
           | PREEMPT => (preempt := !preempt + 1; gpreempt := !gpreempt + 1)
           | BSEND => (bsend := !bsend + 1;gbsend := !gbsend + 1)
           | BRECV => (brecv := !brecv + 1; gbrecv := !gbrecv + 1)
           | NBSEND => (nbsend := !nbsend + 1; gnbsend := !gnbsend + 1)
           | NBRECV => (nbrecv := !nbrecv + 1; gnbrecv := !gnbrecv + 1));
           setStats (st);
           atomicEnd ())
      end


      fun atomicPrefixAndSwitchTo (thlet, stats) =
      let
        val state = getThreadletState ()
        val _ = setThreadletType (ASYNC)
        val _ = setStats (stats)
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

      fun writeMainThreadStats () =
      let
          val _ = atomicBegin ()
          val ({preempt, bsend, brecv, nbsend, nbrecv}) = getStats ()
          val i2s = Int.toString
          val str = concat ["M ", i2s(!preempt), " ", i2s(!bsend), " ", i2s(!brecv), " ", i2s(!nbsend), " ", i2s(!nbrecv), "\n"]
          val _ = TextIO.output (valOf (!outstrm), str)
          val _ = atomicEnd ()
      in
        ()
      end

      fun writeGlobalStats () =
      let
          val i2s = Int.toString
          val str' = "Global Stats\n"
          val str = concat [str',
                            "Parasite preempt = ", i2s (!gapreempt),
                            " preempt = ",i2s (!gpreempt),
                            " bsend = ",i2s (!gbsend),
                            " brecv = ",i2s (!gbrecv),
                            " nbsend = ", i2s (!gnbsend),
                            " nbrecv = ", i2s (!gnbrecv),
                            " avgParasiteSize = ", Real.toString (!sizeOfParasite),
                            "\n"]
          val _ = TextIO.output (valOf (!outstrm), str)
      in
        ()
      end

      fun async f =
      let
        fun doit () =
        let
          val _ = setNextPointer (getFrameBottomAsOffset ())
          val _ = f ()
          (* write stats to file *)
          val _ = atomicBegin ()
          val ({preempt, bsend, brecv, nbsend, nbrecv}) = getStats ()
          val i2s = Int.toString
          val str = concat ["P ", i2s(!preempt), " ", i2s(!bsend), " ", i2s(!brecv), " ", i2s(!nbsend), " ", i2s(!nbrecv), "\n"]
          val _ = TextIO.output (valOf (!outstrm), str)
          val _ = atomicEnd ()
        in
          noop () (* Needed to prevent inlining f () *)
        end
        val state = getThreadletState ()
        val _ = setThreadletType (ASYNC)
        val _ = setStats ({preempt = ref 0, nbsend = ref 0, bsend = ref 0,
                          nbrecv = ref 0, brecv = ref 0})
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
        val x = Q.deque (q)
        val _ = case x of
                     SOME _ => ignore (fetchAndAdd (numAsyncs , ~1))
                   | NONE => ()
      in
        x
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
          (!SH.pauseHook())
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
           val x = dequeA ()
           fun spliceAnM (a, stats, RTHRD (tid, t)) =
               let
                 val TID.TID {statistics, ...} = tid
                 val newT = T.new (wf (fn () => (T.atomicBegin ();
                                    atomicPrefixAndSwitchTo (a, stats);
                                    ignore (T.switch
                                             (fn _ => (setStats (!statistics)
                                                       ;t)));
                                    noop ())) tid)
                 val newRT = T.prepare (newT, ())
               in
                 RTHRD (tid, newRT)
               end
           val thrd' = case x of
                           NONE => thrd
                         | SOME (a, stats) => spliceAnM (a, stats, thrd)
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

      fun readyOnProc (thrd,procNum) = B.addWorkTo thrd procNum

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

      fun new (f : thread_id -> ('a -> unit)) : 'a thread =
         let
            val () = Assert.assertAtomic' ("Scheduler.new", NONE)
            val tid = TID.new ()
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
            val _ = incStat (PREEMPT)
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
                             val _ = incStat (APREEMPT)
                             val thlet = preemptAsync (t', getNextPointer ())
                             val _ = enqueA (thlet, getStats ())
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
