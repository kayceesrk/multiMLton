(* run-cml.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* run-cml-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure MLtonRunPCML : MLTON_RUN_PCML =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure R = Running
      structure B = Basic
      structure S = Scheduler
      structure SH = SchedulerHooks
      structure TID = ThreadID
      structure TO = TimeOut
      structure T = MLtonThread

      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg^" : "
                                    ^Int.toString(B.processorNumber()))


      local
         structure Signal = MLtonSignal

         fun getAlrmHandler () =
            Signal.getHandler Posix.Signal.alrm
         fun setAlrmHandler h =
            Signal.setHandler (Posix.Signal.alrm, h)

      in
         fun prepareAlrmHandler tq =
            let
               val origAlrmHandler = getAlrmHandler ()
            in
               (* XXX kc dont set timer here. Timer is set globlally *)
               (fn alrmHandler =>
                setAlrmHandler (Signal.Handler.handler (S.unwrap alrmHandler)),
                fn () => setAlrmHandler origAlrmHandler)
            end
      end

      fun isRunning () = !Running.isRunning


      fun reset running =
          (S.reset running
          ; SH.reset ()
          ; TID.reset ()
          ; TO.reset ())

      fun alrmHandler thrd =
         (let
            val () = Assert.assertAtomic' ("RunCML.alrmHandler", NONE)
            val () = debug' "alrmHandler" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("RunCML.alrmHandler", SOME 1)
            val () = S.preempt thrd
            val () = if (B.processorNumber () = 0) then ignore (TO.preempt ())
                     else ()
            val nextThrd = S.next()
         in
            nextThrd
         end)

      (* Note that SH.pauseHook is only invoked by S.next
       * when there are no threads on the ready queue;
       * Furthermore, note that alrmHandler always
       * enqueues the preepted thread (via S.preempt).
       * Hence, the ready queue is never empty
       * at the S.next in alrmHandler.  Therefore,
       * pauseHook is never run within alrmHandler.
       *)
      fun pauseHook () =
         let
            val () = Assert.assertAtomic' ("RunCML.pauseHook", SOME 1)
            val () = if not (isRunning ()) then (S.atomicEnd ();ignore (B.return ())) else ()
            val to = TO.preempt ()
            val () = debug' ("pauseHook ")
         in
          S.next ()
         end

      fun doit (initialProc: unit -> unit,
                tq: Time.time option,
                filename : string) =
         let
            val () =
               if isRunning ()
                  then raise Fail "CML is running"
                  else ()
            val (installAlrmHandler, _) = prepareAlrmHandler tq
            val ((*cleanUp*)_, status) =
               S.switchToNext
               (fn thrd =>
                let
                   val () = reset true
                   val () = B.enablePreemption ()
                   val () = S.outstrm := SOME (TextIO.openOut filename)
                   val () = SH.shutdownHook := S.prepend (thrd, fn arg => (S.atomicBegin (); arg))
                   val () = SH.pauseHook := pauseHook
                   val () = ignore (Thread.spawn (fn ()=> (Running.isRunning := true;initialProc ())))
                   val () = installAlrmHandler alrmHandler
                in
                   ()
                end)
            val () = reset false
            val () = Running.isRunning := false
           val () = S.writeGlobalStats ()
           val () = TextIO.closeOut (valOf(!S.outstrm))
            val () = S.atomicEnd ()
         in
            status
         end

      fun shutdown status =
         if isRunning ()
            then S.switch (fn _ => S.prepVal (!SH.shutdownHook, (true, status)))
            else raise Fail "CML is not running"
   end
