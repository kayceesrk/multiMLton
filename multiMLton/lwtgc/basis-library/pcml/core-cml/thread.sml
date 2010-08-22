(* thread.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* thread.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure Thread : THREAD =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure S = Scheduler
      structure SH = SchedulerHooks
      structure B = Basic

      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg)

      open ThreadID

      val enablePreemption = Basic.enablePreemption
      val disablePreemption = Basic.disablePreemption

      val canMoveDeferredSpawns = _import "GC_canMoveDeferredSpawns": unit -> bool;

      fun generalExit (tid', clr') =
         let
            val () = Assert.assertNonAtomic' "Thread.generalExit"
            val () = debug' "generalExit" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Thread.generalExit"
            val () = S.atomicBegin ()
            val tid as TID {dead, props, ...} = S.getCurThreadId ()
            val () = Assert.assert ([], fn () =>
                                    concat ["Thread.generalExit ",
                                            Option.getOpt (Option.map tidToString tid', "NONE"),
                                            " <> ",
                                            tidToString tid], fn () =>
                                      case tid' of NONE => true
                                      | SOME tid' => sameTid (tid', tid))
            val () = if clr' then props := [] else ()
            val () = Event.atomicCVarSet dead

         in
           if S.finishWork () then
             S.atomicSwitch
             (fn t =>
             let
               val _ = debug' "Quiting"
               val _ = SH.pauseHook := (fn () => (ignore (B.return ()); !SH.pauseHook()))
             in
               S.prepFn (!SH.shutdownHook, fn () => (true, OS.Process.success))
             end)
           else
            S.atomicSwitchToNext
            (fn t => ())
         end

        (* fun doHandler (TID {exnHandler, ...}, exn) =
         (debug (fn () => concat ["Exception: ", exnName exn, " : ", exnMessage exn])
          ; ((!exnHandler) exn) handle _ => ()) *)

        fun doHandler (TID {exnHandler, ...}, exn) =
         (print (concat ["\nException: ", exnName exn, " : ", exnMessage exn])
          ; ignore (OS.Process.exit OS.Process.failure)
          ; ((!exnHandler) exn) handle _ => ())

      fun spawnc f x forceSame =
         let
            val () = S.atomicBegin ()
            fun thread tid () =
               ((f x) handle ex => doHandler (tid, ex)
                ; generalExit (SOME tid, false))
            val t = S.new thread
            val tid = S.getThreadId t
            val () = S.readySpawn (S.prep t) forceSame
            val () = S.atomicEnd ()
            val () = debug (fn () => concat ["spawnc ", tidToString tid])  (* NonAtomic *)
         in
            tid
         end

     fun wrapFunction f tid =
         let
            fun thread () =
               ((f ()) handle ex => doHandler (tid, ex)
                ; generalExit (SOME tid, false))
         in
            thread
         end

      val _ = S.wrapFunction := SOME (wrapFunction)

     (* forceSame is false. Hence, Spawns on different processors *)
      fun spawn f = spawnc f () false

      fun spawnOnCurProc f = spawnc f () true

      fun spawn' f =
      let
        (* XXXGC KC must be done at alrmHandler *)
        val _ =
          if canMoveDeferredSpawns () then
              let
                fun loop () =
                  case Deferred.getSpawn () of
                      SOME f => (ignore (spawn f);
                                 loop ())
                    | NONE => ()
              in
                loop ()
              end
          else ()
      in
        Deferred.deferSpawn f
      end


      fun joinEvt (TID{dead, ...}) = Event.cvarGetEvt dead

      val getTid = S.getCurThreadId

      fun exit () =
         let
            val () = Assert.assertNonAtomic' "Thread.exit"
            val () = debug' "exit" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Thread.exit"
         in
            generalExit (NONE, true)
         end

      fun yield () =
         let
            val () = Assert.assertNonAtomic' "Thread.yield"
            val () = debug' "yield" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Thread.yield"
         in
            S.readyAndSwitchToNext (fn () => ())
         end

      (* thread-local data *)
      local
         fun mkProp () =
            let
               exception E of 'a
               fun cons (a, l) = E a :: l
               fun peek [] = NONE
                 | peek (E a :: _) = SOME a
                 | peek (_ :: l) = peek l
               fun delete [] = []
                 | delete (E _ :: r) = r
                 | delete (x :: r) = x :: delete r
            in
               {cons = cons,
                peek = peek,
                delete = delete}
            end
         fun mkFlag () =
            let
               exception E
               fun peek [] = false
                 | peek (E :: _) = true
                 | peek (_ :: l) = peek l
               fun set (l, flg) =
                  let
                     fun set ([], _) = if flg then E::l else l
                       | set (E::r, xs) = if flg then l else List.revAppend(xs, r)
                       | set (x::r, xs) = set (r, x::xs)
                  in
                     set (l, [])
                  end
            in
               {set = set,
                peek = peek}
            end
         fun getProps () =
            let val TID {props, ...} = getTid ()
            in props
            end
      in
         fun newThreadProp (init : unit -> 'b) =
            let
               val {peek, cons, delete} = mkProp()
               fun peekFn () = peek(!(getProps()))
               fun getF () =
                  let val h = getProps()
                  in
                     case peek(!h) of
                        NONE => let val b = init()
                                in h := cons(b, !h); b
                                end
                      | (SOME b) => b
                  end
               fun clrF () =
                  let val h = getProps()
                  in h := delete(!h)
                  end
               fun setFn x =
                  let val h = getProps()
                  in h := cons(x, delete(!h))
                  end
            in
               {peekFn = peekFn,
                getFn = getF,
                clrFn = clrF,
                setFn = setFn}
            end

         fun newThreadFlag () =
            let
               val {peek, set} = mkFlag()
               fun getF ()= peek(!(getProps()))
               fun setF flg =
                  let val h = getProps()
                  in h := set(!h, flg)
                  end
            in
               {getFn = getF,
                setFn = setF}
            end
      end
   end
