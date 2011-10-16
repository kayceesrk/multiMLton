structure Event : EVENT_EXTRA =
struct
  structure Assert = LocalAssert (val assert = false)
  structure Debug = LocalDebug (val debug = false)

  open Critical
  structure S = Scheduler
  structure L = Lock
  structure PT = ProtoThread
  structure TID = ThreadID

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => (msg())^"."^(PT.getThreadTypeString())
                                   ^" : "^Int.toString(PacmlFFI.processorNumber()))

  datatype status = datatype RepTypes.status

  val enabled = ENABLED
  val blocked = BLOCKED

  type 'a base = 'a RepTypes.base

  datatype sevt = datatype RepTypes.sevt
  datatype aevt = datatype RepTypes.aevt
  datatype cevt = datatype RepTypes.cevt

  type rdy_thread = RepTypes.rdy_thread

  val bevt = fn pollFn => BEVT [pollFn]

  (* Used as a marker for type of event *)
  datatype event_type = ASYNC | SYNC
  datatype 'a doit_result = RETRY | ASYNC_DONE | SYNC_DONE of 'a

  datatype thread = datatype RepTypes.thread


  datatype 'a group =
      BASE of 'a base list

  fun wrapForDoit (doit : unit -> 'a, evt) : 'a doit_result =
    case evt of
          SYNC => (SYNC_DONE (doit ()) handle RepTypes.DOIT_FAIL => RETRY)
        | ASYNC => (PT.spawnParasite (fn () => ignore (doit ()))
                   ; ASYNC_DONE) handle RepTypes.DOIT_FAIL => RETRY

  val never : 'a sevt = BEVT []

  fun alwaysEvt (v : 'a) : 'a sevt =
    let
      fun doitFn () =
          let
            val () = Assert.assertAtomic' ("Event.alwaysEvt.doitFn", NONE)
            val () = debug' (fn () => "alwaysEvt(3.1)") (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Event.alwaysEvt(3.1)", SOME 1)
            val () = atomicEnd ()
            val () = debug' (fn () => "alwaysEvt(3.2)") (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Event.alwaysEvt(3.2)"
          in
            v
          end
      fun pollFn () =
          let
            val () = Assert.assertAtomic' ("Event.alwaysEvt.pollFn", NONE)
            val () = debug' (fn () => "alwaysEvt(2)") (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Event.alwaysEvt(2)", SOME 1)
          in
            enabled {prio = ~1, doitFn = doitFn}
          end
    in
      bevt pollFn
    end


  val aevt = fn s => PAIR (alwaysEvt (), s)

  fun wrap (evt : 'a sevt, wfn : 'a -> 'b) : 'b sevt =
    let
      fun wrapF f x = wfn (f x)
      fun wrapBaseEvt pollFn () =
          case pollFn () of
            ENABLED {prio, doitFn} =>
                ENABLED {prio = prio, doitFn = wrapF doitFn}
          | BLOCKED blockFn =>
                BLOCKED (wrapF blockFn)
      fun wrap' evt =
          case evt of
            BEVT bevts =>
                BEVT(List.map wrapBaseEvt bevts)
          | CHOOSE evts =>
                CHOOSE(List.map wrap' evts)
          | GUARD g =>
                GUARD(fn () => wrap (g (), wfn))
    in
      wrap' evt
    end

  fun sWrap (PAIR (evt1, evt2) : ('a, 'b) aevt, wfn : 'a -> 'c) : ('c, 'b) aevt =
        PAIR (wrap (evt1, wfn), evt2)

  fun aWrap (PAIR (evt1, evt2) : ('a, 'b) aevt, wfn : 'b -> 'c) : ('a, 'c) aevt =
        PAIR (evt1, wrap (evt2, wfn))

  fun wrapHandler (evt : 'a sevt, hfn : exn -> 'a) : 'a sevt =
    let
      fun wrapF f x = (f x) handle exn => hfn exn
      fun wrapBaseEvt pollFn () =
          case pollFn () of
            ENABLED {prio, doitFn} =>
                ENABLED {prio = prio, doitFn = wrapF doitFn}
          | BLOCKED blockFn =>
                BLOCKED (wrapF blockFn)
      fun wrap' evt =
          case evt of
            BEVT bevts =>
                BEVT(List.map wrapBaseEvt bevts)
          | CHOOSE evts =>
                CHOOSE(List.map wrap' evts)
          | GUARD g =>
                GUARD(fn () => wrapHandler (g (), hfn))
    in
      wrap' evt
    end

  fun sWrapHandler (PAIR (evt1, evt2) : ('a, 'b) aevt, hfn : exn -> 'a) : ('a, 'b) aevt =
    PAIR (wrapHandler (evt1, hfn), evt2)

  fun aWrapHandler (PAIR (evt1, evt2) : ('a, 'b) aevt, hfn : exn -> 'b) : ('a, 'b) aevt =
    PAIR (evt1, wrapHandler (evt2, hfn))

  val guard = GUARD

  fun choose (evts : 'a sevt list) : 'a sevt =
    let
      val () = Assert.assertNonAtomic' "Event.choose"
      val () = debug' (fn ()=> "choose(1)") (* NonAtomic *)
      val () = Assert.assertNonAtomic' "Event.choose(1)"
      fun gatherBEvts (evts, bevts') =
          case (evts, bevts') of
            ([], bevts') => BEVT bevts'
          | ((BEVT bevts)::evts, bevts') => gatherBEvts (evts, bevts @ bevts')
          | (evts, []) => gather (evts, [])
          | (evts, bevts') => gather (evts, [BEVT bevts'])
      and gather (evts, evts') =
          case (evts, evts') of
            ([], [evt']) => evt'
          | ([], evts') => CHOOSE evts'
          | ((CHOOSE cevts)::evts, evts') =>
                gather (evts, cevts @ evts')
          | ((BEVT [])::evts, evts') =>
                gather (evts, evts')
          | ((BEVT bevts)::evts, (BEVT bevts')::evts') =>
                gather (evts, BEVT (bevts @ bevts')::evts')
          | (evt::evts, evts') =>
                gather (evts, evt::evts')
      val evt = gatherBEvts (List.rev evts, [])
    in
      evt
    end

  local
      val cnt = ref 0
      fun random i =
        let val j = !cnt
        in
            if j = 1000000 then cnt := 0 else cnt := j + 1
            ; Int.rem (j, i)
        end
  in
  fun selectDoitFn (doitFns : {prio : int, doitFn : 'a} list) : 'a =
      let
        val () = Assert.assertAtomic' ("Event.selectDoitFn", NONE)
        val () = debug' (fn () => "selectDoitFn(2)") (* Atomic 1 *)
        val () = Assert.assertAtomic' ("Event.selectDoitFn(2)", SOME 1)
      in
        case doitFns of
            [{doitFn, ...}] => doitFn
          | doitFns =>
              let
              fun select (doitFns, maxP,
                          doitFnsMaxP, numMaxP,
                          doitFnsFixed, numFixed) =
                case doitFns of
                    [] => (case (doitFnsMaxP, doitFnsFixed) of
                              ([doitFn], []) => doitFn
                            | ([], [doitFn]) => doitFn
                            | (doitFnsMaxP, doitFnsFixed) =>
                                let
                                    val bias = 2
                                    val num = numFixed + bias * numMaxP
                                    val k = random num
                                in
                                    if k < numFixed
                                      then List.nth (doitFnsFixed, k)
                                      else List.nth (doitFnsMaxP,
                                                      Int.mod(k - numFixed, numMaxP))
                                end)
                  | {prio, doitFn}::doitFns =>
                      if prio = ~1
                          then select(doitFns, maxP,
                                      doitFnsMaxP, numMaxP,
                                      doitFn::doitFnsFixed, numFixed + 1)
                          else if prio > maxP
                                  then select(doitFns, prio,
                                              [doitFn], 1,
                                              doitFnsFixed, numFixed)
                                  else if prio = maxP
                                          then select(doitFns, maxP,
                                                      doitFn::doitFnsMaxP, numMaxP + 1,
                                                      doitFnsFixed, numFixed)
                                          else select(doitFns, maxP,
                                                      doitFnsMaxP, numMaxP,
                                                      doitFnsFixed, numFixed)
              in
                  select (doitFns, 0, [], 0, [], 0)
              end
      end
  end

  fun syncTypeToString (et) =
    case et of
         SYNC => "SYNC"
       | ASYNC => "ASYNC"

  fun syncOnBEvt (pollFn : 'a base, et : event_type) : 'a option=
    let
      val () = Assert.assertNonAtomic' "Event.syncOnBEvt"
      val () = debug' (fn () => "syncOnBEvt(1): SyncType : "^(syncTypeToString(et))) (* NonAtomic *)
      val () = Assert.assertNonAtomic' "Event.syncOnBEvt(1)"
      val () = atomicBegin ()
      val () = debug' (fn () => "syncOnBEvt(2)") (* Atomic 1 *)
      val () = Assert.assertAtomic' ("Event.syncOnBEvt(2)", SOME 1)
      fun get () =
          case pollFn () of
              (* doitFn may not always be successful
                * returns false if unsuccessful *)
              ENABLED {doitFn, ...} =>
              let
                val x : 'a option =
                  (case (wrapForDoit (doitFn, et)) of
                        RETRY => get ()
                      | SYNC_DONE w => SOME w
                      | ASYNC_DONE => NONE)
              in
                x
              end
            | BLOCKED blockFn =>
                let
                  val transId = ref 0
                in (case et of
                        SYNC => SOME (blockFn (transId))
                      | ASYNC => (PT.spawnParasite (fn () => ignore (blockFn (transId)));
                                  NONE))
                end
      val x = get ()
      val () = debug' (fn () => "syncOnBEvt(4)") (* NonAtomic *)
      val () = Assert.assertNonAtomic' "Event.syncOnBEvt(4)"
    in
      x
    end

  (* this function handles the case of synchronizing on a list of
    * base events (w/o any negative acknowledgements).   It also handles
    * the case of syncrhonizing on NEVER.
    *)
  fun syncOnBEvts (bevts' : 'a base list, et : event_type) : 'a option =
    let
      val () = Assert.assertNonAtomic' "Event.syncOnBEvts"
      val () = debug' (fn () => "syncOnBEvts(1) : SyncType : "^(syncTypeToString (et))) (* NonAtomic *)
      val () = Assert.assertNonAtomic' "Event.syncOnBEvts(1)"
      fun ext (bevts, blockFns) =
          let
            val () = debug' (fn () => "syncOnBEvts(2).ext") (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Event.syncOnBEvts(2).ext", SOME 1)
          in
            case bevts of
                [] =>
                let
                  val () = debug' (fn () => "syncOnBEvts(2).ext([])") (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.syncOnBEvts(2).ext([])", SOME 1)
                  val transId = ref 0
                  fun blockHelper () =
                    S.atomicSwitch (fn t=>
                      let
                        val _ = Assert.assertAtomic (fn () => "Event.syncOnBEvts blockHelper", SOME 1)
                        fun loop (blockFns) =
                          case blockFns of
                               [] => (case et of
                                          ASYNC => (S.ready (PT.prepVal (t, NONE)))
                                        | SYNC => ())
                             | blockFn::blockFns =>
                                 let
                                   val _ = PT.spawnParasite (fn () =>
                                                              let
                                                                val _ = atomicBegin ()
                                                                val x = blockFn(transId)
                                                                val _ = Assert.assertNonAtomic (fn () => "Event.syncOnBEvts.blockFn returned")
                                                                val _ = debug' (fn () => "Event.syncOnBEvts.blockFn returned")
                                                              in
                                                                case et of
                                                                     ASYNC => ()
                                                                   | SYNC => (S.ready (PT.prepVal (t, SOME x)))
                                                              end)
                                 in
                                   if (!transId = 0) then
                                     loop (blockFns)
                                   else
                                     ()
                                 end
                      in
                        Thread.createHost (fn () => loop (blockFns))
                      end)
                in
                  blockHelper ()
                end
              | pollFn::bevts =>
                  (case pollFn () of
                      ENABLED doitFn => extRdy (bevts, [doitFn])
                    | BLOCKED blockFn => ext (bevts, blockFn::blockFns))
          end
      and extRdy (bevts, doitFns) =
          let
            val () = debug' (fn () => "syncOnBEvts(2).extRdy") (* Atomic 1*)
            val () = Assert.assertAtomic' ("Event.syncOnBEvts(2).extRdy", SOME 1)
          in
            case bevts of
                [] =>
                  let
                    (* Checks every doitFn for satisfiability. If such a function is
                      * not found in the list, sync From Beginning. This is inefficient.
                      * We should have blocked in this case. But since we do not have blockFn
                      * for every event, we have to repeat it. Hopefully, we are not unlucky in
                      * every round (Find a enabled event but find it claimed when we actually
                      * sync on it). But If we find all events are blocked in a round, we block
                      * anyway *)
                    fun indexList lst = List.tabulate (List.length lst, fn i =>
                                          let
                                            val {doitFn, prio} = List.nth(lst, i)
                                          in
                                            {doitFn = (doitFn,i), prio = prio}
                                          end
                                          )
                    val idxdDoitFns = indexList doitFns
                    fun doSomeDoit (arg) =
                      if List.length(arg) = 0 then
                        ext (bevts', [])
                      else
                        let
                          val (doitFn, i) = selectDoitFn arg
                        in
                          (case (wrapForDoit (doitFn, et)) of
                              RETRY =>
                              let
                                fun filter ({doitFn, prio}, acc) =
                                  let
                                    val (doitFn, j) = doitFn
                                  in
                                    if i=j then acc else {doitFn=doitFn, prio=prio}::acc
                                  end
                                val filteredDoitFns = foldl filter [] arg
                                val idxdDoitFns = indexList filteredDoitFns
                              in
                                doSomeDoit idxdDoitFns
                              end
                            | SYNC_DONE w => SOME w
                            | ASYNC_DONE => NONE)
                        end
                        (* doSomeDoit() ends *)
                  in
                    doSomeDoit idxdDoitFns
                  end
                  (* case [] end *)
              | pollFn::bevts =>
                  (case pollFn () of
                      ENABLED doitFn => extRdy (bevts, doitFn::doitFns)
                    | _ => extRdy (bevts, doitFns))
          end
      val x =
          case bevts' of
            [] => S.switchToNext (fn _ => ())
          | [bevt] => syncOnBEvt (bevt, et)
          | bevts => (atomicBegin (); ext (bevts, []))
      val () = debug' (fn () => "syncOnBEvts(4)") (* NonAtomic *)
      val () = Assert.assertNonAtomic' "Event.syncOnBEvts(4)"
    in
      x
    end

    local
        (* force the evaluation of any guards in an event collection,
        * returning an event group.
        *)
        fun forceBL (evts : 'a sevt list, bevts : 'a base list) : 'a group =
          case evts of
              [] => BASE bevts
            | evt::evts =>
                (case force evt of
                    BASE bevts' => forceBL (evts, bevts' @ bevts))
        and force (evt : 'a sevt) : 'a group =
          let
              val gevt =
                case evt of
                    BEVT bevts => BASE bevts
                  | CHOOSE evts => forceBL (evts, [])
                  | GUARD g => force (g ())
          in
              gevt
          end
    in
        fun forceHelper (e, t) =
          (case force e of
            BASE bevts => syncOnBEvts (bevts, t))

        fun eventTypeToString (et) =
          case et of
               SEVT _ => "SEVT"
             | AEVT _ => "AEVT"

        fun sync (con_evt : ('a,'b) cevt) : 'a =
          let
              val () = Assert.assertNonAtomic' "Event.sync"
              val () = debug' (fn () => "sync(1) : EventType : "^(eventTypeToString (con_evt))) (* NonAtomic *)
              val () = Assert.assertNonAtomic' "Event.sync(1)"
              val x = case con_evt of
                        SEVT evt => forceHelper (evt, SYNC)
                      | AEVT (PAIR (syncHalf, asyncHalf)) =>
                          let
                            val () = ignore (forceHelper (asyncHalf, ASYNC))
                          in
                            forceHelper (syncHalf, SYNC)
                          end
              val () = debug' (fn () => "sync(4)") (* NonAtomic *)
              val () = Assert.assertNonAtomic' "Event.sync(4)"
          in
            valOf (x)
          end

        fun aSync (evt : ('a,'b) aevt) : 'a = sync (AEVT (evt))
        fun sSync (evt : 'a sevt) : 'a = sync (SEVT(evt))

        fun select (evts : 'a sevt list) : 'a =
          let
              val () = Assert.assertNonAtomic' "Event.select"
              val () = debug' (fn () => "select(1)") (* NonAtomic *)
              val () = Assert.assertNonAtomic' "Event.select(1)"
              val x =
                case forceBL (evts, []) of
                    BASE bevts => syncOnBEvts (bevts, SYNC)
              val () = debug' (fn () => "select(4)") (* NonAtomic *)
              val () = Assert.assertNonAtomic' "Event.select(4)"
          in
            valOf (x)
          end

      (* XXX KC atrans polls the a's pollFn. Make it poll b's pollFn *)
      fun aTrans (PAIR (a,b) : ('a, 'b) aevt) : 'a sevt =
        let
          fun atomicATransF e =
            (atomicEnd (); (ignore o forceHelper) (b, ASYNC); atomicBegin (); e)
          fun aTransF e =
            ((ignore o forceHelper) (b, ASYNC); e)
          fun aTransBaseEvt pollFn () =
              case pollFn () of
                ENABLED {prio, doitFn} =>
                    ENABLED {prio = prio, doitFn = atomicATransF doitFn}
              | BLOCKED blockFn =>
                    BLOCKED (atomicATransF blockFn)
          fun aTrans' evt =
              case evt of
                BEVT bevts =>
                    BEVT(List.map aTransBaseEvt bevts)
              | CHOOSE evts =>
                    CHOOSE(List.map aTrans' evts)
              | GUARD g =>
                    GUARD(fn () => aTransF (g ()))
        in
          aTrans' a
        end

      fun sTrans (e) = aevt (e)
    end
end
