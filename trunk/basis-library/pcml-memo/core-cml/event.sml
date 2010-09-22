(* event.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* event.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of event values and the event combinators.
 *
 * Some important requirements on the implementation of base event values:
 *
 *  1)  The pollFn, doitFn, and blockFn are always called from inside
 *      atomic regions.
 *
 *  2)  The pollFn returns an integer priority: this is 0 when not enabled,
 *      ~1 for fixed priority, and a positive value for dynamic priority.
 *      The standard scheme is to associate a counter with the underlying
 *      synchronization object, and to increase it by one for each
 *      synchronization attempt.
 *
 *  3)  The blockFn is responsible for exiting the atomic region; the doitFns
 *      should NOT leave the atomic region.
 *
 *  4)  The blockFn is responsible for executing the "cleanUp" action
 *      prior to leaving the atomic region.
 *)

 (* KC : dummy cleanups are used if there are no negative acks involved.
 * Haven't ported nacks *)
structure Event : EVENT_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      structure S = Scheduler
      structure L = Lock
      structure B = Basic

      fun debug msg = Debug.sayDebug ([S.atomicMsg, S.tidMsg], msg)
      fun debug' msg = debug (fn () => msg^" : "
                                    ^Int.toString(B.processorNumber()))

      datatype trans_id = datatype TransID.trans_id
      datatype trans_id_state = datatype TransID.trans_id_state
      datatype cvar = datatype CVar.cvar
      datatype cvar_state = datatype CVar.cvar_state


      datatype status = datatype RepTypes.status
      val enabled = ENABLED
      val blocked = BLOCKED

      type 'a base = 'a RepTypes.base

      datatype event = datatype RepTypes.event
      val bevt = fn pollFn => BEVT [pollFn]

      datatype 'a group =
         BASE of 'a base list
       | GRP of 'a group list
       | NACK of cvar * 'a group


      (** Condition variables.  Because these variables are set inside atomic
       ** regions, we have to use different conventions for clean-up, etc.
       ** Instead of requiring the blockFn continuation to call the cleanUp
       ** action and to leave the atomic region, we call the cleanUp function
       ** when setting the condition variable (in atomicCVarSet), and have the
       ** invariant that the blockFn continuation is dispatched outside the
       ** atomic region.
       **)

      (* set a condition variable;
       * we assume that this function is always executed in an atomic region.
       *)
      fun atomicCVarSet (CVAR (state, lock)) : unit =
         let
            val () = Assert.assertAtomic' ("Event.atomicCVarSet", NONE)
            val () = debug' "atomicCVarSet" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Event.atomicCVarSet", SOME 1)
            val () = L.getCmlLock lock (S.tidNum())
           val res =
            (case !state of
               CVAR_unset waiting =>
                  let
                     fun add waiting : unit =
                        case waiting of
                           [] => ()
                         | ({transId as TXID {txst, cas}, cleanUp: unit -> unit, thread, procNum}::waiting) =>
                             let
                               fun matchLp  () =
                              (case cas (txst, WAITING,SYNCHED) of
                                  SYNCHED => ()
                                | WAITING =>
                                     ( cleanUp ();
                                       S.readyOnProc (thread, procNum))
                                | CLAIMED => matchLp ()
                               ; add waiting)
                             in
                               matchLp ()
                             end
                  in
                     state := CVAR_set 1
                     ; add waiting
                  end
             | _ =>  raise Fail "atomicCVarSet")
           val () = L.releaseCmlLock lock (S.tidNum())
         in
           res
         end

      (* the event constructor for waiting on a cvar.
       *)
      fun cvarGetEvt (CVAR (state, lock)) : unit event =
         let
            fun doitFn () =
               let
                  val () = Assert.assertAtomic' ("Event.cvarGetEvt.doitFn", NONE)
                  val () = debug' "cvarGetEvt(3.1.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.cvarGetEvt(3.1.1)", SOME 1)
                  val () = L.getCmlLock lock (S.tidNum())
                  val res =
                    case !state of
                         CVAR_set _ => (state := CVAR_set 1;
                                        L.releaseCmlLock lock (S.tidNum ());
                                        S.atomicEnd ();
                                        SOME ())
                       | _ => (L.releaseCmlLock lock (S.tidNum ())
                                ;NONE)
                  val () = debug' "cvarGetEvt(3.1.2)" (* NonAtomic/Atomic 1 *)
                  val () = case res of
                                SOME _ => Assert.assertNonAtomic' "Event.cvarGetEvt(3.1.2)"
                              | _ => Assert.assertAtomic' ("Event.cvarGetEvt(3.1.2)", SOME 1)
               in
                  res
               end
            fun blockFn {transId as TXID {txst, cas}, cleanUp, next} =
               let
                  val () = Assert.assertAtomic' ("Event.cvarGetEvt.blockFn", NONE)
                  val () = debug' "cvarGetEvt(3.2.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.cvarGetEvt(3.2.1)", SOME 1)
                  val () = L.getCmlLock lock (S.tidNum())
                  val () =
                     (case !state of
                          CVAR_unset _ =>
                                S.atomicSwitch
                                (fn t =>
                                  let
                                    val item = {transId = transId,
                                                cleanUp = cleanUp,
                                                thread = S.prep t,
                                                procNum = Basic.processorNumber ()}
                                    val waiting =
                                        case !state of
                                          CVAR_unset waiting => waiting
                                        | _ => raise Fail "cvarGetEvt:blockFn"
                                  in
                                    state := CVAR_unset (item::waiting)
                                    ; L.releaseCmlLock lock (S.tidNum ())
                                    ; next ()
                                  end)
                       | CVAR_set _ => let
                                         fun matchLp () =
                                         (case cas (txst, WAITING, CLAIMED) of
                                            (* This event is still not synched *)
                                            WAITING => (state := (CVAR_set 1);
                                                        txst := SYNCHED;
                                                        L.releaseCmlLock lock (S.tidNum ());
                                                        S.atomicEnd ())
                                            (* In timeEvt *)
                                          | CLAIMED => matchLp ()
                                            (* This event was synched *)
                                          | SYNCHED => (L.releaseCmlLock lock (S.tidNum ());
                                                    S.atomicSwitchToNext (fn _ => ())))
                                       in
                                         matchLp ()
                                       end
                     )
                  val () = debug' "cvarGetEvt(3.2.2)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Event.cvarGetEvt(3.2.2)"
               in
                  ()
               end
            fun pollFn () =
               let
                  val () = Assert.assertAtomic' ("Event.cvarGetEvt.pollFn", NONE)
                  val () = debug' "cvarGetEvt(2)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.cvarGetEvt(2)", SOME 1)
                 val () = L.getCmlLock lock (S.tidNum())
                 val res =
                  case !state of
                     CVAR_set n =>
                        (state := CVAR_set (n + 1)
                         ; enabled {prio = n, doitFn = doitFn})
                   | _ => blocked blockFn
                 val () = L.releaseCmlLock lock (S.tidNum ())
               in
                 res
               end
         in
            bevt pollFn
         end


      (* event combinators *)
      val never : 'a event =
         BEVT []
      fun alwaysEvt (v : 'a) : 'a event =
         let
            fun doitFn () =
               let
                  val () = Assert.assertAtomic' ("Event.alwaysEvt.doitFn", NONE)
                  val () = debug' "alwaysEvt(3.1)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.alwaysEvt(3.1)", SOME 1)
                  val () = S.atomicEnd ()
                  val () = debug' "alwaysEvt(3.2)" (* NonAtomic *)
                  val () = Assert.assertNonAtomic' "Event.alwaysEvt(3.2)"
               in
                  SOME v
               end
            fun pollFn () =
               let
                  val () = Assert.assertAtomic' ("Event.alwaysEvt.pollFn", NONE)
                  val () = debug' "alwaysEvt(2)" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.alwaysEvt(2)", SOME 1)
               in
                  enabled {prio = ~1, doitFn = doitFn}
               end
         in
            bevt pollFn
         end

      fun wrap (evt : 'a event, wfn : 'a -> 'b) : 'b event =
         let
            fun wrapF f x = wfn (f x)
            fun wrapDoitF f x = case(f x) of
                                     NONE => NONE
                                   | SOME w => SOME (wfn w)
            fun wrapBaseEvt pollFn () =
               case pollFn () of
                  ENABLED {prio, doitFn} =>
                     ENABLED {prio = prio, doitFn = wrapDoitF doitFn}
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
                | WNACK f =>
                     WNACK(fn evt => wrap (f evt, wfn))
         in
            wrap' evt
         end
      fun wrapHandler (evt : 'a event, hfn : exn -> 'a) : 'a event =
         let
            fun wrapF f x = (f x) handle exn => hfn exn
            fun wrapDoitF f x = (f x) handle exn => SOME (hfn exn)
            fun wrapBaseEvt pollFn () =
               case pollFn () of
                  ENABLED {prio, doitFn} =>
                     ENABLED {prio = prio, doitFn = wrapDoitF doitFn}
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
                | WNACK f =>
                     WNACK(fn evt => wrapHandler (f evt, hfn))
         in
            wrap' evt
         end

      val guard = GUARD
      val withNack = WNACK

      fun choose (evts : 'a event list) : 'a event =
         let
            val () = Assert.assertNonAtomic' "Event.choose"
            val () = debug' "choose(1)" (* NonAtomic *)
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
            val () = debug' "selectDoitFn(2)" (* Atomic 1 *)
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

      fun syncOnBEvt (pollFn : 'a base) : 'a =
         let
            val () = Assert.assertNonAtomic' "Event.syncOnBEvt"
            val () = debug' "syncOnBEvt(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Event.syncOnBEvt(1)"
            val () = S.atomicBegin ()
            val () = debug' "syncOnBEvt(2)" (* Atomic 1 *)
            val () = Assert.assertAtomic' ("Event.syncOnBEvt(2)", SOME 1)
            fun get () : 'a =
               case pollFn () of
                    (* doitFn may not always be successful
                     * returns false if unsuccessful *)
                    ENABLED {doitFn, ...} =>
                        (case doitFn () of
                             NONE => get ()
                           | SOME w => w)
                  | BLOCKED blockFn =>
                     let
                       val transId = TransID.mkFlg ()
                     in blockFn {transId = transId,
                                 cleanUp = fn () => (),
                                 next = S.next}
                     end
            val x : 'a = get ()
            val () = debug' "syncOnBEvt(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Event.syncOnBEvt(4)"
         in
            x
         end

      (* this function handles the case of synchronizing on a list of
       * base events (w/o any negative acknowledgements).   It also handles
       * the case of syncrhonizing on NEVER.
       *)
      fun syncOnBEvts (bevts' : 'a base list) : 'a =
         let
            val () = Assert.assertNonAtomic' "Event.syncOnBEvts"
            val () = debug' "syncOnBEvts(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Event.syncOnBEvts(1)"
            fun ext (bevts, blockFns) : 'a =
               let
                  val () = debug' "syncOnBEvts(2).ext" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.syncOnBEvts(2).ext", SOME 1)
               in
                  case bevts of
                     [] =>
                        let
                           val () = debug' "syncOnBEvts(2).ext([])" (* Atomic 1 *)
                           val () = Assert.assertAtomic' ("Event.syncOnBEvts(2).ext([])", SOME 1)
                        in
                           S.atomicSwitch
                           (fn (t : 'a S.thread) =>
                            let
                               val _ = debug' "test1"
                               val transId = TransID.mkFlg ()
                               fun log blockFns : S.rdy_thread =
                                  let
                                     val () = debug' "syncOnBEvts(2).ext([]).log" (* Atomic 1 *)
                                     val () = Assert.assertAtomic' ("Event.syncOnBEvts(2).ext([]).log", SOME 1)
                                  in
                                     case blockFns of
                                        [] => S.next ()
                                      | blockFn::blockFns =>
                                           (S.prep o S.new)
                                           (fn _ => fn () =>
                                            let
                                               val () = S.atomicBegin ()
                                               val x = blockFn {transId = transId,
                                                                cleanUp = fn () => (),
                                                                next = fn () => log blockFns}
                                            in S.switch(fn _ => S.prepVal (t, x))
                                            end)
                                  end
                            in
                               log blockFns
                            end)
                        end
                   | pollFn::bevts =>
                        (case pollFn () of
                            ENABLED doitFn => extRdy (bevts, [doitFn])
                          | BLOCKED blockFn => ext (bevts, blockFn::blockFns))
               end
            and extRdy (bevts, doitFns) : 'a =
               let
                  val () = debug' "syncOnBEvts(2).extRdy" (* Atomic 1*)
                  val () = Assert.assertAtomic' ("Event.syncOnBEvts(2).extRdy", SOME 1)
               in
                  case bevts of
                     [] =>
                        let
                          (* XXX KC *)
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
                                (case doitFn () of
                                    NONE =>
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
                                  | SOME w => w)
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
                | [bevt] => syncOnBEvt bevt
                | bevts => (S.atomicBegin (); ext (bevts, []))
            val () = debug' "syncOnBEvts(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Event.syncOnBEvts(4)"
         in
            x
         end

      (* walk the event group tree, collecting the base events (with associated
       * ack flags), and a list of flag sets.  A flag set is a (cvar * ack flag list)
       * pair, where the flags are those associated with the events covered by the
       * nack cvar.
       *)
      type ack_flg = bool ref
      type ack_flgs = ack_flg list
      type 'a back = 'a base * ack_flg
      type 'a backs = 'a back list
      type flg_set = cvar * ack_flg list
      type flg_sets = flg_set list
      fun collect (gevt : 'a group) : 'a backs * flg_sets =
         let
            fun gatherWrapped (gevt : 'a group, backs : 'a backs, flgSets : flg_sets) :
                              'a backs * flg_sets =
               let
                  fun gather (gevt : 'a group, backs : 'a backs,
                              ackFlgs : ack_flgs, flgSets : flg_sets) :
                             'a backs * ack_flgs * flg_sets =
                     case gevt of
                        BASE bevts =>
                           let
                              fun append (bevts, backs, ackFlgs) =
                                 case bevts of
                                    [] => (backs, ackFlgs)
                                  | bevt::bevts =>
                                       let val ackFlg = ref false
                                       in append (bevts, (bevt, ackFlg)::backs, ackFlg::ackFlgs)
                                       end
                              val (backs', ackFlgs') = append (bevts, backs, ackFlgs)
                           in
                              (backs', ackFlgs', flgSets)
                           end
                      | GRP gevt =>
                           let
                              fun f (gevt', (backs', ackFlgs', flgSets')) =
                                 gather (gevt', backs', ackFlgs', flgSets')
                           in List.foldl f (backs, ackFlgs, flgSets) gevt
                           end
                      | NACK (cvar, gevt) =>
                           let
                              val (backs', ackFlgs', flgSets') =
                                 gather (gevt, backs, [], flgSets)
                           in
                              (backs', ackFlgs' @ ackFlgs, (cvar, ackFlgs')::flgSets')
                           end
                  val (backs, _, flgSets) = gather (gevt, backs, [], flgSets)
               in
                  (backs, flgSets)
               end
         in
            case gevt of
               GRP _ =>
                  let
                     val ackFlg = ref false
                     fun gather (gevt : 'a group, backs : 'a backs, flgSets : flg_sets) :
                                'a backs * flg_sets =
                        case gevt of
                           BASE bevts =>
                              let
                                 fun append (bevts, backs) =
                                    case bevts of
                                       [] => backs
                                     | bevt::bevts => append (bevts, (bevt, ackFlg)::backs)
                              in
                                 (append (bevts, backs), flgSets)
                              end
                         | GRP gevt =>
                              let
                                 fun f (gevt', (backs', flgSets')) =
                                    gather(gevt', backs', flgSets')
                              in List.foldl f (backs, flgSets) gevt
                              end
                         | NACK _ =>
                              gatherWrapped (gevt, backs, flgSets)
                  in
                     gather (gevt, [], [])
                  end
             | gevt => gatherWrapped (gevt, [], [])
         end

      (* this function handles the more complicated case of synchronization
       * on groups of events where negative acknowledgements are involved.
       *)
      fun syncOnGrp (gevt : 'a group) : 'a =
         let
            val _ = raise Fail "syncOnGrp : Not implemented"
            val () = Assert.assertNonAtomic' "Event.syncOnGrp"
            val () = debug' "syncOnGrp(1)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Event.syncOnGrp(1)"
            val (backs, flgSets) = collect gevt
            fun chkCVars () =
               let
                  val () = debug' "syncOnGrp.chkCVars" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.syncOnGrp.chkCVars", SOME 1)
                  (* chkCVar checks the flags of a flag set.
                   * If they are all false, then the corresponding cvar
                   * is set to signal the negative ack.
                   *)
                  fun chkCVar (cvar, flgs) =
                     if List.exists ! flgs
                        then ()
                        else atomicCVarSet cvar
               in
                  List.app chkCVar flgSets
               end
            fun ext (backs, blockFns) : 'a =
               let
                  val () = debug' "syncOnGrp(2).ext" (* Atomic 1 *)
                  val () = Assert.assertAtomic' ("Event.syncOnGrp(2).ext", SOME 1)
               in
                  case backs of
                     [] =>
                        let
                           val () = debug' "syncOnGrp(2).ext([])" (* Atomic 1 *)
                           val () = Assert.assertAtomic' ("Event.syncOnGrp(2).ext([])", SOME 1)
                        in
                           S.atomicSwitch
                           (fn (t : 'a S.thread) =>
                            let
                               val _ = debug' "\ntest2"
                               val transId = TransID.mkFlg ()
                               val cleanUp = fn flg => fn () =>
                                  ( flg := true
                                   ; chkCVars ())
                               fun log blockFns : S.rdy_thread =
                                  let
                                     val () = debug' "syncOnGrp(2).ext([]).log" (* Atomic 1 *)
                                     val () = Assert.assertAtomic' ("Event.syncOnGrp(2).ext([]).log", SOME 1)
                                  in
                                     case blockFns of
                                        [] => S.next ()
                                      | (blockFn,ackFlg)::blockFns =>
                                           (S.prep o S.new)
                                           (fn _ => fn () =>
                                            let
                                               val () = S.atomicBegin ()
                                               val x = blockFn {transId = transId,
                                                                cleanUp = cleanUp ackFlg,
                                                                next = fn () => log blockFns}
                                            in S.switch(fn _ => S.prepVal (t, x))
                                            end)
                                  end
                            in
                               log blockFns
                            end)
                        end
                   | (pollFn,ackFlg)::backs =>
                        (case pollFn () of
                            ENABLED {prio, doitFn} =>
                               extRdy (backs, [{prio = prio,doitFn = (doitFn, ackFlg)}])
                          | BLOCKED blockFn => ext (backs, (blockFn,ackFlg)::blockFns))
               end
            and extRdy (backs, doitFns) : 'a =
               let
                  val () = debug' "syncOnGrp.extRdy(2)" (* Atomic 1*)
                  val () = Assert.assertAtomic' ("Event.syncOnGrp.extRdy(2)", SOME 1)
               in
                  case backs of
                     [] => let
                              val (doitFn, flg) = selectDoitFn doitFns
                           in
                              (flg := true
                              ; chkCVars ()
                              ; (case doitFn () of
                                     NONE => raise Fail "KC: not implemented"
                                   | SOME w => w))
                           end
                   | (pollFn,ackFlg)::backs =>
                           (case pollFn () of
                               ENABLED {prio, doitFn} =>
                                  extRdy (backs, {prio = prio, doitFn = (doitFn, ackFlg)}::doitFns)
                             | _ => extRdy (backs, doitFns))
               end
            val x = (S.atomicBegin (); ext (backs, []))
            val () = debug' "syncOnGrp(4)" (* NonAtomic *)
            val () = Assert.assertNonAtomic' "Event.syncOnGrp(4)"
         in
            x
         end

      local
         (* force the evaluation of any guards in an event collection,
          * returning an event group.
          *)
         fun forceBL (evts : 'a event list, bevts : 'a base list) : 'a group =
            case evts of
               [] => BASE bevts
             | evt::evts =>
                  (case force evt of
                      BASE bevts' => forceBL (evts, bevts' @ bevts)
                    | GRP gevts => forceGL (evts, if List.null bevts then gevts else gevts @ [BASE bevts])
                    | gevt => forceGL (evts, if List.null bevts then [gevt] else [gevt, BASE bevts]))
         and forceGL (evts : 'a event list, gevts : 'a group list) : 'a group =
            case (evts, gevts) of
               ([], [gevt]) => gevt
             | ([], gevts) => GRP gevts
             | (evt::evts, gevts) =>
                  (case (force evt, gevts) of
                      (BASE [], gevts) =>
                         forceGL (evts, gevts)
                    | (BASE bevts', (BASE bevts)::gevts) =>
                         forceGL (evts, BASE (bevts' @ bevts)::gevts)
                    | (GRP gevts', gevts) =>
                         forceGL (evts, gevts' @ gevts)
                    | (gevt, gevts) =>
                         forceGL (evts, gevt::gevts))
         and force (evt : 'a event) : 'a group =
            let
               val gevt =
                  case evt of
                     BEVT bevts => BASE bevts
                   | CHOOSE evts => forceBL (evts, [])
                   | GUARD g => force (g ())
                   | WNACK f =>
                        let val cvar = CVar.new ()
                        in NACK(cvar, force (f (cvarGetEvt cvar)))
                        end
            in
               gevt
            end
      in
         fun sync evt =
            let
               val () = Assert.assertNonAtomic' "Event.sync"
               val () = debug' "sync(1)" (* NonAtomic *)
               val () = Assert.assertNonAtomic' "Event.sync(1)"
               val x =
                  case force evt of
                     BASE bevts => syncOnBEvts bevts
                   | gevt => syncOnGrp gevt
               val () = debug' "sync(4)" (* NonAtomic *)
               val () = Assert.assertNonAtomic' "Event.sync(4)"
            in
               x
            end
         fun select (evts : 'a event list) : 'a =
            let
               val () = Assert.assertNonAtomic' "Event.select"
               val () = debug' "select(1)" (* NonAtomic *)
               val () = Assert.assertNonAtomic' "Event.select(1)"
               val x =
                  case forceBL (evts, []) of
                     BASE bevts => syncOnBEvts bevts
                   | gevt => syncOnGrp gevt
               val () = debug' "select(4)" (* NonAtomic *)
               val () = Assert.assertNonAtomic' "Event.select(4)"
            in
               x
            end
      end
   end
