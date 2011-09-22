structure SyncVar : SYNC_VAR_EXTRA =
struct
  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Critical

  structure S = Scheduler
  structure L = Lock
  structure Q = ImpQueue
  structure TID = ThreadID
  structure E = Event
  structure PT= ProtoThread

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => msg()^"."^(PT.getThreadTypeString())
                                   ^" : "^Int.toString(PacmlFFI.processorNumber()))

  datatype 'a cell =
      CELL of {prio  : int ref,
              readQ : (int ref * 'a S.thread * (unit -> unit)) Q.t,
              value : 'a option ref,
              lock  : L.cmlLock}

  type 'a ivar = 'a cell
  type 'a mvar = 'a cell

  exception Put

  val cas = PacmlFFI.vCompareAndSwap

  fun newCell () = CELL {prio = ref 0, readQ = Q.new(), value = ref NONE,
    lock = L.initCmlLock ()}

  (* sameCell : ('a cell * 'a cell) -> bool *)
  fun sameCell (CELL {prio = prio1, ...}, CELL {prio = prio2, ...}) =
      prio1 = prio2

  (* bump a priority value by one, returning the old value *)
  fun bumpPriority (p) =
    PacmlFFI.fetchAndAdd (p, 1)

  (* functions to clean channel input and output queues *)
  local
      fun cleaner (txid, _, _) =
        if (!txid = 2) then true else false
  in
      fun cleanAndDeque q =
        Q.dequeLazyClean (q, cleaner)
      fun enqueAndClean (q, item) =
        (Q.cleanSuffix (q,cleaner);
        Q.enque (q, item))
  end

  fun relayMsg (readQ, msg, lock) =
    let
      val readyList = ref []
      fun tryLp () =
        case cleanAndDeque (readQ) of
              SOME (txid, rt, doSwap) =>
                (let
                  val () = debug' (fn () => "SyncVar.relayMsg.tryLp")
                  fun matchLp () =
                    (let
                      val () = debug' (fn () => "SyncVar.relayMsg.matchLp")
                      val res = cas (txid, 0, 2)
                     in
                      if res = 0 then
                        let
                          val _ = readyList := ((ProtoThread.prepVal (rt, msg))::(!readyList))
                          val _ = doSwap ()
                        in
                          tryLp ()
                        end
                      else if res = 1 then matchLp () (* CLAIMED *)
                      else tryLp () (* SYNCHED *)
                    end) (* matchLp ends *)
                in
                  matchLp ()
                end) (* SOME ends *)
            | NONE =>
                let
                  val () = debug' (fn () => "SyncVar.relayMsg.NONE")
                  val rdyLst = !readyList
                  val _ = L.releaseCmlLock lock PT.getLockId
                in
                  ignore (List.map (fn (rthrd) => (S.atomicReady (rthrd); atomicBegin())) rdyLst)
                end
       (* tryLp ends *)
    in
      tryLp ()
    end (* relayMSg ends *)

  fun gPut (name, CELL {prio, readQ, value, lock}, x) =
    let
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name])
      val () = debug' (fn () =>  concat [name, "(1)"]) (* NonAtomic *)
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(1)"])
      val () = atomicBegin()
      val () = L.getCmlLock lock PT.getLockId
      val () = debug' (fn () =>  concat [name, "(2)"]) (* Atomic 1 *)
      val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
      val () =
          case !value of
            NONE =>
                let
                  val () = debug' (fn () =>  concat [name, "(3.1.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.1.1)"], SOME 1)
                  val () = value := SOME x
                  val () = prio := 1
                  (* Implicitly releases lock *)
                  val () = relayMsg (readQ, x, lock)
                  val () = atomicEnd ()
                  val () = debug' (fn () =>  concat [name, "(3.1.2)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.1.2)"])
                in
                  ()
                end
          | SOME _ =>
                let
                  val () = debug' (fn () =>  concat [name, "(3.2.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                  val () = L.releaseCmlLock lock PT.getLockId
                  val () = atomicEnd ()
                  val () = debug' (fn () =>  concat [name, "(3.2.2)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
                in
                  raise Put
                end
      val () = debug' (fn () =>  concat [name, "(4)"]) (* NonAtomic *)
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(4)"])
    in
      ()
    end

  (* Swap the current contents of the cell with a new value *)
  fun gSwap (name, doSwap, CELL {prio, readQ, value, lock}) =
    let
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, ""])
      val () = debug' (fn () =>  concat [name, "(1)"]) (* NonAtomic *)
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(1)"])
      val () = atomicBegin()
      val () = L.getCmlLock lock PT.getLockId
      val () = debug' (fn () =>  concat [name, "(2)"]) (* Atomic 1 *)
      val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
      val msg =
          case !value of
            NONE =>
                let
                  val () = debug' (fn () =>  concat [name, "(3.2.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                  val msg =
                      S.atomicSwitchToNext
                      (fn rt => (enqueAndClean (readQ, (ref 0, rt, fn () => doSwap value));
                                 L.releaseCmlLock lock PT.getLockId))
                  val () = debug' (fn () =>  concat [name, "(3.2.3)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.3)"])
                in
                  msg
                end
          | SOME x =>
                let
                  val () = debug' (fn () =>  concat [name, "(3.2.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                  val () = prio := 1
                  val () = doSwap value
                  val () = L.releaseCmlLock lock PT.getLockId
                  val () = atomicEnd ()
                  val () = debug' (fn () =>  concat [name, "(3.2.2)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
                in
                  x
                end
      val () = debug' (fn () =>  concat [name, "(4)"]) (* NonAtomic *)
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(4)"])
    in
      msg
    end

  fun gSwapEvt (name, doSwap, CELL{prio, readQ, value, lock}) =
    let
      fun doitFn () =
        let
          val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, ".doitFn"], NONE)
          val () = debug' (fn () =>  concat [name, "(3.2.1)"]) (* Atomic 1 *)
          val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
          val () = L.getCmlLock lock PT.getLockId
          val x =
            case !value of
                  NONE => (L.releaseCmlLock lock PT.getLockId;
                           raise RepTypes.DOIT_FAIL)
                | SOME x => let
                              val () = prio := 1
                              val () = doSwap value
                              val () = L.releaseCmlLock lock PT.getLockId
                              val () = atomicEnd ()
                            in
                              x
                            end
          val () = debug' (fn () =>  concat [name, "(3.2.2)"]) (* NonAtomic/Atomic 1 *)
          val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
        in
          x
        end
      fun blockFn (mytxid) =
        let
          val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, ".blockFn"], NONE)
          val () = debug' (fn () =>  concat [name, "(3.2.1)"]) (* Atomic 1 *)
          val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
          val () = L.getCmlLock lock PT.getLockId
          val msg =
            case !value of
                NONE => let
                          val () = debug' (fn () =>  concat [name, "(3.2.2).tryLp.NONE"]) (* Atomic 1 *)
                          val msg = S.atomicSwitchToNext
                                    (fn rt =>
                                      (enqueAndClean (readQ, (mytxid, rt, fn () => doSwap value))
                                      ; L.releaseCmlLock lock PT.getLockId))
                          val () = atomicBegin ()
                          val () = Thread.reifyCurrentIfParasite ()
                        in
                          msg
                        end
              | SOME x =>
                  let
                    val () = debug' (fn () =>  concat [name, "(3.2.2).tryLp.SOME"]) (* Atomic 1 *)
                    val msg =
                      let
                        fun matchLp () =
                          let
                            val res = cas (mytxid, 0, 2)
                          in
                            if res = 0 then
                              (prio := 1
                              ; L.releaseCmlLock lock PT.getLockId
                              ; Thread.reifyCurrentIfParasite ()
                              ; x)
                            else if res = 1 then matchLp ()
                            else (L.releaseCmlLock lock PT.getLockId
                                  ; S.atomicSwitchToNext (fn _ => ()))
                          end
                      in
                        matchLp ()
                      end
                  in
                    msg
                  end
          val () = debug' (fn () =>  concat [name, "(3.2.3)"]) (* NonAtomic *)
          val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.3)"])
        in
          msg
        end
      fun pollFn () =
        let
          val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, ".pollFn"], NONE)
          val () = debug' (fn () =>  concat [name, "(2)"]) (* Atomic 1 *)
          val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
        in
          case !value of
              NONE => E.blocked blockFn
            | SOME _ => E.enabled {prio = bumpPriority prio, doitFn = doitFn}
        end
    in
      E.bevt pollFn
    end


  fun gSwapPoll (name, doSwap, CELL{prio, value, lock, ...}) =
    let
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, ""])
      val () = debug' (fn () =>  concat [name, "(1)"]) (* NonAtomic *)
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(1)"])
      val () = atomicBegin()
      val () = L.getCmlLock lock PT.getLockId
      val () = debug' (fn () =>  concat [name, "(2)"]) (* Atomic 1 *)
      val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(2)"], SOME 1)
      val msg =
          case !value of
            NONE =>
                let
                  val () = debug' (fn () =>  concat [name, "(3.1.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                  val msg = NONE
                  val () = debug' (fn () =>  concat [name, "(3.1.2)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"], SOME 1)
                  val () = L.releaseCmlLock lock PT.getLockId
                  val () = atomicEnd ()
                  val () = debug' (fn () =>  concat [name, "(3.1.3)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.3)"])
                in
                  msg
                end
          | SOME x =>
                let
                  val () = debug' (fn () =>  concat [name, "(3.2.1)"]) (* Atomic 1 *)
                  val () = Assert.assertAtomic (fn () => concat ["SyncVar.", name, "(3.2.1)"], SOME 1)
                  val () = prio := 1
                  val () = doSwap value
                  val () = L.releaseCmlLock lock PT.getLockId
                  val () = atomicEnd ()
                  val () = debug' (fn () =>  concat [name, "(3.2.2)"]) (* NonAtomic *)
                  val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(3.2.2)"])
                in
                  SOME x
                end
      val () = debug' (fn () =>  concat [name, "(4)"]) (* NonAtomic *)
      val () = Assert.assertNonAtomic (fn () => concat ["SyncVar.", name, "(4)"])
    in
      msg
    end



  (** I-variables **)

  val iVar = newCell
  val sameIVar = sameCell

  fun iPut (cell, x) = gPut ("iPut", cell, x)
  local fun doGetSwap _ = ()
  in
      fun iGet cell = gSwap ("iGet", doGetSwap, cell)
      fun iGetEvt cell = gSwapEvt ("iGetEvt", doGetSwap, cell)
      fun iGetPoll cell = gSwapPoll ("iGetPoll", doGetSwap, cell)
  end

  (** M-variables **)

  val mVar = newCell
  fun mVarInit x = CELL {prio = ref 0, readQ = Q.new(), value = ref (SOME x), lock = L.initCmlLock ()}
  val sameMVar = sameCell

  fun mPut (cell, x) = gPut ("mPut", cell, x)
  local fun doTakeSwap value = value := NONE
  in
      fun mTake cell = gSwap ("mTake", doTakeSwap, cell)
      fun mTakeEvt cell = gSwapEvt ("mTakeEvt", doTakeSwap, cell)
      fun mTakePoll cell = gSwapPoll ("mTakePoll", doTakeSwap, cell)
  end
  local fun doGetSwap _ = ()
  in
      fun mGet cell = gSwap ("mGet", doGetSwap, cell)
      fun mGetEvt cell = gSwapEvt ("mGetEvt", doGetSwap, cell)
      fun mGetPoll cell = gSwapPoll ("mGetPoll", doGetSwap, cell)
  end
  local fun doSwapSwap x value = value := SOME x
  in
      fun mSwap (cell, x) = gSwap ("mSwap", doSwapSwap x, cell)
      fun mSwapEvt (cell, x) = gSwapEvt ("mSwap", doSwapSwap x, cell)
  end


end
