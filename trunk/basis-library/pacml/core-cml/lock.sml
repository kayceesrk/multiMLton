structure Lock :> LOCK =
struct

  open Critical
  structure TID = ThreadID

  datatype rdy_thread = datatype RepTypes.rdy_thread

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => (msg())^" : "^Int.toString(PacmlFFI.processorNumber()))

  val pN = PacmlFFI.processorNumber
  val vCas = PacmlFFI.vCompareAndSwap

  (* quickly try for some iterations before failing *)
  val cas = fn (r, v1, v2) =>
            let
              fun loop i =
                if (i=0) then
                  vCas (r, v1, v2)
                else if (!r = v1) then
                  (if (vCas (r, v1, v2) = v1) then
                    v1
                  else loop (i-1))
                else loop (i-1)
            in
              loop (100)
            end

  type cmlLock = RepTypes.cmlLock

  exception UnlockError of string

  fun initCmlLock () =
    RepTypes.LOCK {state = ref 0,
                   lockId = ref ~1,
                   count = ref 0,
                   que = CirQueue.new ()}

  val FREE = 0
  val LOCKED = 1
  val CLAIMED = 2

  fun yieldForLock (q, state) =
  let
    val _ = TID.mark (TID.getCurThreadId ())
    val lockId = TID.getLockId ()
    val atomicState = getAtomicState ()
    val () = setAtomicState (1)
    val () = Scheduler.atomicSwitchToNextHostForWB
              (fn rt =>
                let
                  val rt' = SOME rt
                  val rt' = if (Primitive.Lwtgc.isObjptrInSharedHeap q) then
                              PacmlPrim.move (rt', false, true)
                            else
                              rt'
                  val _ = CirQueue.enque (q, rt')
                  val _ = state := LOCKED
                  (* val _ = debug' (fn () => "yieldForLock(2) "^(TID.tidToString t)) *)
                in
                  ()
                end)
    val () = setAtomicState (atomicState)
    (* val _ = debug' (fn () => "yieldForLock(3)") *)
    val lockId' = TID.getLockId ()
    (* val _ = Assert.assert' ("yieldForLock: lockIds dont match ("
                            ^(Int.toString lockId)^", "^(Int.toString lockId')^")"
                            , fn () => (lockId = lockId')) *)
  in
      ()
  end

  fun getCmlLock (l as RepTypes.LOCK {state, lockId, count, que}) flockId =
  let
    val t = flockId () (* Has to be this way to account for parasite reification at maybePreempt *)
    fun enque () =
    let
      val res = cas (state, LOCKED, CLAIMED)
    in
      if res = LOCKED then
        ( (* debug' (fn () => concat["yieldForLock(1): Lock held by ", Int.toString (!lockId)]) ; *)
        yieldForLock (que, state)
        ; getCmlLock l flockId)
      else if res = CLAIMED then
        ((*print "enque CLAIMED\n";*)
         PacmlFFI.maybeWaitForGC ();
         enque ())
      else (* res = FREE *)
        getCmlLock l flockId
    end
  in
    if (!lockId = t) then
      count := !count + 1
    else
      let
        val res = cas (state, FREE, LOCKED)
      in
        if res = FREE then
          (* We got the lock *)
          lockId := t
        else (* res = LOCKED orelse res = CLAIMED *)
          enque ()
      end
  end

  fun releaseCmlLock (l as RepTypes.LOCK {state, lockId, count, que}) flockId =
  let
    val t = flockId ()
  in
    if (!lockId = t) andalso (!count > 0) then
      count := !count - 1
    else
      let
        val res = cas (state, LOCKED, CLAIMED)
      in
        if (res = LOCKED) then
          let
            (* val str = concat ["Current: ", Int.toString t,
                              " Lock: ", Int.toString (!lockId)] *)
            val _ = if not (!lockId = t) then
                      raise UnlockError ("Kind1")
                    else ()
            val _ = lockId := ~1
            val waitingT = if CirQueue.isEmpty que then
                             NONE
                           else
                             (* case *) CirQueue.deque que (* of
                                  NONE => (Assert.assert' ("Impossible: Queue is empty", fn () => false)
                                          ; raise Fail "Impossible: Queue is empty")
                                | SOME t => SOME t *)
            val _ = state := FREE
          in
           case waitingT of
                NONE => ()
              | SOME t =>
                  let
                    (* val RepTypes.RHOST (tid, _) = t
                    val _ = debug' (fn () => "Lock: readying "^(TID.tidToString tid)) *)
                  in
                    case t of
                         H_RTHRD (t) => SchedulerQueues.enqueHost (t, RepTypes.PRI)
                       | P_RTHRD (lockId, par) => SchedulerQueues.enqueParasite (lockId, par)
                  end
          end
        else if (res = CLAIMED) then
          ((* print "releaseCmlLock CLAIMED\n"; *)
          PacmlFFI.maybeWaitForGC ();
          releaseCmlLock l flockId)
        else (* res = FREE *)
          raise UnlockError ("Kind2")
      end

  end
end
