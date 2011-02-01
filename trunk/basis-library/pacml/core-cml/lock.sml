structure Lock :> LOCK =
struct

  open Critical
  structure TID = ThreadID

  structure Assert = LocalAssert(val assert = true)
  structure Debug = LocalDebug(val debug = true)

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))

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
                   tid = ref ~1,
                   count = ref 0,
                   que = CirQueue.new (1024)}

  val FREE = 0
  val LOCKED = 1
  val CLAIMED = 2

  fun yieldForLock (q, state) =
  let
    val tid = TID.getCurThreadId ()
    val () = TID.mark tid
    val atomicState = getAtomicState ()
    val () = setAtomicState (1)
    val () = Scheduler.atomicSwitchForWB
              (fn rt =>
                let
                  val rt' = PacmlPrim.move (SOME rt, false, true)
                  val _ = CirQueue.enque (q, rt')
                  val _ = state := LOCKED
                  val res = Scheduler.next ()
                  val RepTypes.RHOST (t, _) = res
                  val _ = debug' ("yieldForLock(5) "^(TID.tidToString t))
                in
                  res
                end)
    val () = setAtomicState (atomicState)
    val _ = debug' "yieldForLock(6)"
    val tid' = TID.getCurThreadId ()
    val _ = Assert.assert' ("yieldForLock: TIDs dont match ("
                            ^(TID.tidToString tid)^", "^(TID.tidToString tid')^")"
                            , fn () => TID.sameTid (tid, tid'))
  in
      ()
  end

  fun getCmlLock (l as RepTypes.LOCK {state, tid, count, que}) ftid =
  let
    val t = ftid () (* Has to be this way to account for parasite reification at maybePreempt *)
    fun enque () =
    let
      val res = cas (state, LOCKED, CLAIMED)
    in
      if res = LOCKED then
        (yieldForLock (que, state);
        getCmlLock l ftid)
      else if res = CLAIMED then
        enque ()
      else (* res = FREE *)
        getCmlLock l ftid
    end
  in
    if (!tid = t) then
      count := !count + 1
    else
      let
        val res = cas (state, FREE, LOCKED)
      in
        if res = FREE then
          (* We got the lock *)
          tid := t
        else (* res = LOCKED orelse res = CLAIMED *)
          enque ()
      end
  end

  fun releaseCmlLock (l as RepTypes.LOCK {state, tid, count, que}) ftid =
  let
    val t = ftid ()
  in
    if (!tid = t) andalso (!count > 0) then
      count := !count - 1
    else
      let
        val res = cas (state, LOCKED, CLAIMED)
      in
        if (res = LOCKED) then
          let
            (* val str = concat ["Current: ", Int.toString t,
                              " Lock: ", Int.toString (!tid)] *)
            val _ = if not (!tid = t) then
                      raise UnlockError ("Kind1")
                    else ()
            val _ = tid := ~1
            val _ = state := FREE
          in
           case (CirQueue.deque que) of
                NONE => ()
              | SOME t => SchedulerQueues.enque (t, RepTypes.PRI)
          end
        else if (res = CLAIMED) then
          releaseCmlLock l ftid
        else (* res = FREE *)
          raise UnlockError ("Kind2")
      end

  end
end
