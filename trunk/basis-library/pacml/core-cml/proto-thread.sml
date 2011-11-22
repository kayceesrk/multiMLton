structure ProtoThread : PROTO_THREAD =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Critical
  structure TID = ThreadID
  structure MT = MLtonThread

  exception ThreadCasting of string

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => (msg())^" : "^Int.toString(PacmlFFI.processorNumber()))

  type thread_id = ThreadID.thread_id
  type parasite = RepTypes.parasite
  type lock_id = int

  datatype thread = datatype RepTypes.thread
  datatype rdy_thread = datatype RepTypes.rdy_thread
  datatype thread_type = datatype RepTypes.thread_type
  datatype runnable_host = datatype RepTypes.runnable_host
  datatype parasite_state = datatype RepTypes.parasite_state


  (* Continuation management *)
  fun prepend (H_THRD (tid, t), f) =
        H_THRD (tid, MT.prepend (t, f))
    | prepend (P_THRD (lockId: int, par : parasite, g : (unit -> 'a) -> unit), f : 'b -> 'a) =
        P_THRD (lockId, par, fn h => g (f o h))

  fun prep (H_THRD (tid, t)) = H_RTHRD (RHOST (tid, MT.prepare (t, ())))
    | prep (P_THRD (lockId, par, g)) = (g (fn () => ()); P_RTHRD (lockId, par))

  fun prepVal (H_THRD (tid, t), v) = H_RTHRD (RHOST((tid, MT.prepare (t,v))))
    | prepVal (P_THRD (lockId, par, g), v) = (g (fn () => v); P_RTHRD (lockId, par))

  fun prepFn (H_THRD (tid, t), f) = H_RTHRD (RHOST ((tid, MT.prepare (MT.prepend (t,f), ()))))
    | prepFn (p, f) = prep (prepend (p, f))



  (* Manipulate current thread info *)
  fun getThreadState () =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
    in
      !pstate
    end

  fun setThreadState (ps) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
    in
      pstate := ps
    end

  fun getProp (x) =
    let
      val TID.TID {pstate = ref (PSTATE (ps)), ...} = TID.getCurThreadId ()
    in
      (x ps)
    end

  fun getThreadType () = getProp (#threadType)
  fun getParasiteBottom () = case getProp (#parasiteBottom) of
                                  (offset, tid) => (Assert.assert' ("PT.getParasiteBottom", fn () => (tid = TID.tidNum ()));
                                                    debug' (fn () => ("PT.getParasiteBottom = "^(Int.toString offset)));
                                                    offset)
  fun getNumPenaltySpawns () = getProp (#numPenaltySpawns)
  fun getLockId () = getProp (#lockId)

  fun setThreadType (t) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
    in
      PacmlPrim.unsafeAssign
        (pstate, PSTATE {threadType = t,
                         parasiteBottom = #parasiteBottom ps,
                         numPenaltySpawns = #numPenaltySpawns ps,
                         lockId = #lockId ps})
    end

  fun setParasiteBottom (pb) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
    in
      PacmlPrim.unsafeAssign
        (pstate, PSTATE {threadType = #threadType ps,
                         parasiteBottom = (pb, TID.tidNum ()),
                         numPenaltySpawns = #numPenaltySpawns ps,
                         lockId = #lockId ps})
    end

  fun setNumPenaltySpawns (n) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
    in
      PacmlPrim.unsafeAssign
        (pstate, PSTATE {threadType = #threadType ps,
                         parasiteBottom = #parasiteBottom ps,
                         numPenaltySpawns = n,
                         lockId = #lockId ps})
    end

  fun setLockId (n) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
    in
      PacmlPrim.unsafeAssign
        (pstate, PSTATE {threadType = #threadType ps,
                         parasiteBottom = #parasiteBottom ps,
                         numPenaltySpawns = #numPenaltySpawns ps,
                         lockId = n})
    end

  fun setParasiteState (tt, pb, nps, lid) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
      val newSt = PSTATE {threadType = tt,
                          parasiteBottom = (pb, TID.tidNum ()),
                          numPenaltySpawns = nps,
                          lockId = lid}
    in
      PacmlPrim.unsafeAssign (pstate, newSt)
    end



  fun disableParasitePreemption () =
  let
    val TID.TID {preemptParasite, ...} = TID.getCurThreadId ()
  in
    preemptParasite := false
  end

  fun enableParasitePreemption () =
  let
    val TID.TID {preemptParasite, ...} = TID.getCurThreadId ()
  in
    preemptParasite := true
  end

  fun toPreemptParasite () =
  let
    val TID.TID {preemptParasite, ...} = TID.getCurThreadId ()
  in
    !preemptParasite
  end

  fun getThreadTypeString () =
    case getThreadType () of
         HOST => "HOST"
       | PARASITE => "PARASITE"

  (* Parasite management *)

  val extractParasiteFromHost = ParasiteFFI.extractParasiteFromHost
  val copyParasite = ParasiteFFI.copyParasite
  val proceedToExtractParasite = ParasiteFFI.proceedToExtractParasite
  val prefixAndSwitchTo = ParasiteFFI.prefixAndSwitchTo
  val getFrameBottomAsOffset = ParasiteFFI.getFrameBottomAsOffset
  val jumpDown = ParasiteFFI.jumpDown

  datatype prefix_kind = datatype RepTypes.prefix_kind

  fun atomicPrefixAndSwitchToHelper (lockId, thlet, kind) =
  let
    val () = Assert.assertAtomic' ("ProtoThread.atomicPrefixAndSwitchToHelper", NONE)
    val () = debug' (fn () => "ProtoThread.atomicPrefixAndSwitchToHelper")
    val () = TID.mark (TID.getCurThreadId ())
    val state = getThreadState ()
    fun doit () =
    let
      val pb = getFrameBottomAsOffset ()
      val tt = case kind of
                  PREFIX_REGULAR => PARASITE
                | _ => getThreadType ()
      val nps = getNumPenaltySpawns ()
      val () = setParasiteState (tt, pb, nps, lockId)
      val _ = prefixAndSwitchTo (thlet) (* Implicit atomic End *)
      val _ = disableParasitePreemption ()
    in
      PacmlFFI.noop ()
    end
    val _ = Primitive.dontInline doit

    (* control returns *)
    fun foo () = case kind of
                     PREFIX_REGULAR => ("PrefixRegular "^(Bool.toString (toPreemptParasite ())))
                   | _ => ("PrefixSpecial"^(Bool.toString (toPreemptParasite ())))
    val _ = Assert.assert ([],
                           fn () => "ProtoThread.atomicPrefixAndSwitchToHelper: Preemption enabled! -- "^foo(),
                           fn () => (toPreemptParasite () = false))
    val _ = setThreadState (state)
    val _ = enableParasitePreemption ()
  in
    ()
  end

  fun atomicPrefixAndSwitchTo (lockId, thlet) = atomicPrefixAndSwitchToHelper (lockId, thlet, PREFIX_REGULAR)
  (* Special version does not set the thread type to parasite. Used when reifying a host thread
    * from a parasite.
    *)
  fun atomicPrefixAndSwitchToSpecial (lockId, thlet) = atomicPrefixAndSwitchToHelper (lockId, thlet, PREFIX_SPECIAL)

  fun getRunnableHost (rthrd) = case rthrd of
                                     H_RTHRD rhost => rhost
                                   | _ => raise ThreadCasting "getRunnableHost failed"

  fun spawnParasite f =
  let
    val _ = Statistics.doit (PacmlFFI.parasiteCreatedEvent)
    val _ = atomicBegin ()
    val state = getThreadState () (* Save state on stack *)
    val () = TID.mark (TID.getCurThreadId ())

    fun cleanUp () =
    let
      val _ = Assert.assert' ("ProtoThread.atomicPrefixAndSwitchToHelper: Preemption enabled!",
                              fn () => (toPreemptParasite () = false))
      val _ = debug' (fn () => "ProtoThread.spawnParasite.resetting thread state")
      val _ = setThreadState (state)
      val _ = enableParasitePreemption ()
    in
      ()
    end

    fun doit () =
    let
      val offset = getFrameBottomAsOffset ()
      val _ = setParasiteBottom (offset)
      val _ = setNumPenaltySpawns (0)
      val _ = setLockId (TID.nextLockId ())
      val _ = atomicEnd ()
      val _ = f () handle e => case e of
                                  RepTypes.DOIT_FAIL => (debug' (fn () => "DOIT_FAIL. Letting though");
                                                          disableParasitePreemption ();
                                                          cleanUp ();
                                                          raise e)
                                | _ => (debug' (fn () => concat["SpawnParasite: parasite threw an exception -- Exn: ", exnName e, " Msg: ", exnMessage e]);
                                        ignore (OS.Process.exit OS.Process.failure))
      val _ = disableParasitePreemption ()
    in
      PacmlFFI.noop () (* Needed to prevent inlining f () *)
    end

    val _ = setThreadType (PARASITE)
    val _ = Primitive.dontInline (doit) (* call the parasite *)

    (* control returns *)
    val _ = cleanUp ()
  in
    ()
  end

  fun new f =
  let
    val nt = MLtonThread.new f
  in
    nt
  end

end
