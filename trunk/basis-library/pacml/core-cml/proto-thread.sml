structure ProtoThread : PROTO_THREAD =
struct

  structure Assert = LocalAssert(val assert = false)
  structure Debug = LocalDebug(val debug = false)

  open Critical
  structure TID = ThreadID
  structure MT = MLtonThread

  exception ThreadCasting of string

  fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
  fun debug' msg = debug (fn () => msg^" : "^Int.toString(PacmlFFI.processorNumber()))

  type thread_id = ThreadID.thread_id
  type parasite = RepTypes.parasite
  datatype thread = datatype RepTypes.thread
  datatype rdy_thread = datatype RepTypes.rdy_thread
  datatype thread_type = datatype RepTypes.thread_type
  datatype runnable_host = datatype RepTypes.runnable_host
  datatype parasite_state = datatype RepTypes.parasite_state


  (* Continuation management *)
  fun prepend (H_THRD (tid, t), f) = H_THRD (tid, MT.prepend (t, f))
    | prepend (P_THRD (par : parasite, g : (unit -> 'a) -> unit), f : 'b -> 'a) = P_THRD (par, fn h => g (f o h))

  fun prep (H_THRD (tid, t)) = H_RTHRD (RHOST (tid, MT.prepare (t, ())))
    | prep (P_THRD (par, g)) = (g (fn () => ()); P_RTHRD (par))

  fun prepVal (H_THRD (tid, t), v) = H_RTHRD (RHOST((tid, MT.prepare (t,v))))
    | prepVal (P_THRD (par, g), v) = (g (fn () => v); P_RTHRD (par))

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
                                                    offset)
  fun getNumPenaltySpawns () = getProp (#numPenaltySpawns)

  fun setThreadType (t) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
    in
      pstate := PSTATE {threadType = t,
                        parasiteBottom = #parasiteBottom ps,
                        numPenaltySpawns = #numPenaltySpawns ps}
    end

  fun setParasiteBottom (pb) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
    in
      pstate := PSTATE {threadType = #threadType ps,
                        parasiteBottom = (pb, TID.tidNum ()),
                        numPenaltySpawns = #numPenaltySpawns ps}
    end

  fun setNumPenaltySpawns (n) =
    let
      val TID.TID {pstate, ...} = TID.getCurThreadId ()
      val PSTATE (ps) = !pstate
    in
      pstate := PSTATE {threadType = #threadType ps,
                        parasiteBottom = #parasiteBottom ps,
                        numPenaltySpawns = n}
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

  fun atomicPrefixAndSwitchToHelper (thlet, kind) =
  let
    val () = Assert.assertAtomic' ("ProtoThread.atomicPrefixAndSwitchToHelper", NONE)
    val () = TID.mark (TID.getCurThreadId ())
    val state = getThreadState ()
    val _ = case kind of
                  PREFIX_REGULAR => setThreadType (PARASITE)
                | _ => ()
    fun doit () =
    let
      val offset = getFrameBottomAsOffset ()
      val _ = setParasiteBottom (offset)
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

  fun atomicPrefixAndSwitchTo (thlet) = atomicPrefixAndSwitchToHelper (thlet, PREFIX_REGULAR)
  (* Special version does not set the thread type to parasite. Used when reifying a host thread
    * from a parasite.
    *)
  fun atomicPrefixAndSwitchToSpecial (thlet) = atomicPrefixAndSwitchToHelper (thlet, PREFIX_SPECIAL)

  fun getRunnableHost (rthrd) = case rthrd of
                                     H_RTHRD rhost => rhost
                                   | _ => raise ThreadCasting "getRunnableHost failed"

  fun spawnParasite f =
  let

    val _ = atomicBegin ()
    val state = getThreadState () (* Save state on stack *)
    val () = TID.mark (TID.getCurThreadId ())

    fun cleanUp () =
    let
      val _ = Assert.assert' ("ProtoThread.atomicPrefixAndSwitchToHelper: Preemption enabled!",
                              fn () => (toPreemptParasite () = false))
      val _ = debug' "ProtoThread.spawnParasite.resetting thread state"
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
      val _ = atomicEnd ()
      val _ = f () handle e => case e of
                                  RepTypes.DOIT_FAIL => (debug' "DOIT_FAIL. Letting though";
                                                          disableParasitePreemption ();
                                                          cleanUp ();
                                                          raise e)
                                | _ => (debug' (concat["SpawnParasite: parasite threw an exception -- Exn: ", exnName e, " Msg: ", exnMessage e]);
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

end
