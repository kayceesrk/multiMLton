signature SYNC_VAR =
sig

  type 'a ivar  (* I-structure variable *)
  type 'a mvar  (* M-structure variable *)

  exception Put (* raised on put operations to full cells *)

  val iVar     : unit -> 'a ivar
  val iPut     : ('a ivar * 'a) -> unit
  val iGet     : 'a ivar -> 'a
  val iGetEvt  : 'a ivar -> 'a Event.sevt
  val iGetPoll : 'a ivar -> 'a option
  val sameIVar : ('a ivar * 'a ivar) -> bool

  val mVar      : unit -> 'a mvar
  val mVarInit  : 'a -> 'a mvar
  val mPut      : ('a mvar * 'a) -> unit
  val mTake     : 'a mvar -> 'a
  val mTakeEvt  : 'a mvar -> 'a Event.sevt
  val mTakePoll : 'a mvar -> 'a option
  val mGet      : 'a mvar -> 'a
  val mGetEvt   : 'a mvar -> 'a Event.sevt
  val mGetPoll  : 'a mvar -> 'a option
  val mSwap     : ('a mvar * 'a) -> 'a
  val mSwapEvt  : ('a mvar * 'a) -> 'a Event.sevt
  val sameMVar  : ('a mvar * 'a mvar) -> bool
end

signature SYNC_VAR_EXTRA =
sig
  include SYNC_VAR
end
