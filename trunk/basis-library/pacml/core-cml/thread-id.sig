signature THREAD_ID =
sig
  type thread_id
  type thread_type
  type parasite_state = RepTypes.parasite_state

  val sameTid    : (thread_id * thread_id) -> bool
  val compareTid : (thread_id * thread_id) -> order
  val hashTid    : thread_id -> word

  val tidToString : thread_id -> string
  val tidToInt : thread_id -> int

  val getCurThreadId : unit -> thread_id
  val tidMsg : unit -> string
  val tidNum : unit -> int

  val getProcId : thread_id -> int
  val sameProcessor : thread_id * thread_id -> bool
end

signature THREAD_ID_EXTRA =
sig
  datatype thread_id' = datatype RepTypes.thread_id
  include THREAD_ID where type thread_id = thread_id'

  val setCurThreadId : thread_id -> unit

  val new : unit -> thread_id
  val newOnProc : int -> thread_id
  val newWithTid : int -> thread_id

  val bogus : string -> int -> thread_id

  val mark     : thread_id -> unit
  val unmark   : thread_id -> unit
  val isMarked : thread_id -> bool

  val reset : unit -> unit
  val nextLockId : unit -> int

  val getLockId : unit -> int

end
