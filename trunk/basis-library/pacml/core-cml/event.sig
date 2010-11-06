signature EVENT =
sig
  type 'a sevt = 'a RepTypes.sevt
  type ('a,'b) aevt = ('a, 'b) RepTypes.aevt

  val never : 'a sevt
  val alwaysEvt : 'a -> 'a sevt

  val wrap        : ('a sevt * ('a -> 'b)) -> 'b sevt
  val sWrap        : (('a,'b) aevt * ('a -> 'c)) -> ('c,'b) aevt
  val aWrap        : (('a,'b) aevt * ('b -> 'c)) -> ('a,'c) aevt

  val guard    : (unit -> 'a sevt) -> 'a sevt

  val choose : 'a sevt list -> 'a sevt
  val select : 'a sevt list -> 'a

  val aSync : ('a,'b) aevt -> 'a
  val sSync : 'a sevt -> 'a

  val sTrans : 'a sevt -> (unit, 'a) aevt
  val aTrans : ('a, 'b) aevt -> 'a sevt
end

signature EVENT_EXTRA =
sig
  include EVENT

  type 'a status
  type ('a,'b) cevt

  type 'a thread = 'a RepTypes.thread
  type rdy_thread = RepTypes.rdy_thread

  val wrapHandler : ('a sevt * (exn -> 'a)) -> 'a sevt
  val sWrapHandler : (('a, 'b) aevt * (exn -> 'a)) -> ('a, 'b) aevt
  val aWrapHandler : (('a, 'b) aevt * (exn -> 'b)) -> ('a, 'b) aevt

  val enabled : {prio : int, doitFn : unit -> 'a} -> 'a status
  val blocked : (int ref -> 'a) -> 'a status
  val bevt : (unit -> 'a status) -> 'a sevt
  val aevt : 'a sevt -> (unit, 'a) aevt
end
