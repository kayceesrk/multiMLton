(* event.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* events-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of event values and the event combinators.
 *)

signature EVENT =
   sig
      val never     : 'a EventType.sevt
      val alwaysEvt : 'a -> 'a EventType.sevt

      val wrap        : ('a EventType.sevt * ('a -> 'b)) -> 'b EventType.sevt
      val sWrap        : (('a,'b) EventType.aevt * ('a -> 'c)) -> ('c,'b) EventType.aevt
      val aWrap        : (('a,'b) EventType.aevt * ('b -> 'c)) -> ('a,'c) EventType.aevt

      val wrapHandler : ('a EventType.sevt * (exn -> 'a)) -> 'a EventType.sevt
      val sWrapHandler : (('a, 'b) EventType.aevt * (exn -> 'a)) -> ('a, 'b) EventType.aevt
      val aWrapHandler : (('a, 'b) EventType.aevt * (exn -> 'b)) -> ('a, 'b) EventType.aevt

      val guard    : (unit -> 'a EventType.sevt) -> 'a EventType.sevt
      val withNack : (unit EventType.sevt -> 'a EventType.sevt) -> 'a EventType.sevt

      val choose : 'a EventType.sevt list -> 'a EventType.sevt
      val select : 'a EventType.sevt list -> 'a

      val aSync : ('a,'b) EventType.aevt -> 'a
      val sSync : 'a EventType.sevt -> 'a

      val sTrans : 'a EventType.sevt -> (unit, 'a) EventType.aevt
      val aTrans : ('a, 'b) EventType.aevt -> 'a EventType.sevt

   end

signature EVENT_EXTRA =
   sig
      include EVENT

      type sync_status
      type 'a status

      val enabled : {prio : int, doitFn : (sync_status Scheduler.thread option) -> 'a option} -> 'a status
      val blocked : ({transId : int ref,
                      cleanUp : unit -> unit,
                      next : unit -> Scheduler.rdy_thread,
                      parentThread : (unit -> Scheduler.rdy_thread) option} -> 'a) -> 'a status
      val bevt : (unit -> 'a status) -> 'a EventType.sevt
      val aevt : 'a EventType.sevt -> (unit, 'a) EventType.aevt

      val atomicCVarSet : CVar.cvar -> unit
      val cvarGetEvt    : CVar.cvar -> unit EventType.sevt
   end
