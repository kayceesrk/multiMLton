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
      val never     : 'a EventType.event
      val alwaysEvt : 'a -> 'a EventType.event

      val wrap        : ('a EventType.event * ('a -> 'b)) -> 'b EventType.event
      val wrapHandler : ('a EventType.event * (exn -> 'a)) -> 'a EventType.event

      val guard    : (unit -> 'a EventType.event) -> 'a EventType.event
      val withNack : (unit EventType.event -> 'a EventType.event) -> 'a EventType.event

      val choose : 'a EventType.event list -> 'a EventType.event
      val sync : 'a EventType.event -> 'a
      val select : 'a EventType.event list -> 'a
   end

signature EVENT_EXTRA =
   sig
      include EVENT

      type 'a status
      val enabled : {prio : int, doitFn : unit -> 'a option} -> 'a status
      val blocked : ({transId : TransID.trans_id,
                      cleanUp : unit -> unit,
                      next : unit -> Scheduler.rdy_thread} -> 'a) -> 'a status
      val bevt : (unit -> 'a status) -> 'a EventType.event

      val atomicCVarSet : CVar.cvar -> unit
      val cvarGetEvt    : CVar.cvar -> unit EventType.event
   end
