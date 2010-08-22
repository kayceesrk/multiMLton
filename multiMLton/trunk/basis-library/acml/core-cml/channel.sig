(* channel.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* channel-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of synchronous channels.
 *)

signature CHANNEL =
   sig

      type 'a chan

      val channel : unit -> 'a chan
      val sameChannel : ('a chan * 'a chan) -> bool

      val send : ('a chan * 'a) -> unit
      val aSend : ('a chan * 'a) -> unit
      val recv : 'a chan -> 'a

      val sendEvt  : ('a chan * 'a) -> unit EventType.sevt
      val recvEvt  : 'a chan -> 'a EventType.sevt

      val sendPoll : ('a chan * 'a) -> bool
      val recvPoll : 'a chan -> 'a option

      val aSendEvt : ('a chan * 'a) -> (unit, unit) EventType.aevt
      val aRecvEvt  : 'a chan -> (unit, 'a) EventType.aevt

      (* XXX KC Channel structure is not where this belongs. Move it elsewhere *)
      val callbackEvt : ('a, 'c) EventType.aevt * ('c -> 'b) -> ('b EventType.sevt, 'c) EventType.aevt
  end

signature CHANNEL_EXTRA =
   sig
      include CHANNEL
      (*val resetChan : 'a chan -> unit*)
   end
