(* multicast.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* multicast-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Asynchronous multicast (one-to-many) channels.
 *)

signature MULTICAST =
   sig
      type 'a mchan
      type 'a port
      type 'a event = 'a Event.sevt

      (* create a new multicast channel *)
      val mChannel : unit -> 'a mchan
      (* create a new output port on a channel *)
      val port : 'a mchan -> 'a port
      (* receive a message from a port *)
      val recv : 'a port -> 'a
      val recvEvt : 'a port -> 'a event
      (* send a message to all of the ports of a channel *)
      val multicast : ('a mchan * 'a) -> unit
   end
