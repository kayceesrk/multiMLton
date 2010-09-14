(* Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHANNEL =
sig
    type 'a chan

    val channel: unit -> 'a chan
    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a

    val sendEvt  : ('a chan * 'a) -> unit Event.sevt
    (* val recvEvt  : 'a chan -> 'a EventType.sevt *)

    val sendPoll : ('a chan * 'a) -> bool
    val recvPoll : 'a chan -> 'a option

    val aSendEvt : ('a chan * 'a) -> (unit, unit) Event.aevt
    (* val aRecvEvt  : 'a chan -> (unit, 'a) EventType.aevt *)
end

signature CHANNEL_EXTRA =
sig
  include CHANNEL

end
