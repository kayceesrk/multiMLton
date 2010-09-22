(* Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature P_CHANNEL =
sig
    type 'a pChan

    val pChannel: unit -> 'a pChan
    val pSpawn: (unit -> unit) -> unit
    val pSend : ('a pChan * 'a) -> unit
    val pRecv : 'a pChan -> 'a

    val pSendEvt  : ('a chan * 'a) -> unit EventType.sevt
    val pRecvEvt  : 'a chan -> 'a EventType.sevt

    val pSendPoll : ('a chan * 'a) -> bool
    val pRecvPoll : 'a chan -> 'a option

    val pASendEvt : ('a chan * 'a) -> (unit, unit) EventType.aevt
    val pARecvEvt  : 'a chan -> (unit, 'a) EventType.aevt
end

signature P_CHANNEL_EXTRA =
sig
  include P_CHANNEL

  val printFrames : unit -> unit
  val dontInline : (unit -> 'a) -> 'a
end
