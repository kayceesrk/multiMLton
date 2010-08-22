(* Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature THREADLET =
sig
    type 'a asyncChan

    val newAChan : unit -> 'a asyncChan
    val async : (unit -> unit) -> unit
    val aSend : ('a asyncChan * 'a) -> unit
    val aRecv : 'a asyncChan -> 'a

    val printFrames : unit -> unit
    val dontInline : (unit -> 'a) -> 'a

end
