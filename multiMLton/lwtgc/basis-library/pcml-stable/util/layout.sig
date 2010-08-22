(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LAYOUT =
   sig

      type t

      val str: string -> t
      val toString: t -> string
      val seq: t list -> t

   end
