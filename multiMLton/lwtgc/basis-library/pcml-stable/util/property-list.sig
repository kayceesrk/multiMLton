(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature PROPERTY_LIST =
   sig
      type t

      val equals: t * t -> bool
      val new: unit -> t
      val newProperty:
         unit -> {
                  (* See if a property is in a property list.
                   * NONE if it isn't.
                   *)
                  peek: t -> 'a option,
                  (* Add the value of the property -- must not already exist. *)
                  add: t * 'a -> unit,
                  (* Remove a property from a property list.
                   * Noop if the property isn't there.
                   *)
                  remove: t -> unit
                  }

   end
