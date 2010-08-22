(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
type word = Word.t

signature HASH_SET =
   sig
      type 'a t

      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      (* insertIfNew (s, h, p, f, g) looks in the set s for an entry with hash h
       * satisfying predicate p.  If the entry is there, it is returned after
       * being applied to g.  Otherwise, the function f is called to create a
       * new entry, which is inserted and returned.
       * NOTE: f must not modify the hash set during its evaluation.
       *)
      val insertIfNew:
         'a t * word * ('a -> bool) * (unit -> 'a) * ('a -> unit) -> 'a
      (* lookupOrInsert (s, h, p, f)  looks in the set s for an entry with hash h
       * satisfying predicate p.  If the entry is there, it is returned.
       * Otherwise, the function f is called to create a new entry, which is
       * inserted and returned.
       * NOTE: f must not modify the hash set during its evaluation.
       *)
      val new: {hash: 'a -> word} -> 'a t
      (* newOfSize {hash, size}
       * creates a table that can handle size elements without resizing.
       *)
      val newOfSize: {hash: 'a -> word,
                      size: int} -> 'a t
      val peek: 'a t * word * ('a -> bool) -> 'a option
      (* remove an entry.  Error if it's not there. *)
      val remove: 'a t * word * ('a -> bool) -> unit
      (* removeAll (s, p) removes all entries from s that satisfy predicate p. *)
      val size: 'a t -> int
   end


functor TestHashSet (S: HASH_SET): sig end =
struct

open S


end
