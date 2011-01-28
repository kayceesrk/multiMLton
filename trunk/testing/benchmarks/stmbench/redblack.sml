(* Signature for dictionaries *)
(*
   For simplicity, we assume keys are strings, while stored entries
   are of arbitrary type.  This is prescribed in the signature.

   Existing entries may be "updated" by inserting a new entry
   with the same key.
*)

signature DICT =
sig
  type key = string
  type 'a entry = key * 'a

  type 'a dict

  val empty : 'a dict
  val lookup : 'a dict -> key -> 'a
  val insert : 'a dict * 'a entry -> 'a dict
end

(* Unbalanced binary search tree implementation. *)
structure BinarySearchTree :> DICT = struct
  type key = string
  type 'a entry = key * 'a

  (* Rep invariant: 'a tree is a binary search tree *)
  datatype 'a tree = Empty | Node of 'a tree * 'a entry * 'a tree
  type 'a dict = 'a tree

  exception Lookup of key

  val empty = Empty

  fun insert (Empty, entry) = Node (Empty, entry, Empty)
    | insert (n as Node (l, e as (k,_), r), e' as (k',_)) =
      (case String.compare (k, k')
         of LESS => Node (insert (l, e'), e, r)
          | GREATER => Node (l, e, insert (r, e'))
          | EQUAL => n)

  fun lookup (Empty) k = raise (Lookup k)
    | lookup (Node (l, (k, v), r)) k' =
      (case String.compare (k, k')
         of EQUAL => v
          | LESS => lookup l k'
          | GREATER => lookup r k')

end

structure RedBlackTree :> DICT =
struct
  type key = string
  type 'a entry = string * 'a

  (* Representation invariant: binary search tree + red-black conditions *)
  datatype 'a dict =
    Empty				(* considered black *)
  | Red of 'a entry * 'a dict * 'a dict
  | Black of 'a entry * 'a dict * 'a dict

  val empty = Empty

  exception Lookup of key

  fun lookup dict key =
    let
      fun lk (Empty) = raise (Lookup key)
	| lk (Red tree) = lk' tree
        | lk (Black tree) = lk' tree
      and lk' ((key1, datum1), left, right) =
	    (case String.compare(key,key1)
	       of EQUAL => datum1
	        | LESS => lk left
		| GREATER => lk right)
      in
	lk dict
      end

  fun restoreLeft (Black (z, Red (y, Red (x, d1, d2), d3), d4)) =
        Red (y, Black (x, d1, d2), Black (z, d3, d4))
    | restoreLeft (Black (z, Red (x, d1, Red (y, d2, d3)), d4)) =
        Red (y, Black (x, d1, d2), Black (z, d3, d4))
    | restoreLeft dict = dict

  fun restoreRight (Black (x, d1, Red (y, d2, Red (z, d3, d4)))) =
        Red (y, Black (x, d1, d2), Black (z, d3, d4))
    | restoreRight (Black (x, d1, Red (z, Red (y, d2, d3), d4))) =
        Red (y, Black (x, d1, d2), Black (z, d3, d4))
    | restoreRight dict = dict

  fun insert (dict, entry as (key, datum)) =
    let
      (* val ins : 'a dict -> 'a dict insert entry *)
      (* ins (Red _) may violate color invariant at root *)
      (* ins (Black _) or ins (Empty) will be red/black tree *)
      (* ins preserves black height *)
      fun ins (Empty) = Red (entry, Empty, Empty)
	| ins (Red (entry1 as (key1, datum1), left, right)) =
	  (case String.compare (key, key1)
	     of EQUAL => Red (entry, left, right)
	      | LESS => Red (entry1, ins left, right)
	      | GREATER => Red (entry1, left, ins right))
        | ins (Black (entry1 as (key1, datum1), left, right)) =
	  (case String.compare (key, key1)
	     of EQUAL => Black (entry, left, right)
	      | LESS => restoreLeft (Black (entry1, ins left, right))
	      | GREATER => restoreRight (Black (entry1, left, ins right)))
    in
      case ins dict
	of Red (t as (_, Red _, _)) => Black t (* re-color *)
	 | Red (t as (_, _, Red _)) => Black t (* re-color *)
	 | dict => dict
    end
end
