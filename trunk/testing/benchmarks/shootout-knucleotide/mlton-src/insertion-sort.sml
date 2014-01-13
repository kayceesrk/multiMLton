(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t

signature INSERTION_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for should be the <= funtion,
       * not just <.
       * This is necessary to handle duplicate elements.
       *)
      val sort: 'a array * ('a * 'a -> bool) -> unit
   end

(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure InsertionSort: INSERTION_SORT =
struct

open Array

(* Based on page 108 of Programming Pearls, by Bentley. *)
fun sort (a: 'a array, op <= : 'a * 'a -> bool): unit =
   let
      fun x i = sub (a, i)
      val _ =
	 Int.for
	 (1, Array.length a, fn i =>
	  let
	     val t = x i
	     fun sift (j: int) =
		if j > 0
		   then
		      let
			 val j' = j - 1
			 val z = x j'
		      in
			 if z <= t
			    then j
			 else (update (a, j, z)
			       ; sift j')
		      end
		else j
	     val _ = update (a, sift i, t)
	  in ()
	  end)
   in
      ()
   end

end
