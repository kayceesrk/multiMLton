(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Counter: COUNTER =
struct

type counter = int ref
		  
fun newAt n = ref  n

fun new () = ref 0

fun reset (r, n) = r := n

fun clear r = r := 0

fun incN r n = r := !r + n

fun decN r n = let val v = !r - n
	       in
		   if v < 0
		   then r := 0
		   else r := v
	       end
	       
fun inc r = incN r 1
	    
fun dec r =  decN r 1

fun value r = !r

fun next r = value r before inc r

val equals = fn (r, r') => r = r'
   
end
