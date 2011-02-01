(* fun-queue.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature CIR_QUEUE =
   sig
      type 'a t

      val deque: 'a t -> 'a option
      val isEmpty: 'a t -> bool
      val enque: 'a t * 'a option -> unit
      val new: int -> 'a t
   end
