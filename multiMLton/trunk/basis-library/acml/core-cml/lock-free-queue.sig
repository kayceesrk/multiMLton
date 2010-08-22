(* imp-queue.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature LOCK_FREE_QUEUE =
   sig
      type 'a t

      val deque: 'a t -> 'a option
      val unsafeEmpty: 'a t -> bool
      val enque: 'a t * 'a -> unit
      val undeque: 'a t * 'a -> unit
      val new: unit -> 'a t
      val unsafeReset: 'a t -> unit
   end
