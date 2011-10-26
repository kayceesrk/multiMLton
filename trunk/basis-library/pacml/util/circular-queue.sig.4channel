(* circular-queue.sig
 * @authors KC Sivaramakrishnan (chandras@cs.purdue.edu)
 *)
signature CIR_QUEUE =
   sig
      type 'a t

      val new: unit -> 'a t
      val newExplicit : {arr : 'a option array, rp : int, wp : int, size: int} -> 'a t

      val printLayout : 'a t * ('a -> string) -> unit
      val isEmpty: 'a t -> bool

      val deque: 'a t -> 'a option
      val enque: 'a t * 'a option -> unit
      val undeque: 'a t * 'a option -> unit

      val cleanPrefix: 'a t * ('a -> bool) -> unit
      val cleanSuffix: 'a t * ('a -> bool) -> unit
   end
