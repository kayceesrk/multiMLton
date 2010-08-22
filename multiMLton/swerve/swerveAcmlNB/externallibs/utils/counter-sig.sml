signature COUNTER =
   sig
       
       type counter
	    
       val new: unit -> counter
       val newAt: int -> counter

       val clear: counter -> unit
       val reset: counter * int -> unit

       val incN: counter -> int -> unit
       val decN: counter -> int -> unit

       val inc: counter -> unit
       val dec: counter -> unit

       val next:counter -> int

       val value:counter -> int
       val equals:counter * counter-> bool

   end
