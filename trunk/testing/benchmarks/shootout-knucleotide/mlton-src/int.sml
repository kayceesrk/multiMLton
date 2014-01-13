structure Int =
   struct
      open Int

      type t = int

      fun for (start, stop, f) =
	 let
	    fun loop i = if i = stop then () else (f i; loop (i + 1))
	 in
	    loop start
	 end

      fun forDown (lo, hi, f) =
	 let
	    fun loop i =
	       let
		  val i = i - 1
	       in
		  if i < lo
		     then ()
		  else (f i; loop i)
	       end
	 in
	    loop hi
	 end

      fun exists (start: t, stop: t, f: t -> bool): bool =
	 let
	    fun loop i = i < stop andalso (f i orelse loop (i + 1))
	 in
	    loop start
	 end

      fun forall (start, stop, f) = not (exists (start, stop, not o f))

      fun fold (start: t, stop: t, a: 'a, f: t * 'a -> 'a): 'a =
	 let
	    fun loop (i: t, a: 'a) =
	       if i >= stop
		  then a
	       else loop (i + 1, f (i, a))
	 in loop (start, a)
	 end

      fun dec r = r := !r - 1
      fun inc r = r := !r + 1
      fun add v r = r := !r + v
   end

val for = Int.for
