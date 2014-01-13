structure List =
   struct
      open List

      type 'a t = 'a list

      fun peek (l, f) = List.find f l
	 
      fun fold (l, b, f) = List.foldl f b l

      fun foreach (l, f) = List.app f l

      fun forall (l, f) = List.all f l

      fun appendRev (l1, l2) = fold (l1, l2, op ::)
	 
      fun removeFirst (l, f) =
	 let
	    fun loop (l, ac) =
	       case l of
		  [] => raise Fail "removeFirst"
		| x :: l =>
		     if f x
			then appendRev (ac, l)
		     else loop (l, x :: ac)
	 in
	    loop (l, [])
	 end
   end
