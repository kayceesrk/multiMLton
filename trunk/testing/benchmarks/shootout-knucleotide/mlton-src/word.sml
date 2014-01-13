
structure Word =
   struct
      open Word
	 
      type t = word

      val fromChar = fromInt o Char.ord

      fun log2 (w: t): t =
	 if w = 0w0
	    then raise Fail "Word.log2 0"
	 else
	    let
	       fun loop (n, s, ac): word =
		  if n = 0w1
		     then ac
		  else
		     let
			val (n, ac) =
			   if n >= << (0w1, s)
			      then (>> (n, s), ac + s)
			   else (n, ac)
		     in
			loop (n, >> (s, 0w1), ac)
		     end
	    in
	       loop (w, 0w16, 0w0)
	    end

      fun roundDownToPowerOfTwo (w: t) = << (0w1, log2 w)

      fun roundUpToPowerOfTwo w =
	 let
	    val w' = roundDownToPowerOfTwo w
	 in
	    if w = w'
	       then w
	    else w' * 0w2
	 end
   end
