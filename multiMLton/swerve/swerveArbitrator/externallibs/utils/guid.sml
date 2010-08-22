(* Oh boy not thread (even fair!!) safe at all *)

functor GuidFn () :> GUID =
struct

  type guid = int

  val guid = ref 0

  fun peek () = !guid

  fun next () = let val v = peek()
		in
		    guid := v + 1;
		    v
		end

  fun eq (g1,g2) = g1 = g2
		    
  fun toString guid = Int.toString (guid)
  fun sPeek () = Int.toString (peek())

  fun layout gid = Layout.str (toString gid)

end
