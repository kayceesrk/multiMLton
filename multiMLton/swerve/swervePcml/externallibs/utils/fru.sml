structure Fru =
struct

  fun fru2 (r1, r2, t) (r, (f, z)) =
      let datatype t = A of 'a | B of 'b 
	  val (a, b) = t r
	  fun g h z = r1 (case h z of A a => a | _ => a,
			  case h z of B b => b | _ => b)
      in
	  f (r2 (g A, g B)) z
      end
      
  fun fru3 (r1, r2, t) (r, (f, z)) =
      let datatype t = A of 'a | B of 'b | C of 'c
	  val (a, b, c) = t r
	  fun g h z =
              r1 (case h z of A a => a | _ => a,
		  case h z of B b => b | _ => b,
		  case h z of C c => c | _ => c)
      in
	  f (r2 (g A, g B, g C)) z
      end
      
  fun fru4 (r1, r2, t) (r, (f, z)) =
      let datatype t = A of 'a | B of 'b | C of 'c | D of 'd
	  val (a, b, c, d) = t r
	  fun g h z =
              r1 (case h z of A a => a | _ => a,
		  case h z of B b => b | _ => b,
		  case h z of C c => c | _ => c,
		  case h z of D d => d | _ => d)
      in
	  f (r2 (g A, g B, g C, g D)) z
      end

  fun fru5 (r1, r2, t) (r, (f, z)) =
      let datatype t = A of 'a | B of 'b | C of 'c | D of 'd | E of 'e
	  val (a, b, c, d, e) = t r
	  fun g h z =
              r1 (case h z of A a => a | _ => a,
		  case h z of B b => b | _ => b,
		  case h z of C c => c | _ => c,
		  case h z of D d => d | _ => d,
		  case h z of E d => e | _ => e)
      in
	  f (r2 (g A, g B, g C, g D, g E)) z
      end
      
end
