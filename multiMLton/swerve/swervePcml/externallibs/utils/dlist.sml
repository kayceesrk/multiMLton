(* imperative double linked lists *)

signature DLIST = sig

    exception IndexException
    exception CorruptedDList

    type 'a dlist
    type 'a dnode

    val mkEmpty: unit -> 'a dlist
    val isEmpty: 'a dlist -> bool
    val cons: 'a -> 'a dlist -> 'a dlist
    val snip: 'a dlist -> 'a dnode -> unit
    val nth: 'a dlist -> int -> 'a dnode

    val isFirst: 'a dnode -> bool
    val isLast: 'a dnode -> bool

    val first: 'a dlist -> 'a dnode
    val last: 'a dlist -> 'a dnode
    val size: 'a dlist -> int

    val element: 'a dnode -> 'a

    val foreach: 'a dlist -> ('a -> 'b) -> unit

end

structure DoubleList :> DLIST  =
struct

  exception IndexException
  exception CorruptedDList

  datatype 'a dlist = Nil 
		    | FLNode of {first: 'a dlist ref, last: 'a dlist ref} 
		    | Node of {previous: 'a dlist ref, next: 'a dlist ref, element: 'a}

  type 'a dnode = 'a dlist
			      
  fun mkEmpty () = let val n as FLNode {first,last} = FLNode {first = ref Nil, last = ref Nil}
		   in
		       first := n;
		       last  := n;
		       n
		   end
		   
  fun isEmpty (fl as FLNode {first, last}) = let fun isEmpty' (FLNode _) (FLNode _) = true
						   | isEmpty' _ _ = false
					     in
						 isEmpty' (!first) (!last)
					     end
    | isEmpty _ = false
						 
  fun cons e (dl as FLNode {first,last}) = let val currfirst = !first
					       val node = Node {previous = ref dl, next = ref currfirst, element = e}
					       val currfirst_previous = case currfirst of
									    FLNode _ => last
									  | Node {previous,...} => previous
									  | _ => raise CorruptedDList
					   in
					       first := node;
					       currfirst_previous := node;
					       dl
					   end
    | cons _ _ = raise CorruptedDList

					       
  fun snip (dl as FLNode {first,last}) (node as Node {previous, next,...}) = 
      let val pnode = !previous
	  val nnode = !next				     
	  val pnodenext = case pnode of
			      FLNode _ => first
			    | Node {next,...} => next
			    | Nil => raise CorruptedDList
	  val nnodeprevious = case nnode of 
				  FLNode _ => last
				| Node {previous,...} => previous
				| Nil => raise CorruptedDList
      in
	  pnodenext := nnode;
	  nnodeprevious := pnode;
	  previous := Nil;
	  next := Nil
      end
    | snip _ _ = raise CorruptedDList
	  
  fun isFirst (Node {previous,...}) = case !previous of
					  FLNode _ => true
					| _ => false

  fun isLast  (Node {next,...}) = case !next of
				       FLNode _ => true
				     | _ => false

  fun first (FLNode {first,...}) = !first
  fun last  (FLNode {last,...}) = !last
  fun element (Node {element,...}) = element

  fun size (FLNode {first,...}) = let fun count node n = case node of 
							     Node {next,...} => count (!next) (n+1)
							   | _ => n
				  in
				      count (!first) 0
				  end
					 
  local
      fun chain node 0 = (case node of
			      Node _ => node
			    | _ => raise IndexException)
	| chain node n = case node of
			     Node {next,...} => chain (!next) (n-1)
			   | _ => raise IndexException
  in
  fun nth (FLNode {first,...}) n = chain (!first) n
    | nth (node as Node _) n = chain node n
    | nth Nil _ = raise CorruptedDList
  end
	     
  fun foreach (FLNode {first,last}) proc = (case !first of
						Node {next,element,...} => let val _ = proc element
									   in
									       case !next of 
										   FLNode _ => ()
										 | _ => foreach (!next) proc
									   end
					      | FLNode _ => ()
					      | Nil => raise CorruptedDList )
    | foreach (Node {next, element, ...}) proc = let val _ = proc element
						 in
						     case !next of
							 FLNode _ => ()
						       | Node _  => foreach (!next) proc
						       | Nil => raise CorruptedDList
						 end
    | foreach Nil proc = raise CorruptedDList
			       
  fun test () = let val l = mkEmpty()		  
		in
		    cons "ray" l;
		    cons "eve" l;
		    cons "cory" l;
		    l
		end 
end
