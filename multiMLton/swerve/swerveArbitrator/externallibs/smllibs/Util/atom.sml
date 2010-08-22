(* atom.sml
 *
 * COPYRIGHT (c) 1996 by AT&T Research
 *
 * AUTHOR:	John Reppy
 *		AT&T Bell Laboratories
 *		Murray Hill, NJ 07974
 *		jhr@research.att.com
 *
 * TODO: add a gensym operation?
 *)

structure Atom :> ATOM =
  struct

  (* Atoms are hashed strings that support fast equality testing. *)
    datatype atom = ATOM of {
	hash : word,
	id : string
      }

  (* return the string representation of the atom *)
    fun toString (ATOM{id, ...}) = id

  (* return a hash key for the atom *)
    fun hash (ATOM{hash, ...}) = hash

  (* return true if the atoms are the same *)
    fun sameAtom (ATOM{hash=h1, id=id1}, ATOM{hash=h2, id=id2}) =
	  (h1 = h2) andalso (id1 = id2)

  (* compare two names for their relative order; note that this is
   * not lexical order!
   *)
    fun compare (ATOM{hash=h1, id=id1}, ATOM{hash=h2, id=id2}) =
	  if (h1 = h2)
	    then if (id1 = id2)
	      then EQUAL
	    else if (id1 < id2)
	      then LESS
	      else GREATER
	  else if (h1 < h2)
	    then LESS
	    else GREATER

  (* the unique name hash table *)
    val tableSz = 64
    val table = ref(Array.array(tableSz, [] : atom list))
    val numItems = ref 0

  (* Map a string to the corresponding unique atom. *)
    fun atom s = let
	  val h = HashString.hashString s
	  fun isName (ATOM{hash, id}) = (hash = h) andalso (id = s)
	  fun mk () = let
		val tbl = !table
		val sz = Array.length tbl
	      (* grow the table by doubling its size *)
		fun growTable () = let
		      val newSz = sz+sz
		      val newMask = Word.fromInt newSz - 0w1
		      val newTbl = Array.array(newSz, [])
		      fun ins (item as ATOM{hash, ...}) = let
			    val indx = Word.toIntX(Word.andb(hash, newMask))
			    in
			      Array.update (newTbl, indx,
				item :: Array.sub(newTbl, indx))
			    end
		      val appins = app ins
		      fun insert i = (appins (Array.sub(tbl, i)); insert(i+1))
		      in
			(insert 0) handle _ => ();
			table := newTbl
		      end
		in
		  if (!numItems >= sz)
		    then (growTable(); mk())
		    else let
		      val indx = Word.toIntX(Word.andb(h, Word.fromInt sz - 0w1))
		      fun look [] = let
			    val newName = ATOM{hash = h, id = s}
			    in
			      numItems := !numItems + 1;
			      Array.update(tbl, indx, newName :: Array.sub(tbl, indx));
			      newName
			    end
			| look (name::r) = if (isName name) then name else look r
		      in
			look (Array.sub(tbl, indx))
		      end
		end
	  in
	    mk()
	  end

  (* eventually, we should hash the substring and check for prior definition
   * before creating the string.
   *)
    fun atom' ss = atom(Substring.string ss)

  end (* signature ATOM *)
