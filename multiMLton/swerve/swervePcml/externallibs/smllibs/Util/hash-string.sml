(* hash-string.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *)

structure HashString : sig

    val hashString : string -> word

  end = struct

    fun charToWord c = Word.fromInt(Char.ord c)

  (* A function to hash a character.  The computation is:
   *
   *   h = 33 * h + 720 + c
   *)
    fun hashChar (c, h) = Word.<<(h, 0w5) + h + 0w720 + (charToWord c)

(* NOTE: another function we might try is h = 5*h + c, which is used
 * in STL.
 *)

    fun hashString s = CharVector.foldl hashChar 0w0 s
	  
  end (* HashString *)
