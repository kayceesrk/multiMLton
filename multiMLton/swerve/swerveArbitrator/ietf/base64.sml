(*
Original Code - Copyright (c) 2001 Anthony L Shipman
MLton Port Modifications - Copyright (c) Ray Racine

Permission is granted to anyone to use this version of the software
for any purpose, including commercial applications, and to alter it and
redistribute it freely, subject to the following restrictions:

    1. Redistributions in source code must retain the above copyright
    notice, this list of conditions, and the following disclaimer.

    2. The origin of this software must not be misrepresented; you must
    not claim that you wrote the original software. If you use this
    software in a product, an acknowledgment in the product documentation
    would be appreciated but is not required.

    3. If any files are modified, you must cause the modified files to
    carry prominent notices stating that you changed the files and the
    date of any change.

Disclaimer

    THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
    INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
    IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

Modification History
====================
Ray Racine 6/3/2005 - MLton Port and idiomatic fixups.
*)

(* Cut and Pasted up of the orginal in Swerve.
 * I don't think this is too efficient and should be replaced later.
 * MLton has a Base64 as well.  However, it is an internal one
 * which uses lots of proprietary routines.
 * Deal with this later IF I ever get Swerve ported. RPR *)

structure Base64: BASE64 =
struct

    structure SS = Substring

    type base64 = string

    structure Decode: sig
	val cvt: char -> word
	val get: substring -> int -> word -> int * word
	val split: word -> int -> char list -> char list
	val push: substring -> char list -> char list
	val base64Decode: string -> string option
      end = struct
    
    fun  cvt ch = if ch >= #"A" andalso ch <= #"Z"
		  then Word.fromInt((ord ch) - 65)
		  else if ch >= #"a" andalso ch <= #"z"
		  then Word.fromInt((ord ch) - 71)
		  else if ch >= #"0" andalso ch <= #"9"
		  then Word.fromInt((ord ch) + 4)
		  else if ch = #"+"
		  then 0w62
		  else if ch = #"/"
		  then 0w63
		  else 0w0

    fun get ss numeq bits = case SS.getc ss of
				NONE => (numeq, bits)								    
			      | SOME (c, rest) => let val v = cvt c
						      val n' = if c = #"=" then numeq+1 else numeq
						      val b' = Word.orb(v, Word.<<(bits, 0w6))
						  in
						      get rest n' b'
						  end
						  	
    fun split bytes nc r = case nc of
			       0 => r
			     | _ => let val left = Word.>>(bytes, 0w24)	(* get left-most byte *)
					val rest = Word.<<(bytes, 0w8)
					val c = chr(Word.toInt left)
				    in
					split rest (nc-1) (c::r)
				    end
				     
    (*  Convert a substring of 4 chars to 3 chars and push them onto rslt. *)
    fun push (group: substring) (rslt: char list) = 
	let val (numeq, bytes) = get group 0 0w0
	in
	    split (Word.<<(bytes, 0w8)) (3 - numeq) rslt
	end
								
    (* This will return NONE if there is some error.
     * This will throw an exception if there aren't 0 or 4 chars left in ss. *)
    fun base64Decode str : string option =
	let fun loop ss rslt = if SS.isEmpty ss
			       then SOME(implode(rev rslt))
			       else let val four = SS.slice(ss, 0, SOME 4)
					val rest = SS.slice(ss, 4, NONE)
				    in
					loop rest (push four rslt)
				    end
	in
	    loop (SS.full str) []
	end handle _ => NONE

    end (* structure Decode *)
    

    structure Encode: sig
	val cvt: int -> char
	val padz: word -> int -> word
	val pade: int -> char list -> char list
	val push: word -> int -> char list -> char list
	val base64Encode: string -> string				    
    end = struct
    
    (*  Pad the acc with 0 bytes. *)
    fun padz acc 0 = acc
      | padz acc m = padz (Word.<<(acc, 0w8)) (m-1)		       

    (*  Pad with = chars at the end of the string. *)
    fun pade 0 rslt = rslt
      | pade n rslt = pade (n-1) (#"="::rslt)

    fun cvt b =	
	if b <= 25
	then chr(65 + b)
	else if b <= 51
	then chr(71 + b)
	else if b <= 61
	then chr(b - 4)
	else if b = 62
	then #"+"
	else #"/"

    (* Push n characters from acc onto rslt.
     * The input is padded with 0 to get 24 bits. *)
    fun push acc 0 rslt = rslt
      | push acc n rslt = let
	    val b = Word.>>(acc, 0w18)	(* top 6 bits *)
	    val acc' = Word.andb(Word.<<(acc, 0w6), 0wxffffff)
	    val c = cvt(Word.toInt b)
	in
	    push acc' (n-1) (c::rslt)
	end
			  	     
    fun base64Encode the_str = 
	(* Grab groups of 3 chars. 
	 * n is the number of chars to add to a group. *)	
	let fun loop ss 0 acc rslt = loop ss 3 0w0 (push acc 4 rslt) (* end of a group *)
	      | loop ss n acc rslt =    (* add to a group, n=1,2,3 *)
		(case SS.getc ss of
		     (* no more chars, pad the group *)
		     NONE => let val final = if n = 3    (* group is empty *)
					     then rslt
					     (* flush the group, n=1,2 *)
					     else let val acc' = padz acc n
						  in
						      pade n (push acc' (4-n) rslt)
						  end
			     in
				 implode(rev final) (* RPR wow, this is done as a reversed list.  Could be faster else where.  Someday use MLtons for example. *)
			     end			     
		   | SOME (c, rest) => let val b = Word.fromInt(ord c)
					   val acc' = Word.orb(Word.<<(acc, 0w8), b)
				       in
					   loop rest (n-1) acc' rslt
				       end)
	in
	    if the_str = ""
	    then the_str	(* spec appears to be undefined on this case *)
	    else loop (SS.full the_str) 3 0w0 []
	end
	
    end
	  
    val decode = Decode.base64Decode
    val encode = Encode.base64Encode
    
    fun toString b64 = b64

end

