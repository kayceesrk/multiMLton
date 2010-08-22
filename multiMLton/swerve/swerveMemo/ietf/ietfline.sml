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

functor IETFLineFn (structure IPart: IETF_PART where type part = IETFLex.UserDeclarations.P.part)
	:> IETF_LINE where type IP.part = IETFPart.part =
struct
structure SS = Substring
structure IL = IETFLex
structure IP = IPart
	       
(*	Run the string through the header lexer. *)
fun split str =
    let val done = ref false
	fun input n = if !done 
		      then "" 
		      else (done := true; str)
	val lexer = IL.makeLexer input			
	fun read toks = case lexer() of
			    IP.EOF => rev toks
			  | t       => read (t::toks)
    in
	read []
    end
    
(* concat all the parts together into a string.
	  slight twist of dropping trailing whitespace. *)
fun join parts = 
    
    let fun safe c = not (Char.isCntrl c orelse
			 Char.isSpace c orelse
			 Char.contains "()<>@,;:\\\"/[]?={}" c)
		     
	fun strip_dq str =
	    let
		val fields = SS.fields (fn c => c = #"\"") (SS.full str)
	    in
		concat("\"" :: ((map SS.string fields) @ ["\""]))
	    end		
	    
	fun quote str = 
	    let val (_, right) = SS.splitl safe (SS.full str)
	    in
		(* If there are unsafe characters then right won't be empty. *)
		if SS.isEmpty right
		then str
		else strip_dq str
	    end
	    
	fun toString p = 
	    case p of
		IP.Token s => quote s
	      | IP.Spec c => String.str c 
	      | IP.WS s => s
	      | IP.Bad _ => ""
	      | IP.EOF => ""
			  
	val skiptrailing = ref false 
			   
	fun isWS p = case p of 
			 IP.WS _ => true
		       | _ => false
			      
	fun join (p,ps) = if !skiptrailing andalso isWS p
			  then ps
			  else (skiptrailing := false; 
				toString p ^ ps)
    in			   
	List.foldr join "" parts
    end
    
(* If a string contains special characters then quote the field.
   Control characters are not allowed.

   We can use the lexer to recognise the different kinds of 
   characters and just rejoin them with quoting. *)
fun quoteField field = join (split field)
		       
(* Dump for testing. *)
fun dump hparts =
    let fun put (IP.Token s1) = (print " Tok '"; print s1; print "'")
	  |   put (IP.Spec c)  = (print " TSpec '"; print (str c); print "'")
	  |   put (IP.WS s)    = (print " TWs '"; print s; print "'")
	  |   put (IP.Bad  c)  = (print " TBad")
	  |   put  IP.EOF      = (print " TEOF")
    in
	app put hparts
    end
    
end

structure IETFLine = IETFLineFn (structure IPart = IETFPart)
