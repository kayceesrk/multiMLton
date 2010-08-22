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

functor MimeTypeFn (structure TextFrag: TEXT_FRAG) :> MIME_TYPE where type TF.t = TextFrag.t =
struct

  structure TF = TextFrag
		 
  type param = string * string
	       
  datatype t = MimeType of { mtype: string,
			     msubtype: string,
			     mparams: param list }  
	     | Unknown
	       
  val semi  = TF.str ";"
  val equal = TF.str "="
  val slash = TF.str "/"
	      
  fun mkMimeType m msub ps = MimeType {mtype = m, msubtype = msub, mparams = ps}
			     
  fun simpleType maj min = MimeType {mtype = maj, msubtype = min, mparams = []}
			
  fun fromString path =
      case Files.splitExt path of
	  ( _, SOME ext ) => ( case Config.lookupMime ext of
				   NONE            => simpleType "text" "plain"
				 | SOME (maj, min) => simpleType maj min )
	| _ => simpleType "text" "plain"

  fun isUnknown mtype = 
      case mtype of
	  Unknown => true
	| _ => false
	       
  fun format mime =
      case mime of
	  Unknown => TF.empty
	| MimeType {mtype, msubtype, mparams} =>
	  let fun parm (k,v) = TF.seq [semi, TF.str k, equal, TF.str v]
	      fun noparms () = TF.seq [TF.str mtype, slash, TF.str msubtype]
	  in
	      case mparams of
		  [] => noparms ()
		| _ => TF.seq [noparms (), TF.seq (List.map parm mparams)]
	  end
	  
end

structure MimeType = MimeTypeFn (structure TextFrag = TextFrag)
	 
