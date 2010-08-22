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

(* This defines a type for efficiently representing a collection
 * of text fragments.  It is based on the wseq type of ML Server Pages
 * from Moscow ML. 
						       
 * The goal is to delay the concatenation of strings as much as possible.
 * Building trees is much faster. *)

functor TextFragFn (structure SEP: LINE_SEPARATOR) :> TEXT_FRAG =
struct

structure S = SEP

datatype t =
	 Empty
       | NL
       | Space
       | Str of string
       | Text of t list

val nl = NL

val sp = Space

val empty = Empty

val crlf = Str "\r\n"

fun str s = Str s

fun seq text = Text text

fun concat strs = seq (List.map str strs)

fun seqSep sep lst =
    case lst of
	[] => Empty
      | e :: [] => e
      | e1 :: es => Text (e1 :: (List.foldr (fn (e,es) => sep :: e :: es) [] es))

fun toString t = 
    case t of
	Empty => ""
      | NL => S.sep
      | Space => " "
      | Str s => s
      | Text t => List.foldr (fn (t,s) => toString t ^ s) "" t
		    
fun length t =
    case t of
	Empty => 0
      | NL => S.length
      | Space => 1
      | Str s => String.size s
      | Text t => List.foldl (fn (t,sum) => length t + sum) 0 t

fun app f txt =
    case txt of 
	Empty => ()
      | NL => f S.sep
      | Space => f " "
      | Str s => f s
      | Text txt => List.app (app f) txt


end

structure TextFrag = TextFragFn (structure SEP = CRLFSeparator)
