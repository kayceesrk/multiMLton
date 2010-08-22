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

type int = Int.int
structure IETFLex=
   struct
    structure UserDeclarations =
      struct
(*  Copyright (c) 2001 Anthony L Shipman *)

(* $Id: ietf.lex,v 1.2 2001/07/07 19:12:48 felix Exp $ *)

(* This contains the lex file for lines in IETF messages such as HTTP
    headers. This is just a short cut to splitting a line into tokens,
    symbols and quoted strings.  We assume that there are no CRLF
    characters.

    The types are accessible directly out of IETFLex.UserDeclarations.*)

    structure P = IETFPart

    type lexresult = P.part

    fun eof() = P.EOF

    (*	Strip off the surrounding quotes. *)
    fun fix_str s = P.Token(String.substring(s, 1, size s - 2))

(*--------------------------------------------------------------------------*)

(*  %full selects an 8 bit character set.
    token chars are all but ()<>@,;:\\"/[]?={} \t\000-\031\127
    Due to a bug in ml-lex we can't include a ] in a character set.
    The \h stands for all characters 0x80 and up. *)

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\025\000\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\025\004\022\004\004\004\004\004\021\020\004\004\019\004\004\018\
\\004\004\004\004\004\004\004\004\004\004\017\016\015\014\013\012\
\\011\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\010\009\008\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\007\004\006\004\003\
\\004\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\000\005\005\005\005\005\000\000\005\005\000\005\005\000\
\\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\005\000\005\000\
\\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\023\023\024\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\000\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\
\\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023\023"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 45)], trans = 0},
{fin = [(N 5),(N 45)], trans = 4},
{fin = [(N 5)], trans = 4},
{fin = [(N 43),(N 45)], trans = 0},
{fin = [(N 41),(N 45)], trans = 0},
{fin = [(N 35),(N 45)], trans = 0},
{fin = [(N 27),(N 45)], trans = 0},
{fin = [(N 33),(N 45)], trans = 0},
{fin = [(N 19),(N 45)], trans = 0},
{fin = [(N 37),(N 45)], trans = 0},
{fin = [(N 17),(N 45)], trans = 0},
{fin = [(N 39),(N 45)], trans = 0},
{fin = [(N 15),(N 45)], trans = 0},
{fin = [(N 23),(N 45)], trans = 0},
{fin = [(N 25),(N 45)], trans = 0},
{fin = [(N 31),(N 45)], trans = 0},
{fin = [(N 21),(N 45)], trans = 0},
{fin = [(N 13),(N 45)], trans = 0},
{fin = [(N 11),(N 45)], trans = 0},
{fin = [(N 29),(N 45)], trans = 22},
{fin = [], trans = 22},
{fin = [(N 9)], trans = 0},
{fin = [(N 2),(N 45)], trans = 25},
{fin = [(N 2)], trans = 25}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

type int = Int.int
fun makeLexer (yyinput: int -> string) =
let	val yygone0:int=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl: int ref = ref 1		(*buffer length *)
	val yybufpos: int ref = ref 1		(* location of next character to use *)
	val yygone: int ref = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin: int ref = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0: int) =
	let fun action (i: int,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos: int = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  11 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 13 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 15 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 17 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 19 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 2 => let val yytext=yymktext() in P.WS yytext end
| 21 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 23 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 25 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 27 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 29 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 31 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 33 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 35 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 37 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 39 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 41 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 43 => let val yytext=yymktext() in P.Spec (String.sub(yytext, 0)) end
| 45 => let val yytext=yymktext() in P.Bad (String.sub(yytext, 0)) end
| 5 => let val yytext=yymktext() in P.Token yytext end
| 9 => let val yytext=yymktext() in fix_str yytext end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub (Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord (CharVector.sub (!yyb,l))
		val NewState = Char.ord (CharVector.sub (trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
