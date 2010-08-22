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
functor ConfigLexFun(structure Tokens: Config_TOKENS)=
   struct
    structure UserDeclarations =
      struct
(*  Copyright (c) 2001 Anthony L Shipman *)

(* $Id: config.lex,v 1.10 2001/09/03 19:36:33 felix Exp $ *)

(*
    This contains the lex file for the configuration language.

    The text is free format with white space anywhere. We break 
    the text up into the following

    	words	 - a sequence of alphanumeric characters including:
			/ \ $ _ : .
	symbols  - e.g. braces and semicolons
	strings	 - enclosed in double quotes, with \ escapes
	integers - digits with an optional +/-

    We have to make the section kinds, Server and Node, be reserved words
    so that they can direct the parsing.

    When we are parsing .swerve files we only do a subset of the full
    configuration grammar.  This is indicated by forcing the magic symbol
    ^A^A^A onto the front of the .swerve file.

*)

    val	    line = ref 1		(* current line *)
    val	    line_pos = ref 0		(* char position of preceding \n *)

    fun get_pos file yypos =
    let
	val col = Int.max(yypos - !line_pos, 1)	(* see eof *)
    in
	Common.SrcPos {file=file, line= (!line), col=col}
    end


    fun new_line yypos =
    (
	line := !line + 1;
	line_pos := yypos
    )



    (*	Strip off the leading and trailing quotes. Translate the 
	\ escapes.  Any embedded new-lines must be counted along with
	their positions.
    *)

    fun fix_str yytext file yypos =
    let
	val pos = get_pos file yypos
	val chars = explode(substring(yytext, 1, size yytext - 2))

	fun count_nl [] pp = ()
	|   count_nl (#"\n"::rest) pp = (new_line pp; count_nl rest (pp+1))
	|   count_nl (c::rest) pp     = count_nl rest (pp+1)

	fun xlate [] rslt = implode(rev rslt)
	|   xlate (#"\\"::c::rest) rslt =
	let
	    val nc =
		case c of
		  #"n" => #"\n"
		| #"t" => #"\t"
		| _    => c
	in
	    xlate rest (nc::rslt)
	end
	|   xlate (c::rest) rslt = xlate rest (c::rslt)
    in
	count_nl chars (yypos+1);
	Tokens.TOK_STRING(xlate chars [], pos, pos)
    end


    (*	Convert the digits to an integer. 
	Report any overflow.
    *)

    fun fix_integer yytext file yypos =
    let
	val pos = get_pos file yypos

	val value = (valOf(Int.fromString yytext))
		handle _ => (Log.errorP pos ["The decimal number is too large."]; 0)
    in
	Tokens.TOK_INT(value, pos, pos)
    end



    fun sym tok file yypos =
    let
	val pos = get_pos file yypos
    in
	tok(pos, pos)
    end


    val reserved_words = [
	("SERVER",	Tokens.KW_SERVER),
	("NODE",	Tokens.KW_NODE)
	]


    fun check_reserved yytext file yypos =
    let
	val uword = Common.upperCase yytext
	val pos = get_pos file yypos
    in
	case List.find (fn (w, _) => w = uword) reserved_words of
	  NONE          => Tokens.TOK_WORD(yytext, pos, pos)
	| SOME (_, tok) => tok(pos, pos)
    end

(*--------------------------------------------------------------------------*)

    (*	These definitions are required by the parser.
	The lexer types are supplied by the grammar.
    *)

    type    pos = Common.SrcPos
    type    arg = string		(* type from %arg below *)

    type svalue = Tokens.svalue
    type ('a,'b) token = ('a,'b) Tokens.token
    type lexresult= (svalue,pos) token

    fun eof file = Tokens.EOF(get_pos file 0, get_pos file 0)

(*--------------------------------------------------------------------------*)

(*  %full selects an 8 bit character set.
*)

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
"\003\026\003\003\003\003\003\003\003\023\025\003\003\023\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\023\003\018\015\006\006\003\003\003\003\003\012\014\012\006\006\
\\010\010\010\010\010\010\010\010\010\010\006\009\003\008\003\003\
\\003\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\003\006\003\003\006\
\\003\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\005\003\004\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\007\007\000\000\000\000\000\007\000\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\007\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
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
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\007\007\000\000\000\000\000\007\000\007\007\007\
\\013\013\013\013\013\013\013\013\013\013\007\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\007\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (15, 
"\016\016\016\016\016\016\016\016\016\016\017\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016"
),
 (18, 
"\019\019\019\019\019\019\019\019\019\019\000\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\022\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\020\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019"
),
 (20, 
"\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\021\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\020\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\
\\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019\019"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\024\000\000\000\024\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
 (26, 
"\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
 (27, 
"\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
{fin = [(N 41)], trans = 0},
{fin = [(N 33),(N 41)], trans = 0},
{fin = [(N 31),(N 41)], trans = 0},
{fin = [(N 11),(N 41)], trans = 6},
{fin = [(N 11)], trans = 6},
{fin = [(N 35),(N 41)], trans = 0},
{fin = [(N 27),(N 41)], trans = 0},
{fin = [(N 15),(N 41)], trans = 10},
{fin = [(N 15)], trans = 10},
{fin = [(N 11),(N 41)], trans = 12},
{fin = [(N 11),(N 15)], trans = 12},
{fin = [(N 29),(N 41)], trans = 0},
{fin = [(N 41)], trans = 15},
{fin = [], trans = 15},
{fin = [(N 8)], trans = 0},
{fin = [(N 41)], trans = 18},
{fin = [], trans = 18},
{fin = [], trans = 20},
{fin = [(N 25)], trans = 18},
{fin = [(N 25)], trans = 0},
{fin = [(N 4),(N 41)], trans = 23},
{fin = [(N 4)], trans = 23},
{fin = [(N 1)], trans = 0},
{fin = [(N 41)], trans = 26},
{fin = [], trans = 27},
{fin = [(N 39)], trans = 0}])
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

fun lex (yyarg as (file: string)) =
let fun continue() : Internal.result = 
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

  1 => (new_line yypos; continue())
| 11 => let val yytext=yymktext() in check_reserved yytext file yypos end
| 15 => let val yytext=yymktext() in fix_integer yytext file yypos end
| 25 => let val yytext=yymktext() in fix_str yytext file yypos end
| 27 => (sym Tokens.SYM_SEMICOLON file yypos)
| 29 => (sym Tokens.SYM_COMMA file yypos)
| 31 => (sym Tokens.SYM_LBRACE file yypos)
| 33 => (sym Tokens.SYM_RBRACE file yypos)
| 35 => (sym Tokens.SYM_EQUALS file yypos)
| 39 => (sym Tokens.SYM_SWERVE file yypos)
| 4 => (continue())
| 41 => (Log.errorP (get_pos file yypos)
			["Unrecognised characters in the configuration file."];
		    eof file
		    )
| 8 => (new_line yypos; continue())
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
		        if (l=i0) then UserDeclarations.eof yyarg
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
in continue end
  in lex
  end
end
