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

%%
%header (functor ConfigLexFun(structure Tokens: Config_TOKENS));
%full
%arg (file: string);

wrd1=[A-Za-z_/\\$:.%+-];
wrd=[A-Za-z0-9_/\\$:.%+-];
word={wrd1}{wrd}*;
str=([^"\n]|\\\n|\\\"|\\\\);
digit=[0-9];
int=[+-]?{digit}+;

ws=[\ \t\013];

%%

"\n"		=> (new_line yypos; continue());
{ws}+		=> (continue());
#.*\n		=> (new_line yypos; continue());


{word}		=> (check_reserved yytext file yypos);

{int}		=> (fix_integer yytext file yypos);

\"{str}*\"	=> (fix_str yytext file yypos);

";"		=> (sym Tokens.SYM_SEMICOLON file yypos);
","		=> (sym Tokens.SYM_COMMA file yypos);
"{"		=> (sym Tokens.SYM_LBRACE file yypos);
"}"		=> (sym Tokens.SYM_RBRACE file yypos);
"="		=> (sym Tokens.SYM_EQUALS file yypos);
"\001\001\001"	=> (sym Tokens.SYM_SWERVE file yypos);


.		=> (Log.errorP (get_pos file yypos)
			["Unrecognised characters in the configuration file."];
		    eof file
		    );
