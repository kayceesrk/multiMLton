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

%%
%structure IETFLex
%full

ctl=[\000-\031\127];
ws=[\ \t];
tokn=[!#$%&'*+.0-9A-Z^_`a-z|~\h-];
str=[^\000-\031\127"];

%%

{ws}+		=> (P.WS yytext);
{tokn}+		=> (P.Token yytext);
\"{str}*\"	=> (fix_str yytext);

"("		=> (P.Spec (String.sub(yytext, 0)));
")"		=> (P.Spec (String.sub(yytext, 0)));
"<"		=> (P.Spec (String.sub(yytext, 0)));
">"		=> (P.Spec (String.sub(yytext, 0)));
"@"		=> (P.Spec (String.sub(yytext, 0)));
","		=> (P.Spec (String.sub(yytext, 0)));
";"		=> (P.Spec (String.sub(yytext, 0)));
":"		=> (P.Spec (String.sub(yytext, 0)));
"\\"		=> (P.Spec (String.sub(yytext, 0)));
"\""		=> (P.Spec (String.sub(yytext, 0)));
"/"		=> (P.Spec (String.sub(yytext, 0)));
"["		=> (P.Spec (String.sub(yytext, 0)));
"]"		=> (P.Spec (String.sub(yytext, 0)));
"?"		=> (P.Spec (String.sub(yytext, 0)));
"="		=> (P.Spec (String.sub(yytext, 0)));
"{"		=> (P.Spec (String.sub(yytext, 0)));
"}"		=> (P.Spec (String.sub(yytext, 0)));

.		=> (P.Bad (String.sub(yytext, 0)));
