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

functor ConfigLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Config_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*  Copyright (c) 2001 Anthony L Shipman *)

(* $Id: config.grm,v 1.3 2001/05/15 01:52:57 felix Exp $ *)

(*  This is the grammar for the configuration file language.

*)

    open Common
    open ConfigTypes


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\007\000\002\000\006\000\008\000\005\000\000\000\
\\001\000\003\000\026\000\009\000\022\000\010\000\021\000\011\000\020\000\000\000\
\\001\000\005\000\013\000\000\000\
\\001\000\005\000\016\000\000\000\
\\001\000\006\000\024\000\009\000\011\000\000\000\
\\001\000\006\000\027\000\009\000\011\000\000\000\
\\001\000\007\000\015\000\000\000\
\\001\000\009\000\011\000\000\000\
\\001\000\009\000\012\000\000\000\
\\001\000\009\000\022\000\010\000\021\000\011\000\020\000\000\000\
\\001\000\012\000\000\000\000\000\
\\029\000\001\000\007\000\002\000\006\000\000\000\
\\030\000\009\000\011\000\000\000\
\\031\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\"
val actionRowNumbers =
"\000\000\013\000\011\000\007\000\
\\008\000\002\000\014\000\017\000\
\\012\000\006\000\003\000\007\000\
\\018\000\009\000\007\000\004\000\
\\020\000\001\000\023\000\022\000\
\\024\000\005\000\015\000\021\000\
\\019\000\016\000\010\000"
val gotoT =
"\
\\001\000\026\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\003\000\006\000\000\000\
\\004\000\008\000\005\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\012\000\000\000\
\\000\000\
\\000\000\
\\004\000\015\000\005\000\007\000\000\000\
\\000\000\
\\006\000\017\000\007\000\016\000\000\000\
\\004\000\021\000\005\000\007\000\000\000\
\\005\000\012\000\000\000\
\\000\000\
\\007\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 27
val numrules = 14
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = Common.SrcPos
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | TOK_INT of  (int)
 | TOK_STRING of  (string) | TOK_WORD of  (string)
 | literal of  (Literal) | literal_list of  (Literal list)
 | part of  (SectionPart) | part_list of  (SectionPart list)
 | section of  (Section) | section_list of  (Section list)
 | start of  (Section list)
end
type svalue = MlyValue.svalue
type result = Section list
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_change = 
nil
val noShift = 
fn (T 11) => true | _ => false
val showTerminal =
fn (T 0) => "KW_SERVER"
  | (T 1) => "KW_NODE"
  | (T 2) => "SYM_SEMICOLON"
  | (T 3) => "SYM_COMMA"
  | (T 4) => "SYM_LBRACE"
  | (T 5) => "SYM_RBRACE"
  | (T 6) => "SYM_EQUALS"
  | (T 7) => "SYM_SWERVE"
  | (T 8) => "TOK_WORD"
  | (T 9) => "TOK_STRING"
  | (T 10) => "TOK_INT"
  | (T 11) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 11) :: nil
end
structure Actions =
struct 
type int = Int.int
exception mlyAction of int
local open Header in
val actions = 
fn (i392:int,defaultPos,stack,
    (file):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.section_list section_list,section_list1left,
section_list1right))::rest671) => let val result=MlyValue.start((
section_list))
 in (LrTable.NT 0,(result,section_list1left,section_list1right),
rest671) end
| (1,(_,(MlyValue.part_list part_list,_,part_list1right))::(_,(_,
SYM_SWERVE1left,_))::rest671) => let val result=MlyValue.start((
[SectSwerve {
					    parts = part_list
					    }]))
 in (LrTable.NT 0,(result,SYM_SWERVE1left,part_list1right),rest671)
 end
| (2,(_,(MlyValue.section section,section1left,section1right))::
rest671) => let val result=MlyValue.section_list(([section]))
 in (LrTable.NT 1,(result,section1left,section1right),rest671) end
| (3,(_,(MlyValue.section section,_,section1right))::(_,(
MlyValue.section_list section_list,section_list1left,_))::rest671) => 
let val result=MlyValue.section_list((section_list @ [section]))
 in (LrTable.NT 1,(result,section_list1left,section1right),rest671)
 end
| (4,(_,(_,_,SYM_RBRACE1right))::(_,(MlyValue.part_list part_list,_,_)
)::_::(_,(_,KW_SERVERleft as KW_SERVER1left,_))::rest671) => let val 
result=MlyValue.section((
SectServer {
					    parts = part_list,
					    pos   = KW_SERVERleft
					    }
))
 in (LrTable.NT 2,(result,KW_SERVER1left,SYM_RBRACE1right),rest671)
 end
| (5,(_,(_,_,SYM_RBRACE1right))::(_,(MlyValue.part_list part_list,_,_)
)::_::(_,(MlyValue.TOK_WORD TOK_WORD,_,_))::(_,(_,KW_NODEleft as 
KW_NODE1left,_))::rest671) => let val result=MlyValue.section((
SectNode {
					    path  = TOK_WORD,
					    parts = part_list,
					    pos   = KW_NODEleft
					    }
))
 in (LrTable.NT 2,(result,KW_NODE1left,SYM_RBRACE1right),rest671) end
| (6,(_,(MlyValue.part part,part1left,part1right))::rest671) => let 
val result=MlyValue.part_list(([part]))
 in (LrTable.NT 3,(result,part1left,part1right),rest671) end
| (7,(_,(MlyValue.part part,_,part1right))::(_,(MlyValue.part_list 
part_list,part_list1left,_))::rest671) => let val result=
MlyValue.part_list((part_list @ [part]))
 in (LrTable.NT 3,(result,part_list1left,part1right),rest671) end
| (8,(_,(_,_,SYM_SEMICOLON1right))::(_,(MlyValue.literal_list 
literal_list,_,_))::_::(_,(MlyValue.TOK_WORD TOK_WORD,TOK_WORDleft as 
TOK_WORD1left,_))::rest671) => let val result=MlyValue.part((
SectionPart {
					    left  = TOK_WORD,
					    right = literal_list,
					    pos   = TOK_WORDleft
					    }
))
 in (LrTable.NT 4,(result,TOK_WORD1left,SYM_SEMICOLON1right),rest671)
 end
| (9,(_,(MlyValue.literal literal,literal1left,literal1right))::
rest671) => let val result=MlyValue.literal_list(([literal]))
 in (LrTable.NT 5,(result,literal1left,literal1right),rest671) end
| (10,(_,(MlyValue.literal literal,_,literal1right))::(_,(
MlyValue.literal_list literal_list,literal_list1left,_))::rest671) => 
let val result=MlyValue.literal_list((literal_list @ [literal]))
 in (LrTable.NT 5,(result,literal_list1left,literal1right),rest671)
 end
| (11,(_,(MlyValue.TOK_STRING TOK_STRING,TOK_STRINGleft as 
TOK_STRING1left,TOK_STRING1right))::rest671) => let val result=
MlyValue.literal((LitIsString (TOK_STRING, TOK_STRINGleft)))
 in (LrTable.NT 6,(result,TOK_STRING1left,TOK_STRING1right),rest671)
 end
| (12,(_,(MlyValue.TOK_INT TOK_INT,TOK_INTleft as TOK_INT1left,
TOK_INT1right))::rest671) => let val result=MlyValue.literal((
LitIsInt (TOK_INT, TOK_INTleft)))
 in (LrTable.NT 6,(result,TOK_INT1left,TOK_INT1right),rest671) end
| (13,(_,(MlyValue.TOK_WORD TOK_WORD,TOK_WORDleft as TOK_WORD1left,
TOK_WORD1right))::rest671) => let val result=MlyValue.literal((
LitIsString (TOK_WORD, TOK_WORDleft)))
 in (LrTable.NT 6,(result,TOK_WORD1left,TOK_WORD1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Config_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun KW_SERVER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_NODE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SYM_SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun SYM_COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SYM_LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SYM_RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SYM_EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun SYM_SWERVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_WORD (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.TOK_WORD i,p1,p2))
fun TOK_STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.TOK_STRING i,p1,p2))
fun TOK_INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.TOK_INT i,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
end
end
