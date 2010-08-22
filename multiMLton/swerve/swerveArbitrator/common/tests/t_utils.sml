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

(*  Copyright (c) 2001 Anthony L Shipman *)

(* $Id: t_utils.sml,v 1.3 2001/06/07 01:40:50 felix Exp $ *)

signature TEST_UTILS =
sig
    val expectS:	string -> string -> string -> unit
    val expectSOpt:	string -> string option -> string -> unit
    val expectI:	int -> int -> string -> unit
    val expectIOpt:	int -> int option -> string -> unit
    val expectEmpty:	'a option -> string -> unit

    val fail:	string -> unit

    val numFailures:	unit -> int
end



structure TestUtils: TEST_UTILS =
struct

(*------------------------------------------------------------------------------*)

    val failures = ref 0



    fun expectS str s2 msg =
    (
	if str = s2
	then
	    ()
	else
	    fail (concat[msg, ", expected '", str, "', found '", s2, "'"])
    )



    and expectSOpt str opt msg =
    (
	case opt of
	  NONE => fail (msg ^ ", the field is NONE")

	| SOME s => 
	(
	    if s = str
	    then
		()
	    else
		fail (concat[msg, ", expected '", str,
			         "', found '", s, "'"])
	)
    )


    and expectI i1 i2 msg =
    (
	if i1 = i2
	then
	    ()
	else
	    fail (concat[msg, ", expected ", Int.toString i1,
	                      ", found ", Int.toString i2])
    )




    and expectIOpt i opt msg =
    (
	case opt of
	  NONE => fail (msg ^ ", the field is NONE")

	| SOME j => 
	(
	    if i = j
	    then
		()
	    else
		fail (concat[msg, ", expected '", Int.toString i,
			         "', found '", Int.toString j, "'"])
	)
    )


    and expectEmpty opt msg =
    (
	case opt of
	  NONE => ()

	| SOME s => fail (concat[msg, ", expected it to be empty"])
    )


    and fail msg =
    (
	failures := !failures + 1;
	TextIO.output(TextIO.stdErr, "FAILURE " ^ msg ^ "\n")
    )


    and numFailures() = !failures

(*------------------------------------------------------------------------------*)

end
