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

(*  Copyright (c) 2001 Anthony L Shipman *)


structure Common =
struct

(*-------------------------------------------------*)
(*  A hash table with string keys. *)

    structure STRT_key =
    struct
	type hash_key = string
	val hashVal = HashString.hashString
	fun sameKey (s1, s2) = (s1 = s2)
    end

    structure STRT = HashTableFn(STRT_key)

    exception NotFound

(*-------------------------------------------------*)

end


signature GLOBAL =
sig
    type Option = string option

    (*	Add an option to the table silently overriding
	an existing entry.
    *)
    val addOption: (string * Option) -> unit

    (*	Test if an option is in the table. *)
    val hasOption: string -> bool

    (*	Get the value of an option if it exists. *)
    val getOption: string -> Option option

end


structure Global: GLOBAL =
struct
    open Common

(*-------------------------------------------------*)
(*  The option table. *)

    type Option = string option

    type OptionTable = Option STRT.hash_table

    val option_tbl: OptionTable = STRT.mkTable(20, NotFound)

    fun addOption arg  = STRT.insert option_tbl arg

    fun hasOption name = STRT.find option_tbl name <> NONE

    fun getOption name = STRT.find option_tbl name

(*-------------------------------------------------*)

end


structure Main=
struct

    (* This exception will bomb with a usage message. *)
    exception Usage of string


    fun parse_cmdline argv : string list =
    let
	fun loop [] = []    	    (* no more arguments *)

	|   loop ("-h"::rest) = add ("help", NONE) rest

	|   loop ("-v"::rest) = add ("verbose", NONE) rest

	|   loop ("--verbose"::rest) = add ("verbose", NONE) rest

	|   loop ("--width"::rest)  = get_value "width"  rest

	|   loop ("--height"::rest) = get_value "height" rest

	|   loop (arg::rest) =
	(
	    if String.sub(arg, 0) = #"-"
	    then
		raise Usage (concat[
			"The option ", arg, " is unrecognised."])
	    else
		arg::rest   	    (* the final result *)
	)

	and get_value name [] =
	(
	    raise Usage (concat["The value for the option ",
	                        name, " is missing."])
	)
	|   get_value name (v::rest) = add (name, SOME v) rest

	and add pair rest =
	(
	    Global.addOption pair;
	    loop rest
	)
    in
	loop argv
    end


    fun require_option name and_value : string =
    (
	case Global.getOption name of
	  NONE => raise Usage (concat[
		    "The option '", name, "' is missing."])

	| SOME NONE =>        	    (* found but no value *)
	(
	    if and_value 
	    then
		raise Usage (concat["The option '", name,
				    "' is missing a value."])
	    else
		""
	)

	| SOME (SOME v) => v	    (* found with a value *)
    )



    fun main(arg0, argv) =
    let
	val files = parse_cmdline argv

	val width  = require_option "width"  true
	val height = require_option "height" true

	fun show_stuff() =
	(
	    print "The files are";
	    app (fn f => (print " "; print f)) files;
	    print ".\n";

	    if Global.hasOption "verbose"
	    then
		print(concat[
		    "The width is ",  width,  ".\n",
		    "The height is ", height, ".\n"
		    ])
	    else
		()
	)
    in
	if Global.hasOption "help"
	then
	    print "some helpful blurb\n"
	else
	    show_stuff();

        OS.Process.success
    end
    handle Usage msg =>
    (
	TextIO.output(TextIO.stdErr, concat[msg,
	 "\nUsage: [-h] [-v|--verbose] [--width width]",
	 " [--height height] files\n"]);
        OS.Process.failure
    )


    val _ = SMLofNJ.exportFn("getopt2", main)
end
