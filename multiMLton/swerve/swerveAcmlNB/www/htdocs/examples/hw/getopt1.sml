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


(*  This is the "mostly-functional" getopt program. *)

structure Main=
struct


    (*	The options will be returned as a list of pairs
	of name and value.  We need to use an option type for
	the value so that we can distinguish between a missing
	value and an empty value.

    *)
    type Option = string * string option

    (*	The result from the command line parsing will
	be a list of file names and a set of options.
    *)
    type CmdLine = (Option list) * (string list)

    (*	This exception will bomb with a usage message. *)
    exception Usage of string


    fun parse_cmdline argv : CmdLine =
    let
	fun loop [] opts = (opts, [])	    (* no more args *)

	|   loop ("-h"::rest) opts =
		loop rest (("help", NONE) :: opts)

	|   loop ("-v"::rest)        opts =
		loop rest (("verbose", NONE) :: opts)

	|   loop ("--verbose"::rest) opts =
		loop rest (("verbose", NONE) :: opts)

	|   loop ("--width"::rest)  opts =
		get_value "width"  rest opts

	|   loop ("--height"::rest) opts =
		get_value "height" rest opts

	|   loop (arg::rest) opts =
	(
	    if String.sub(arg, 0) = #"-"
	    then
		raise Usage (concat["The option ", arg,
				" is unrecognised."])
	    else
		(opts, arg::rest)	    (* the final result *)
	)

	and get_value name [] opts =
	(
	    raise Usage (concat[
		"The value for the option ", name, " is missing."])
	)

	|   get_value name (v::rest) opts =
	(
	    loop rest ((name, SOME v) :: opts)
	)
    in
	loop argv []
    end



    and find_option opts name : (string option) option =
    (
	case List.find (fn (n, v) => n = name) opts of
	  NONE => NONE
	| SOME (n, v) => SOME v
    )


    and has_option opts name =
	(find_option opts name) <> NONE


    and require_option opts name and_value : string =
    (
	case find_option opts name of
	  NONE => raise Usage (concat[
	  		"The option '", name,
			"' is missing."])

	| SOME NONE =>	(* found but has no value *)
	(
	    if and_value 
	    then
		raise Usage (concat[
		    "The option '", name, "' is missing a value."])
	    else
		""
	)

	| SOME (SOME v) => v  (* found and has a value *)
    )



    fun main(arg0, argv) =
    let
	val (opts, files) = parse_cmdline argv

	val width  = require_option opts "width"  true
	val height = require_option opts "height" true

	fun show_stuff() =
	(
	    print "The files are";
	    app (fn f => (print " "; print f)) files;
	    print ".\n";

	    if has_option opts "verbose"
	    then
		print(concat[
		    "The width is ",  width,  ".\n",
		    "The height is ", height, ".\n"
		    ])
	    else
		()
	)
    in
	if has_option opts "help"
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


    val _ = SMLofNJ.exportFn("getopt1", main)
end
