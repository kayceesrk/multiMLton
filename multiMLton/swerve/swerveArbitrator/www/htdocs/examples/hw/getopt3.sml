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


signature OPTION =
sig

    (*	Parse the command line. Put options into a
	global variable and return the rest of the arguments.
	Error messages will be written to stdErr.

	This can raise Fail on an internal error.
    *)
    val parseCmdLine: string list -> string list

    (*	Return a usage message. *)
    val usage:	    unit -> string

    (*	Get the value of various options.  *)

    val hasVerbose: unit -> bool
    val hasHelp:    unit -> bool

    (*	These return NONE if the option did not appear
	on the command line.
    *)
    val getWidth:   unit-> string option
    val getHeight:  unit-> string option

end



structure Option: OPTION =
struct
    structure G = GetOpt

(*-----------------------------------------------------*)

    (*	This represents an option found on
        the command line.
    *)
    datatype Option = 
	    Verbose
	|   Help
	|   Width of string
	|   Height of string

    fun NoArg  opt       = G.NoArg (fn () => opt)
    fun ReqArg opt descr = G.ReqArg (opt, descr)

    val options: (Option G.opt_descr) list = [
	    {short = "v", long = ["verbose"],
		desc = NoArg Verbose,
		help = "Select verbose output"
	    },

	    {short = "", long = ["width"],
		desc = ReqArg Width "width",
		help = "The width in pixels"
	    },

	    {short = "", long = ["height"],
		desc = ReqArg Height "height",
		help = "The height in pixels"
	    },

	    {short = "h", long = ["help"],
		desc = NoArg Help,
		help = "Show this message."
	    }
	  ]
          
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

(*-----------------------------------------------------*)

    val opt_tbl: (Option list) ref = ref []


    fun parseCmdLine argv =
    let
	val (opts, files) =
	    G.getOpt {
		argOrder = G.RequireOrder,
		options  = options,
		errFn    = toErr
		} argv
    in
	opt_tbl := opts;
	files
    end


    fun usage() = G.usageInfo {
			header = "Usage: getopt",
			options = options
			}

    fun hasVerbose() =
    (
	List.exists (fn opt => opt = Verbose) (!opt_tbl)
    )

    fun hasHelp() =
    (
	List.exists (fn opt => opt = Help) (!opt_tbl)
    )

    fun getWidth()   =
    let
	val opt_width = List.find
			(fn Width _ => true | _ => false)
			(!opt_tbl)
    in
	case opt_width of
	  NONE          => NONE
	| SOME(Width w) => SOME w
	| _             => raise Fail "Option,getWidth"
    )

    fun getHeight()   =
    let
	val opt_height = List.find
			(fn Height _ => true | _ => false)
			(!opt_tbl)
    in
	case opt_height of
	  NONE           => NONE
	| SOME(Height h) => SOME h
	| _              => raise Fail "Option,getHeight"
    )

(*-----------------------------------------------------*)

end






structure Main=
struct

    exception Usage of string

    fun toErr msg = TextIO.output(TextIO.stdErr, msg)


    fun require_option func name : string =
    (
	case func() of
	  NONE => raise Usage (concat[
	  		    "The option '", name,
			    "' is missing."])

	| SOME v => v
    )



    fun main(arg0, argv) =
    let
	val files  = Option.parseCmdLine argv

	val width  = require_option
			Option.getWidth "width"

	val height = require_option
			Option.getHeight "height"

	fun show_stuff() =
	(
	    print "The files are";
	    app (fn f => (print " "; print f)) files;
	    print ".\n";

	    if Option.hasVerbose()
	    then
		print(concat[
		    "The width is ",  width,  ".\n",
		    "The height is ", height, ".\n"
		    ])
	    else
		()
	)
    in
	if Option.hasHelp()
	then
	    print "some helpful blurb\n"
	else
	    show_stuff();

        OS.Process.success
    end
    handle Usage msg =>
    (
	toErr msg;		toErr "\n";
	toErr(Option.usage());	toErr "\n";

        OS.Process.failure
    )


    val _ = SMLofNJ.exportFn("getopt3", main)
end

