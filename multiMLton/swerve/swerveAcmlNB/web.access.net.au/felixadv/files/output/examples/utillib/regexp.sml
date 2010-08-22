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


structure Main=
struct

    fun toErr s = TextIO.output(TextIO.stdErr, s)

    structure BT  = RegExpFn(structure P=AwkSyntax
		             structure E=BackTrackEngine)

    structure DFA = RegExpFn(structure P=AwkSyntax
		             structure E=BackTrackEngine)

    fun demo1BT msg =
    let
	val regexp = BT.compileString "the.(quick|slow).brown"
    in
	case StringCvt.scanString (BT.find regexp) msg of
	  NONE      => print "demo1 match failed\n"
	| SOME tree => show_matches msg tree
    end


    and demo1DFA msg =
    let
	val regexp = DFA.compileString "the.(quick|slow).brown"
    in
	case StringCvt.scanString (DFA.find regexp) msg of
	  NONE      => print "demo1 match failed\n"
	| SOME tree => show_matches msg tree
    end


    (*  Show the matches n=0, ... *)
    and show_matches msg tree =
    let
	val last = MatchTree.num tree

	fun find n =
	(
	    case MatchTree.nth(tree, n) of
	      NONE => "<Unmatched>"

	    | SOME {pos, len} => String.substring(msg, pos, len)
	)

	and loop n =
	(
	    print(concat[Int.toString n, " => ", find n, "\n"]);
	    if n >= last then () else loop(n+1)
	)
    in
	loop 0
    end



    local
	structure RE = RegExpSyntax
	structure CS = RE.CharSet

	val dot = RE.NonmatchSet(CS.singleton #"\n")

	fun cvt_str s = RE.Concat(map RE.Char (explode s))
    in
	fun demo2BT msg =
	let
	    (* "the.(quick|slow).brown" *)
	    val regexp = BackTrackEngine.compile(RE.Concat[
	    		    cvt_str "the",
			    dot,
			    RE.Group(RE.Alt[
				cvt_str "quick",
				cvt_str "slow"]),
			    dot,
			    cvt_str "brown"
			    ])
	in
	    case StringCvt.scanString (BT.find regexp) msg of
	      NONE      => print "demo2 match failed\n"
	    | SOME tree => show_matches msg tree
	end
    end


    fun main(arg0, argv) =
    let
	val msg = "the quick brown fox"
    in
	print "Demo 1 using BT\n";
	demo1BT msg;

	print "\n";
	print "Demo 1 using DFA\n";
	demo1DFA msg;

	print "\n";
	print "Demo 1 failure using BT\n";
	demo1BT "no quick brown fox";

	print "\n";
	print "Demo 2 using BT\n";
	demo2BT "the quick brown fox";

	OS.Process.success
    end
    handle x =>
    (
	toErr(exnMessage x); toErr("\n");
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("regexp", main)
end
