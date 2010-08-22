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
    structure Map = IntRedBlackMap

    fun toErr s = TextIO.output(TextIO.stdErr, s)


    fun read_file file : int Map.map =
    let
	val istrm = TextIO.openIn file

	(*  Read a pair of ints on a line and loop.
	    Empty lines are ignored. Other junk is fatal.
	*)
	fun get_pairs map_in lnum =
	(
	    case TextIO.inputLine istrm of
	      "" => (TextIO.closeIn istrm; map_in)   (* eof *)

	    | line =>
	    let
		val tokens = String.tokens Char.isSpace line
	    in
		case map Int.fromString tokens of
		  [] => get_pairs map_in (lnum+1)

		| [SOME a, SOME b] =>
		      get_pairs (Map.insert(map_in, a, b)) (lnum+1)

		| _ => raise Fail (concat["Invalid data on line ",
				          Int.toString lnum])
	    end
	)
	handle x => (TextIO.closeIn istrm; raise x)

    in
	get_pairs Map.empty 1
    end


    and show_pairs pairs =
    let
	fun show (a, b) = print(concat[
	    Int.toString a, " => ", Int.toString b, "\n"])
    in
	Map.appi show pairs
    end


    fun main(arg0, argv) =
    (
        case argv of
	  [file] =>
	(
	    show_pairs(read_file file);
	    OS.Process.success
        )

	| _ =>
	(
	    print "Usage: intmap file\n";
	    OS.Process.failure
	)
    )
    handle x =>
    (
	toErr(exnMessage x); toErr("\n");
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("intmap", main)
end
