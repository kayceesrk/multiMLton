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
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    fun count strm file =
    let
	fun read (nchars, nwords, nlines) =
	(
	    (* This ensures the line ends with a \n
	       unless we are at eof.
	    *)
	    case TextIO.inputLine strm of
	      "" => (nchars, nwords, nlines)

	    | line =>
		let
		    val words = String.tokens Char.isSpace line
		in
		    read (nchars + size line,
		          nwords + length words,
			  nlines + 1)
		end
	)

	val (nchars, nwords, nlines) = read (0, 0, 0)
    in
	print(concat[Int.toString nlines, " ",
		     Int.toString nwords, " ",
		     Int.toString nchars, " ",
		     file, "\n"])
    end


    fun main(arg0, argv) =
    let
    in
	case argv of
	  [] => count TextIO.stdIn ""

	| (file::_) =>
	    let
		val strm = TextIO.openIn file
	    in
		(count strm file) handle x =>
		    (TextIO.closeIn strm; raise x);
		TextIO.closeIn strm
	    end;

        OS.Process.success
    end
    handle
      IO.Io {name, function, cause} =>
	(
	    toErr(concat["IO Error: ", name,
	                 ", ", exnMessage cause, "\n"]);
	    OS.Process.failure
	)

    | x => (toErr(concat["Uncaught exception: ", exnMessage x,"\n"]);
    	    OS.Process.failure)


    val _ = SMLofNJ.exportFn("wcx", main)
end
