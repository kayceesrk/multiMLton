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

(*  This demonstrates some termio operations. *)

structure Main=
struct
    structure TTY = Posix.TTY

    fun toErr msg = TextIO.output(TextIO.stdErr, msg)
    exception Error of string


    fun setErase ch =
    let
	val fd   = Posix.FileSys.wordToFD 0w0
	val attr = TTY.getattr fd
	val new_attr = TTY.termios {
		iflag  = TTY.getiflag attr,
		oflag  = TTY.getoflag attr,
		cflag  = TTY.getcflag attr,
		lflag  = TTY.getlflag attr,
		cc     = TTY.V.update
			    (TTY.getcc attr, [(TTY.V.erase, ch)]),
		ispeed = TTY.getispeed attr,
		ospeed = TTY.getospeed attr
		}
    in
	TTY.setattr(fd, TTY.TC.sanow, new_attr)
    end


    fun parseControl s =
    let
	fun bad() = raise Error
		"Invalid control character\n"
    in
	case explode s of
	  [#"^", c] => Char.chr(Word.toInt(
	  	Word.andb(Word.fromInt(Char.ord c), 0wx1f)))

	| [c] => if Char.isCntrl c then c else bad()

	| _ => bad()
    end


    fun main(arg0, argv) =
    let
    in
	case argv of
	  ("erase"::ch::_) => setErase (parseControl ch)

	| _ => raise Error "Usage: ttyx [erase ch]\n";

        OS.Process.success
    end
    handle
      IO.Io {name, function, cause} =>
	(
	    toErr(concat["IO Error: ", name,
	                 ", ", exnMessage cause, "\n"]);
	    OS.Process.failure
	)

    | Error msg => (toErr msg; OS.Process.failure)

    | x => (toErr(concat["Uncaught exception: ", exnMessage x,"\n"]);
    	    OS.Process.failure)


    val _ = SMLofNJ.exportFn("ttyx", main)
end
