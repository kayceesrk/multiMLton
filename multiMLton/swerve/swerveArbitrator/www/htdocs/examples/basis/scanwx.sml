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
    structure FS = OS.FileSys
    structure OP = OS.Path

    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    exception Error of string


    fun open_dir dir =
    (
	FS.openDir dir
    )
    handle 
      OS.SysErr (msg, _) => raise Error (concat[
		"Cannot open directory ", dir, ": ", msg, "\n"])


    fun scan_dir dir =
    let
	(* val _ = print(concat["scan_dir ", dir, "\n"]) *)
	val strm = open_dir dir

	fun get_files files =
	(
	    case FS.readDir strm of
	      "" => rev files		(* done *)

	    | f => 
		let
		    val file = OP.joinDirFile
				{dir = dir, file = f}
		in
		    if FS.isLink file
		    then
			get_files files
		    else
			get_files (file::files)
		end
	)

	val files = get_files []
	val _     = FS.closeDir strm

	fun show_wx file =
	(
	    if FS.access(file, [FS.A_WRITE, FS.A_EXEC])
	    then
		(print file; print "\n")
	    else
		()
	)

	fun scan_subdir file =
	(
	    if FS.isDir file
	    then
		scan_dir file
	    else
		()
	)
    in
	app show_wx files;
	app scan_subdir files
    end


    fun main(arg0, argv) =
    let
    in
	case argv of
	  [] => scan_dir OP.currentArc

	| (file::_) => scan_dir file;

        OS.Process.success
    end
    handle
      OS.SysErr (msg, _) =>
	(
	    toErr(concat["System Error: ", msg, "\n"]);
	    OS.Process.failure
	)

    | Error msg => (toErr msg; OS.Process.failure)

    | x => (toErr(concat["Uncaught exception: ", exnMessage x,"\n"]);
    	    OS.Process.failure)


    val _ = SMLofNJ.exportFn("scanwx", main)
end
