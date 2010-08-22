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
    structure FS = Posix.FileSys

    exception Error of string
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    fun wordToDec w = SysWord.fmt StringCvt.DEC w


    local
	val type_preds = [
		(FS.ST.isDir,   "Directory"),
		(FS.ST.isChr,   "Char Device"),
		(FS.ST.isBlk,   "Block Device"),
		(FS.ST.isReg,   "Regular File"),
		(FS.ST.isFIFO,  "FIFO"),
		(FS.ST.isLink,  "Symbolic Link"),
		(FS.ST.isSock,  "Socket")
		]
    in
	fun filetypeToString st =
	let
	    val pred = List.find (fn (pred, _) => pred st) type_preds
	in
	    case pred of
	      SOME (_, name) => name
	    | NONE => "Unknown"
	end
    end


    local
	fun test flag ch mode =
	(
	    if FS.S.anySet(FS.S.flags [flag], mode)
	    then
		ch
	    else
		#"-"
	)

	fun test2 flag1 ch1 flag2 ch2 mode =
	(
	    if FS.S.anySet(FS.S.flags [flag1], mode)
	    then
		ch1
	    else
		if FS.S.anySet(FS.S.flags [flag2], mode)
		then
		    ch2
		else
		    #"-"
	)

	val flags  = [
		test  FS.S.irusr #"r",
		test  FS.S.iwusr #"w",
		test2 FS.S.isuid #"s" FS.S.ixusr #"x",
		test  FS.S.irgrp #"r",
		test  FS.S.iwgrp #"w",
		test2 FS.S.isgid #"s" FS.S.ixusr #"x",
		test  FS.S.iroth #"r",
		test  FS.S.iwoth #"w",
		test  FS.S.ixoth #"x"
		]
    in
	fun modeToString mode =
	let
	    val chars = foldl
		(fn (func, rslt) => (func mode)::rslt)
		[] flags
	in
	    implode(rev chars)
	end
    end



    local
	structure PROC = Posix.ProcEnv
	structure DB   = Posix.SysDB
    in
	fun uidToInt uid = wordToDec(PROC.uidToWord uid)
	pun gidToInt gid = wordToDec(PROC.gidToWord gid)

	fun uidToName uid =
	(
	    (DB.Passwd.name(DB.getpwuid uid))
		handle _ => "unknown"
	)

	fun gidToName gid =
	(
	    (DB.Group.name(DB.getgrgid gid))
		handle _ => "unknown"
	)
    end



    fun devToString dev =
    let
	val word = FS.devToWord dev
	val w1 = SysWord.andb(SysWord.>>(word, 0w8), 0wxff)
	val w2 = SysWord.andb(word, 0wxff)
    in
	concat[wordToDec w1, ",", wordToDec w2]
    end



    fun stat file =
    let
	val st = (FS.stat file) 
		    handle OS.SysErr (msg, _) =>
			raise Error (concat[ file, ": ", msg, "\n"])

	val mode = FS.ST.mode st
	val uid  = FS.ST.uid st
	val gid  = FS.ST.gid st
    in
	print(concat["  File: ", file, "\n"]);

	print(concat["  Size: ",
	    Position.toString(FS.ST.size st), "\n"]);

	print(concat["  Type: ",
	    filetypeToString st, "\n"]);

	print(concat["  Mode: ",
	    SysWord.fmt StringCvt.OCT (FS.S.toWord mode),
	    "/", modeToString mode, "\n"]);

	print(concat["   Uid: ",
	    uidToInt uid, "/", uidToName uid, "\n"]);

	print(concat["   Gid: ",
	    gidToInt gid, "/", gidToName gid, "\n"]);

	print(concat["Device: ",
	    devToString(FS.ST.dev st), "\n"]);

	print(concat[" Inode: ",
	    wordToDec(FS.inoToWord(FS.ST.ino st)), "\n"]);

	print(concat[" Links: ",
	    Int.toString(FS.ST.nlink st), "\n"]);

	print(concat["Access: ", Date.toString(
	    Date.fromTimeLocal(FS.ST.atime st)), "\n"]);

	print(concat["Modify: ", Date.toString(
	    Date.fromTimeLocal(FS.ST.mtime st)), "\n"]);

	print(concat["Change: ", Date.toString(
	    Date.fromTimeLocal(FS.ST.ctime st)), "\n"]);
	()
    end



    fun main(arg0, argv) =
    let
    in
	case argv of
	  [file] => (stat file; OS.Process.success)

	| _ => (toErr "Usage: statx filename\n"; OS.Process.failure)
    end
    handle
      OS.SysErr (msg, _) =>
	(
	    toErr(concat["Error: ", msg, "\n"]);
	    OS.Process.failure
	)

    | Error msg => (toErr msg; OS.Process.failure)

    | x => (toErr(concat["Uncaught exception: ", exnMessage x,"\n"]);
    	    OS.Process.failure)


    val _ = SMLofNJ.exportFn("statx", main)
end
