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

(* $Id: startup.sml,v 1.7 2001/10/24 19:42:57 felix Exp $ *)


(*  This performs various startup actions.
*)

signature STARTUP =
sig

    (*	This may raise FatalX *)
    val startup:    unit -> unit

    (*	This will be run when the server is shutting down. *)
    val finish:	    unit -> unit

end


structure StartUp: STARTUP =
struct

    open Common

    structure TF    = TextFrag
    structure Cfg   = Config

(*------------------------------------------------------------------------------*)

    fun startup() =
    let
    in
	MyProfile.start();

	if Cfg.haveServerConfig()
	then
	    ()
	else
	(
	    Log.error ["The server configuration has not been specified."];
	    raise FatalX
	);

	(*  Give up if there have been errors already. *)
	if Log.numErrors() > 0 then raise FatalX else ();

	(*  The configuration code checks that all of the files and
	    directories exist.
	*)
	setuid();
	create_lock();

	(*  Give up again. *)
	if Log.numErrors() > 0 then raise FatalX else ();

	()
    end




    (*	Perhaps we should clean out the tmp directory too? *)
    and finish() =
    let
    in
	remove_lock();
	MyProfile.stop();
	Log.flush();
	()
    end

(*------------------------------------------------------------------------------*)

    (*	Run setuid. *)

    and setuid() =
    let
	val Cfg.ServerConfig {run_user, run_group, ...} = Cfg.getServerConfig()

	(*  If a group name is specified then use it as the gid.
	*)
	fun get_ids() =
	let
	    val group_gid =
		case run_group of
		  NONE      => NONE
		| SOME name =>
		    let
			val group = Posix.SysDB.getgrnam name
		    in
			SOME(Posix.SysDB.Group.gid group)
		    end

	    val (user_uid, user_gid) =
		case run_user of
		  NONE      => (NONE, NONE)
		| SOME name =>
		    let
			val passwd = Posix.SysDB.getpwnam name
			val uid    = Posix.SysDB.Passwd.uid passwd
			val gid    = Posix.SysDB.Passwd.gid passwd
		    in
			(SOME uid, SOME gid)
		    end
	in
	    (user_uid, if isSome group_gid then group_gid else user_gid)
	end

	val (opt_uid, opt_gid) = get_ids()
    in
	Option.map Posix.ProcEnv.setuid opt_uid;
	Option.map Posix.ProcEnv.setgid opt_gid
    end
    handle x => (Log.logExn x; raise FatalX)

(*------------------------------------------------------------------------------*)

    (*	Create our lock and pid files. *)
    and create_lock() =
    let
	val Cfg.ServerConfig {var_dir, ...} = Cfg.getServerConfig()
	val lock_file = Files.appendFile var_dir "lock"
	val pid_file  = Files.appendFile var_dir "pid"
    in
	Log.inform Log.Debug (fn() => TF.concat ["Creating lock file ", lock_file]);

	if FileIO.exclCreate lock_file
	then
	    let
		val strm = TextIO.openOut pid_file
		val pid = Posix.ProcEnv.getpid()
		val w   = Posix.Process.pidToWord pid
	    in
		TextIO.output(strm, SysWord.fmt StringCvt.DEC w);
		TextIO.output(strm, "\n");
		TextIO.closeOut(strm)
	    end
	    handle x => (Log.logExn x; raise x)
	else
	(
	    Log.error ["Another server is already running."];
	    raise FatalX
	)
    end
    handle _ => raise FatalX



    and remove_lock() =
    let
	val Cfg.ServerConfig {var_dir, ...} = Cfg.getServerConfig()
	val lock_file = Files.appendFile var_dir "lock"
	val pid_file  = Files.appendFile var_dir "pid"
    in
	FileIO.removeFile pid_file;
	FileIO.removeFile lock_file
    end


end
