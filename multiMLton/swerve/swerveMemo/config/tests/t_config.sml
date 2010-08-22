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

(* $Id: t_config.sml,v 1.3 2001/05/20 18:08:03 felix Exp $ *)

signature T_CONFIG =
sig
    val run:	unit -> unit
end



structure t_Config: T_CONFIG =
struct

    open TestUtils
    open Config

(*------------------------------------------------------------------------------*)

    fun test1() =
    let
    in
	print "Test 1 using test1.cfg\n";
	processConfig "test1.cfg";
	show_root();
	app show_node (getNodeConfigs());
	print "\n"
    end


    and show_root() =
    let
	val ServerConfig {
		server_root:	string,
		config_dir:	string,
		lock_file:	string,
		pid_file:	string,
		mime_file:	string,
		error_log:	string,
		dir_index:	string,

		log_level:	Log.Level,

		run_user:       string option,
		run_group:      string option,

		conn_timeout:   int option,
		max_clients:    int option,

		listen_host:    string option,
		listen_port:    int
		} = getServerConfig()
    in
	print(concat["ServerRoot\t= ",	server_root,	"\n"]);
	print(concat["ConfigDir\t= ",	config_dir,	"\n"]);
	print(concat["LockFile\t= ",	lock_file,	"\n"]);
	print(concat["PidFile \t= ",	pid_file,	"\n"]);
	print(concat["MimeFile\t= ",	mime_file,	"\n"]);
	print(concat["ErrorLog\t= ",	error_log,	"\n"]);
	print(concat["DirIndex\t= ",	dir_index,	"\n"]);

	print(concat["LogLevel\t= ", Log.formatLevel log_level, "\n"]);

	print(concat["RunUser \t= ", opt_str run_user,  "\n"]);
	print(concat["RunGroup\t= ", opt_str run_group, "\n"]);

	print(concat["ConnTimeout\t= ",	opt_int conn_timeout,  "\n"]);
	print(concat["MaxClients\t= ",	opt_int max_clients,   "\n"]);

	print(concat["ListenHost\t= ", opt_str listen_host, "\n"]);
	print(concat["ListenPort\t= ", Int.toString listen_port, "\n"]);
	()
    end


    and show_node config =
    let
	val NodeConfig {path, kind, options, auth} = config

	fun put_option NOFInherit = print " inherit"
	|   put_option NOFAll     = print " all"
	|   put_option NOFNone    = print " none"
	|   put_option (NOFAdd opt) = (print " +"; put_opt opt)
	|   put_option (NOFSub opt) = (print " -"; put_opt opt)

	and put_opt NodeExecCGI         = print "ExecCGI"
	|   put_opt NodeFollowSymLinks	= print "FollowSymLinks"
	|   put_opt NodeWithSubDirs	= print "WithSubDirs"

	fun put_auth NONE = print " NONE\n"
	|   put_auth (SOME(NodeBasic {realm, user_file, group_file, users, groups})) =
	let
	in
	    print(concat["Basic realm=", realm,
	                 " userfile=",  user_file,
	                 " groupfile=", group_file,
			 "\n"]);

	    print "    users";
	    case users of
	      NONE => print " NONE"
	    | SOME u => app (fn p => (print " "; print p)) u;
	    print "\n";

	    print "    groups";
	    app (fn p => (print " "; print p)) groups;
	    print "\n"
	end

    in
	print(concat["\nNode ",	URL.pathToString path,	"\n"]);

	case kind of
	  NodeIsDir {path} =>	  print(concat["Directory ", opt_str path, "\n"])
	| NodeIsHandler {name} => print(concat["Handler ", name, "\n"])
	| NodeIsScript  {path} => print(concat["Script ",  path, "\n"]);

	print "Options"; app put_option options; print "\n";
	print "Auth "; put_auth auth
    end


    and opt_str NONE = "NONE"
    |   opt_str (SOME s) = s

    and opt_int NONE = "NONE"
    |   opt_int (SOME i) = Int.toString i

(*------------------------------------------------------------------------------*)

    fun run() =
    (
	test1()
    )

(*------------------------------------------------------------------------------*)

end
