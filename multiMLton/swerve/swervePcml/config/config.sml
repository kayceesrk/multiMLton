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

(* $Id: config.sml,v 1.32 2002/03/12 18:10:01 felix Exp $ *)

(*  This interprets the configuration language.

@#34567890123456789012345678901234567890123456789012345678901234567890
*)

signature CONFIG =
sig
    (*	This is a simplified form of URLPath with just the parts.
	These paths are case-sensitive and so are stored in the
	original case.
    *)
    type NodePath = string list


    (*	Required parameters are stored as strings with "" for an undefined
	value.	Optional ones as string option.
    *)
    datatype ServerConfig = ServerConfig of {
		server_root:	string,
		config_dir:	string,
		var_dir:	string,
		tmp_dir:	string,
		doc_dir:	string,
		cgi_dir:	string,
		mime_file:	string,
		error_log:	string,
		dir_index:	string,

		log_level:	Log.Level,

		run_user:       string option,
		run_group:      string option,

		conn_timeout:   int option,
		max_clients:    int option,
		max_tmp_space:  int option,
		max_req_size:   int option,

		listen_host:    string option,
		listen_port:    int,
		server_name:	string
		}


    datatype NodeConfig = NodeConfig of {
		path:	    NodePath,
		kind:	    NodeKind,
		options:    NodeOptionFormula list,
		auth:	    NodeAuth
		}

    and	NodeKind =
	    NodeIsDir of {
		path:	string		(* directory path *)
		}

	|   NodeIsBuiltin of {
		name:	string
		}

	|   NodeIsScript of {
		path:	string
		}

    (*	This is a subset of NodeConfig for .swerve files. *)
    and SwerveConfig = SwerveConfig of {
		options:    NodeOptionFormula list,
		auth:	    NodeAuth
		}

    and NodeOptionFormula =
	    NOFInherit
	|   NOFAll
	|   NOFNone
	|   NOFAdd of NodeOption
	|   NOFSub of NodeOption


    and NodeOption =
	    NodeExecCGI
	|   NodeFollowSymLinks
	|   NodeWithSubDirs


    and NodeAuth =
	    NodeBasic of {
		realm:	    string,
		user_file:  string,	(* path to the user file *)
		group_file: string,	(* path to the group file *)
		users:	    string list,(* users to allow *)
		groups:	    string list (* groups to allow *)
		}

	|   NodeNoAuth


    (*	This reads the configuration file and stores the results in
	the global variables.

	This will report errors and raise FatalX if the configuration is
	not usable.

    *)
    val processConfig:	    string -> unit

    val haveServerConfig:   unit -> bool

    (*	This is not defined if the processConfig() has not succeeded. *)
    val getServerConfig:    unit -> ServerConfig

    (*	Return a list of all of the node configurations.
    *)
    val getNodeConfigs:	    unit -> NodeConfig list

    (*	This reads a .swerve file and returns the configuration or
	NONE if it wasn't parsable.
	The Io exception will be raised if the file cannot be read.
    *)
    val	processNodeFile:    string -> SwerveConfig option

    (*	This returns the node configuration from the main configuration
	file if the node path appeared exactly in the file.
    *)
    val findNodeConfig:    NodePath -> NodeConfig option


    (*	This looks up a file extension to get the media type. This
	depends on the server configuration being successfully loaded.
	The lookup will be case-insensitive since for example .jpg and
	.JPG should both be taken as JPEG pictures.

	This returns NONE if the lookup failed. The caller should default
	this to something it considers reasonable.
    *)
    val lookupMime:	    string -> (string * string) option

    (*	These convert between our simple node path which is a list
	of strings and the more elaborate URL path.  These paths
	are always absolute.
    *)
    val URLPathToList:	    URL.URLPath -> NodePath
    val listToURLPath:	    NodePath -> URL.URLPath
    val listToString:	    NodePath -> string

    (*	Test if two node paths match. *)
    val sameNodePath:	    NodePath -> NodePath -> bool

end




structure Config: CONFIG =
struct

    open Common
    open ConfigTypes

    structure TF = TextFrag
    structure SS = Substring

(*------------------------------------------------------------------------------*)

    (*	Assemble the pieces to make a parser. *)

    structure ConfigLrVals = ConfigLrValsFun(structure Token = LrParser.Token)
    structure ConfigLex    = ConfigLexFun(structure Tokens = ConfigLrVals.Tokens)
    structure ConfigParser = JoinWithArg(structure LrParser = LrParser
			    structure ParserData = ConfigLrVals.ParserData
			    structure Lex = ConfigLex)

    (*	Max number of tokens to lookahead when correcting errors. *)
    val max_lookahead = 15;

    (*	The syntax error messages use the token names. This is for editing
	them to something more readable.
    *)
    val syntax_edits = [
	("KW_SERVER",	    "Server"),
	("KW_NODE",	    "Node"),
	("SYM_SEMICOLON",   "semicolon"),
	("SYM_COMMA",       "comma"),
	("SYM_LBRACE",      "'{'"),
	("SYM_RBRACE",      "'}'"),
	("SYM_EQUALS",      "'='"),
	("TOK_WORD",        "word"),
	("TOK_STRING",      "string"),
	("TOK_INT",         "number")
	]

(*------------------------------------------------------------------------------*)

    type NodePath = string list

    datatype ServerConfig = ServerConfig of {
		server_root:	string,
		config_dir:	string,
		var_dir:	string,
		tmp_dir:	string,
		doc_dir:	string,
		cgi_dir:	string,
		mime_file:	string,
		error_log:	string,
		dir_index:	string,

		log_level:	Log.Level,

		run_user:       string option,
		run_group:      string option,

		conn_timeout:   int option,
		max_clients:    int option,
		max_tmp_space:  int option,
		max_req_size:   int option,

		listen_host:    string option,
		listen_port:    int,
		server_name:	string
		}


    datatype NodeConfig = NodeConfig of {
		path:	    string list,
		kind:	    NodeKind,
		options:    NodeOptionFormula list,
		auth:	    NodeAuth
		}

    and	NodeKind =
	    NodeIsDir of {
		path:	string
		}

	|   NodeIsBuiltin of {
		name:	string
		}

	|   NodeIsScript of {
		path:	string
		}

    (*	This is a subset of NodeConfig for .swerve files. *)
    and SwerveConfig = SwerveConfig of {
		options:    NodeOptionFormula list,
		auth:	    NodeAuth
		}

    and NodeOptionFormula =
	    NOFInherit
	|   NOFAll
	|   NOFNone
	|   NOFAdd of NodeOption
	|   NOFSub of NodeOption


    and NodeOption =
	    NodeExecCGI
	|   NodeFollowSymLinks
	|   NodeWithSubDirs


    and NodeAuth =
	    NodeBasic of {
		realm:	    string,
		user_file:  string,	(* path to the user file *)
		group_file: string,	(* path to the group file *)
		users:	    string list,(* users to allow *)
		groups:	    string list (* groups to allow *)
		}

	|   NodeNoAuth



(*------------------------------------------------------------------------------*)

    fun URLPathToList (URL.URLPath {segs, ...}) =
    let
	fun get_part (URL.Segment {part, ...}) = part
    in
	map get_part segs
    end



    fun listToURLPath parts =
    let
	fun mk part = URL.Segment {part=part, params=[]}
    in
	URL.URLPath {segs = map mk parts, absolute=true}
    end


    fun listToString [] = "/"
    |   listToString lst = concat(List.concat(map (fn l => ["/", l]) lst))


    fun sameNodePath []       []       = true
    |   sameNodePath []       _        = false
    |   sameNodePath _        []       = false
    |   sameNodePath (h1::t1) (h2::t2) = (h1 = h2) andalso sameNodePath t1 t2

(*------------------------------------------------------------------------------*)

    (*	These configuration variables will only be read during the operation of the
	server so we don't need concurrent locking.
    *)
    exception Bad

    val cf_server_config: ServerConfig option ref = ref NONE

    val cf_nodes: NodeConfig list ref = ref []

    fun getServerConfig()  = valOf(!cf_server_config)
    fun haveServerConfig() = isSome(!cf_server_config)
    fun getNodeConfigs()   = !cf_nodes


    fun getServerRoot() =
    let
	val ServerConfig{server_root, ...} = getServerConfig()
    in
	server_root
    end


    (*	The mime information is just a map from an extension to
	the pair.
    *)
    val mime_table: (string * string) STRT.hash_table =
    					STRT.mkTable(101, NotFound)


(*------------------------------------------------------------------------------*)

    fun processConfig file : unit =
    let
	(* show the warnings while processing *)
(*	val _ = Log.lowerLevel Log.Warn *)
	val sections = parse_config false file
    in
	(* dump_sections sections; *)

	(* Ensure that the server node is processed first to
	 * get the server root for the nodes' files. *)

	Log.inform Log.Debug (fn () => TF.str "Config server");
	process_server_section sections;
	Log.inform Log.Debug (fn () => TF.str "Config node");
	process_node_sections sections;

	Log.inform Log.Debug (fn () => TF.str "Load mime types");
	process_mime_file();
	Log.inform Log.Debug (fn () => TF.str "Init globals");
	init_globals();
	()
    end
    handle _ => (Log.flush(); raise FatalX)



    (*	This pokes some config parameters into various modules in
	common.
    *)
    and init_globals() =
    let
	val ServerConfig {error_log, log_level, max_tmp_space, ...} =
		getServerConfig()
    in
	(*  Don't change the error stream until the config has been processed. *)
	Log.flush();
	Log.setLogFile error_log;
	Log.setLevel   Log.Fatal;

	case max_tmp_space of
	  NONE   => ()
	| SOME l => TmpFile.setDiskLimit l;

	()
    end




    and processNodeFile file : SwerveConfig option =
    let
	(*  There will only be one swerve section. *)
	val section = hd(parse_config true file)
    in
	(* dump_sections [section]; *)
	process_swerve_section section
    end



    (*	To parse from a .swerve file we force the ^A^A^A sequence onto
	the input.  This requires a state flag during the scanning.

	This will be called from a store node and should use the open
	file manager to compete with other connections.  The BinIO here
	must be the one supplied by MLton.PCML.

	The Io exception will be raised if the file cannot be read.
    *)
    and parse_config swerve file: Section list =
    let
	fun parse_error(msg, pos1, pos2) = Log.errorP pos1 [edit_errors msg]

	val swerve_done = ref false

	fun input rstrm n =
	(
	    if swerve andalso not (!swerve_done)
	    then
		(swerve_done := true; "\^A\^A\^A")
	    else
		TextIO.inputN(rstrm, n)
	)

	fun do_parser holder =
	let
	    val rstrm = TextIOReader.get holder

	    fun do_parse lexstream =
		ConfigParser.parse(max_lookahead, lexstream, parse_error, file)

	    val in_stream = ConfigParser.makeLexer (input rstrm) file

	    val (result, new_stream) = do_parse in_stream
			handle ParseError => ([], in_stream)
	in
	    TextIOReader.closeIt holder;
	    result
	end
    in
	case TextIOReader.openIt' file of
	  NONE   => []
	| SOME h => do_parser h
    end



    (*	Replace each token with its name and update the 'inserting' and 'deleting;
	part.
    *)

    and edit_errors msg =
    let
	fun try_edits []           substr count = (count, SS.string substr)
	|   try_edits (edit::rest) substr count =
	let
	    val (count2, str2) = try_edit edit substr count []
	in
	    try_edits rest (SS.full str2) count2
	end


	and try_edit (old, new) substr count rev_result : int * string =
	let
	    val (pref, suff) = SS.position old substr
	in
	    if SS.isEmpty suff
	    then
		(count, SS.concat(rev (pref::rev_result)))
	    else
	    let
		val remainder = SS.triml (size old) suff
		val replacement = SS.full new
	    in
		try_edit (old, new) remainder (count+1) (replacement::pref::rev_result)
	    end
	end


	val (count, edit1) = try_edits syntax_edits (SS.full msg) 0


	val num_edits =
	    if count = 1
	    then
		[("inserting ",	"inserting a"),
		("deleting ",	"deleting a")]
	    else
		[("inserting ",	"inserting:"),
		("deleting ",	"deleting:")]

	val final = #2(try_edits num_edits (SS.full edit1) 0)
    in
	final
    end



    and dump_sections sections =
    let
	fun dump (SectServer {parts, pos}) =
	(
	    print "Server\n";
	    app dump_part parts;
	    print "\n"
	)

	|   dump (SectNode {path, parts, pos}) =
	(
	    print(concat["Node ", path, "\n"]);
	    app dump_part parts;
	    print "\n"
	)

	|   dump (SectSwerve {parts}) =
	(
	    print "Swerve\n";
	    app dump_part parts;
	    print "\n"
	)


	and dump_part (SectionPart {left, right, pos}) =
	(
	    print(concat["    ", left, " ="]);
	    app (fn l => (print " "; dump_literal l)) right;
	    print ";\n"
	)

	and dump_literal (LitIsString (str, _)) =
	(
	    print(concat["'", str, "'"])
	)
	|   dump_literal (LitIsInt (n, _)) =
	(
	    print(Int.toString n)
	)
    in
	app dump sections
    end

(*------------------------------------------------------------------------------*)

    (*	This should find exactly one server section. *)
    and process_server_section sections =
    let
	fun match (SectServer _) = true
	|   match _              = false

	val sects = List.filter match sections
    in
	case sects of
	  [SectServer {parts, ...}] => process_server_parts parts

	| [] => (Log.error
		   ["A server configuration section must be supplied."];
	    	 raise Bad)

	| _  => (Log.error
		   ["There are multiple server configuration sections."];
	    	 raise Bad)
    end



    and process_node_sections sections =
    let
	fun process sect =
	(
	    case process_node_section sect of
	      NONE        => ()
	    | SOME config => cf_nodes := config :: (!cf_nodes)
	)

	fun match (SectNode _) = true
	|   match _            = false

	val sects = List.filter match sections
    in
	case sects of
	    [] => (Log.log Log.Warn
			   ( TF.str
	  			 "There are no node configuration sections." ) )

	  | _  => app process sects
    end



    (*	This will raise Bad if the config is invalid.
    *)
    and process_server_parts original_parts =
    let
	(*  Extract the parts we recognise and then see if anything is
	    left over.  We try to report as many errors as possible
	    before aborting.
	*)

	(*  The dispatch function must return the value of the parameter as a
	    string.  This includes optional string parameters.
	*)
	type CheckFunc = Literal list -> SrcPos -> string

	val str_dispatch : (string * CheckFunc) list = [
	    ("ServerRoot",	check_ServerRoot),
	    ("TmpDir",		check_TmpDir),
	    ("TypesConfig",	check_TypesConfig),
	    ("ErrorLog",	check_ErrorLog),
	    ("DirectoryIndex",	check_DirectoryIndex),
	    ("User",		check_User),
	    ("Group",		check_Group),
	    ("ServerName",	check_ServerName)
	    ]

	val int_dispatch = [
	    ("Timeout",		check_Timeout),
	    ("MaxClients",	check_MaxClients),
	    ("MaxTmpSpace",	check_MaxTmpSpace),
	    ("MaxReqSize",	check_MaxReqSize)
	    ]

	(*  This reduces the parts to pairs of name,value and left-over
	    parts.
	*)
	fun do_strings []	      parts_in pairs = (pairs, parts_in)
	|   do_strings (entry::erest) parts_in pairs =
	let
	    val (key, func) = entry
	    val (fnd, parts_out) = find_parts [key] parts_in

	    val new_pairs =
		case fnd of
		  [] => pairs		(* param was not found *)

		| (left, lits, pos)::rest =>
		    let
			val _ = app show_dupl rest
			val v = func lits pos
		    in
			(left, v)::pairs
		    end
		    handle Bad => pairs	(* skip if there was an error *)
	in
	    do_strings erest parts_out new_pairs
	end


	and show_dupl (left, _, pos) = warn_duplicate left pos


	fun do_ints []	           parts_in pairs = (pairs, parts_in)
	|   do_ints (entry::erest) parts_in pairs =
	let
	    val (key, func) = entry
	    val (fnd, parts_out) = find_parts [key] parts_in

	    val new_pairs =
		case fnd of
		  [] => pairs		(* param was not found *)

		| (left, lits, pos)::rest =>
		    let
			val _ = app show_dupl rest
			val v = func lits pos
		    in
			(left, v)::pairs
		    end
		    handle Bad => pairs	(* skip if there was an error *)
	in
	    do_ints erest parts_out new_pairs
	end


	(*  Return the pair of optional parameters. *)
	fun do_listen parts_in =
	let
	    val (fnd, parts_out) = find_parts ["Listen"] parts_in
	in
	    case fnd of
	      [] => (NONE, NONE, parts_out)

	    | (left, lits, pos)::rest =>
		let
		    val (o1, o2) = check_Listen lits pos
		in
		    app show_dupl rest;
		    (o1, o2, parts_out)
		end
	end


	fun do_loglevel parts_in =
	let
	    val (fnd, parts_out) = find_parts ["Loglevel"] parts_in
	in
	    case fnd of
	      [] => (NONE, parts_out)

	    | (left, lits, pos)::rest =>
		(
		    app show_dupl rest;
		    (check_LogLevel lits pos, parts_out)
		)
	end


	val (str_pairs, parts1)  = do_strings str_dispatch original_parts []
	val (int_pairs, parts2)  = do_ints int_dispatch parts1 []
	val (host, port, parts3) = do_listen parts2
	val (level,     parts4)  = do_loglevel parts3
	val _                    = app unrec_param parts4;

	(*  Now extract the fields for the config record.
	*)

	fun reqstr name =
	(
	    case List.find (fn (n, _) => kw_match n name) str_pairs of
	      SOME (_, s) => s
	    | _ => (Log.error ["A valid ", name, " has not been supplied."];
	    	    raise Bad)
	)

	fun strdflt name default =
	(
	    case List.find (fn (n, _) => kw_match n name) str_pairs of
	      SOME (_, s) => s
	    | _ => default
	)

	fun optstr name =
	(
	    case List.find (fn (n, _) => kw_match n name) str_pairs of
	      SOME (_, s) => SOME s
	    | _ => NONE
	)

	fun optint name =
	(
	    case List.find (fn (n, _) => kw_match n name) int_pairs of
	      SOME (_, s) => SOME s
	    | _ => NONE
	)

	val root = reqstr "ServerRoot"
	val conf = Files.appendFile root "conf"
	val var  = Files.appendFile root "var"
	val tmp  = Files.appendFile root "tmp"
	val doc  = Files.appendFile root "htdocs"
	val cgi  = Files.appendFile root "cgi-bin"
	val err  = Files.appendFile var  "errors"
	val mime = Files.appendFile conf "mime.types"

	val error_log = strdflt "ErrorLog" err
	val log_level = getOpt(level, Log.Error)

	val _ =
	    if isSome port
	    then
		()
	    else
	    (
		Log.error ["A valid port number has not been supplied."];
		raise Bad
	    );

	val config =
	    ServerConfig {
		server_root	= root,
		config_dir      = conf,
		var_dir 	= strdflt "VarDir"   var,
		tmp_dir 	= strdflt "TmpDir"   tmp,
		doc_dir 	= doc,
		cgi_dir 	= cgi,

		mime_file       =
		    check_rel_readable root (strdflt "TypesConfig" mime),

		error_log       = error_log,
		dir_index       = strdflt "DirectoryIndex" "index.html",

		log_level	= log_level,

		run_user	= optstr "User",
		run_group	= optstr "Group",

		conn_timeout    = optint "TimeOut",
		max_clients	= optint "MaxClients",
		max_tmp_space   = optint "MaxTmpSpace",
		max_req_size    = optint "MaxReqSize",

		listen_host	= host,
		listen_port	= valOf(port),
		server_name	= reqstr "ServerName"
	    }
    in
	cf_server_config := SOME config
    end


    and unrec_param (SectionPart {left, pos, ...}) =
    (
	Log.errorP pos ["Unrecognised parameter: ", left]
    )


    (*	These check functions may raise Bad if there is an error. *)
    and check_ServerRoot value pos =
    let
	val root = expect_one_string value pos
    in
	expect_absolute		root pos;
	expect_accessible_dir	root pos;
	root
    end


    and check_TmpDir value pos =
    let
	val tmp = expect_one_string value pos
    in
	expect_absolute		tmp pos;
	expect_accessible_dir	tmp pos;
	tmp
    end



    and check_ErrorLog value pos =
	    check_writable_file "ErrorLog" value pos


    and check_TypesConfig value pos = expect_one_string value pos


    (*	Make the path relative to the parent if it isn't
	absolute. Check that it is readable.
    *)
    and check_rel_readable parent file =
    let
	val path =
		if Files.absPath file
		then
		    file
		else
		    Files.appendFile parent file
    in
	expect_readable_file path NONE;
	path
    end


    and check_writable_file name value pos =
    let
	val path = expect_one_string value pos
    in
	expect_absolute		path pos;
	expect_writable_file	path pos;
	path
    end


    and check_Timeout value pos =
    let
	val timeout = expect_one_integer value pos
    in
	expect_positive timeout pos;
	timeout
    end


    and check_MaxClients value pos =
    let
	val max = expect_one_integer value pos
    in
	expect_positive max pos;
	max
    end


    and check_MaxTmpSpace value pos =
    let
	val max = expect_one_integer value pos
    in
	expect_positive max pos;
	max
    end


    (*	REVISIT This should check that the req limit is less than the tmp space. *)
    and check_MaxReqSize value pos =
    let
	val max = expect_one_integer value pos
    in
	expect_positive max pos;
	max
    end


    (*	The possible values are localhost:<port> or just :<port>.
	This returns the optional host and port.
    *)
    and check_Listen value pos : (string option * int option) =
    let
	val arg = expect_one_string value pos
	val fields = String.fields (fn c => c = #":") arg

	fun check_port port =
	(
	    case Int.fromString port of
	      NONE => (Log.errorP pos ["The port number is invalid."]; NONE)

	    | SOME p =>
	    (
		expect_range p 1 65535 pos;
		SOME p
	    )
	)
    in
	case fields of
	  ["", port] => (NONE, check_port port)

	| [host, port] => (SOME host, check_port port)

	| _ => (Log.errorP pos [arg, " is not a recognisable Listen value."];
		(NONE, NONE))
    end


    and check_User value pos =
    let
	val name = expect_one_string value pos
    in
	ignore(Posix.SysDB.getpwnam name)
	    handle _ => (
		Log.errorP pos [name, " is not a valid user name."];
		raise Bad
		);

	name
    end


    and check_Group value pos =
    let
	val name = expect_one_string value pos
    in
	ignore(Posix.SysDB.getgrnam name)
	    handle _ => (
		Log.errorP pos [name, " is not a valid group name."];
		raise Bad
		);

	name
    end


    and check_DirectoryIndex value pos =
    (
	expect_one_string value pos
    )


    and check_ServerName value pos =
    (
	expect_one_string value pos
    )


    and check_LogLevel value pos =
    let
	val level = expect_one_string value pos
	val ulevel = upperCase level

	val levels = [
	    ("ERROR",	Log.Error),
	    ("WARN",	Log.Warn),
	    ("INFO",	Log.Info),
	    ("DEBUG",	Log.Debug)
	    ]
    in
	case List.find (fn (n, _) => n = ulevel) levels of
	  NONE =>
	    (
		Log.errorP pos ["No such log level: ", level];
		NONE
	    )

	| SOME (_, l) => SOME l
    end

(*------------------------------------------------------------------------------*)

    (*	In order to be able to trust the server the configuration must
	be error free or else we reject it.
    *)

    and process_node_section (SectNode {path, parts, pos}) : NodeConfig option =
    (let
	val url_path = (URL.parseSimplePath path)
		handle URL.BadURL s => (
		    Log.errorP pos ["Invalid node path: ", s];
		    raise Bad
		    )

	val node_path = URLPathToList url_path

	val _ =
	    if duplicate_path node_path
	    then
	    (
		Log.errorP pos ["The node's URL is already configured."];
		raise Bad
	    )
	    else
		()

	(*  Extract the parts we recognise and then see if anything is
	    left over.
	*)
	val (kind, krest)    = find_kind parts pos
	val (options, orest) = process_options krest
	val (auth, arest)    = process_auth orest
    in
	app unrec_param arest;

	SOME(NodeConfig {
	    path    = node_path,
	    kind    = kind,
	    options = options,
	    auth    = auth
	    })
    end
    handle Bad => NONE)

    |   process_node_section _ = raise InternalError "Config,process_node_section"



    (*  We are only looking for options and authorisation.
	Other parameters are just rejected.
    *)
    and process_swerve_section (SectSwerve {parts}) =
    (let
	val (options, orest) = process_options parts
	val (auth, arest)    = process_auth orest
    in
	app unrec_param arest;

	SOME(SwerveConfig {
	    options = options,
	    auth    = auth
	    })
    end
    handle Bad => NONE)

    |   process_swerve_section _ = raise InternalError "Config,process_swerve_section"



    (*	Check for at most one of Directory, BuiltIn or Script.
	Raise Bad if the kind can't be trusted.

	We Check that the directory exists and is readable.
	REVISIT - We need to check that the script path is accessible and
	that the builtin handler is valid.
    *)
    and find_kind parts pos : (NodeKind * SectionPart list) =
    let
	val (fnd, rest) = find_parts ["Directory", "BuiltIn", "Script"] parts

	fun fix_path prefix file_name =
	(
	    if Files.absPath file_name
	    then
		file_name
	    else
		Files.appendFile prefix file_name
	)

	val kind =
	    case fnd of
	      [("Directory", lits, pos)] =>
		let
		    val ServerConfig {doc_dir, ...} = getServerConfig()
		    val path = fix_path doc_dir (expect_one_string lits pos)
		in
		    expect_accessible_dir path pos;
		    NodeIsDir {path = path}
		end

	    | [("BuiltIn", lits, pos)] =>
		let
		    val name = expect_one_string lits pos
		in
		    NodeIsBuiltin {name = name}
		end

	    | [("Script", lits, pos)] =>
		let
		    val ServerConfig {cgi_dir, ...} = getServerConfig()
		    val path = fix_path cgi_dir (expect_one_string lits pos)
		in
		    expect_execable_file path pos;
		    NodeIsScript {path = path}
		end

	    | _ => (
		Log.errorP pos ["Exactly one of Directory, BuiltIn or Script is allowed."];
		raise Bad
		)
    in
	(kind, rest)
    end


    (*	Parse the Options part.
	Raise Bad if the options can't be trusted.
    *)
    and process_options parts : (NodeOptionFormula list * SectionPart list) =
    let
	(*  Examine the first keyword which is [inherit|all|none]
	    An empty list is used to represent a missing first keyword.
	*)
	fun cvt_first []          pos = ([], [])
	|   cvt_first (lit::rest) pos =
	let
	    val kw = upperCase(lit_as_string lit)
	in
	    case kw of
	      "INHERIT"		=> ([NOFInherit], rest)
	    | "ALL"		=> ([NOFAll], rest)
	    | "NONE"		=> ([NOFNone], rest)

	    | _ =>
	    (
		Log.errorP pos ["The options must start with inherit, all or none"];
		raise Bad
	    )
	end


	fun cvt_word pos lit : NodeOptionFormula =
	let
	    val kw = upperCase(lit_as_string lit)

	    val (oper, base) =
		if size kw > 0
		then
		    case String.sub(kw, 0) of
		      #"+" => (NOFAdd, String.extract(kw, 1, NONE))
		    | #"-" => (NOFSub, String.extract(kw, 1, NONE))
		    | _    => (NOFAdd, kw)
		else
		    (NOFAdd, kw)
	in
	    case base of
	      "EXECCGI"		=> oper NodeExecCGI
	    | "FOLLOWSYMLINKS"	=> oper NodeFollowSymLinks
	    | "WITHSUBDIRS"	=> oper NodeWithSubDirs
	    | _ => (Log.errorP pos ["Unrecognised option: ", kw]; raise Bad)
	end

	val (fnd, rest_parts) = find_parts ["Options"] parts

	val formulas =
	    case fnd of
	      [] => []			(* no Options part *)

	    | [(_, lits, pos)] =>
		let
		    val (first, frest) = cvt_first lits pos
		    val rest = List.map (cvt_word pos) frest
		in
		    List.concat[first, rest]
		end

	    | ((_, _, pos)::_) => (
		Log.errorP pos ["At most one Options part is allowed."];
		raise Bad
		)

    in
	(formulas, rest_parts)
    end



    (*	This will raise Bad if anything is wrong. *)
    and process_auth parts : (NodeAuth * SectionPart list) =
    let
	val (fnd, rest_parts) = find_parts
		["AuthType",       "AuthName",
		 "AuthUserFile",   "AuthGroupFile",
		 "AuthAllowUsers", "AuthAllowGroups"]
		parts

	(*  The part may be present in fnd. *)
	fun get1 name =
	(
	    case List.find (fn (n, _, _) => kw_match n name) fnd of
	      SOME (_, lits, pos) => SOME(expect_one_string lits pos)
	    | _                   => NONE
	)

	fun get_pos name =
	(
	    case List.find (fn (n, _, _) => kw_match n name) fnd of
	      SOME (_, _, pos) => pos
	    | _ => raise InternalError "Config,process_auth"
	)

	fun req1 name =
	(
	    case get1 name of
	      NONE => (
		Log.errorP (get_pos "AuthType")
			["The ", name, " parameter must be supplied."];
		raise Bad
		)

	    | SOME s => s
	)

	fun getn name =
	(
	    case List.find (fn (n, _, _) => kw_match n name) fnd of
	      SOME (_, lits, pos) => map lit_as_string lits
	    | _                   => []
	)


	(*  If the path is relative then it is relative to the server root.
	    Check that the file is readable.

	    We expect that we don't get here unless the server config has
	    been successfully processed.
	*)
	fun get_file name =
	let
	    val file_name = req1 name
	    val path =
		if Files.absPath file_name
		then
		    file_name
		else
		    Files.appendFile (getServerRoot()) file_name
	in
	    expect_readable_file path (SOME(get_pos name));
	    path
	end


	fun build() =
	(
	    Log.inform Log.Debug (fn() => TF.str "process_auth build");

	    NodeBasic {
		realm	    = req1 "AuthName",
		user_file   = get_file "AuthUserFile",
		group_file  = get_file "AuthGroupFile",
		users	    = getn "AuthAllowUsers",
		groups      = getn "AuthAllowGroups"
		}
	)

	val auth =
	    case get1 "AuthType" of
	      NONE   => NodeNoAuth	(* no auth, even if other parts appear *)
	    | SOME t =>
		if kw_match t "Basic"
		then
		    build()
		else
		(
		    Log.errorP (get_pos "AuthType")
			["The AuthType must be Basic."];
		    raise Bad
		)
    in
	(auth, rest_parts)
    end


    (*	Find all of the parts that match the names.
	Names in sections are canonicalised to the names in the list.
	The order of the remainder is preserved so that we always
	deal with the first of any duplicate.
    *)
    and find_parts names parts
	: ((string * Literal list * SrcPos) list * SectionPart list) =
    let
	fun partition []           fnd skip = (rev fnd, rev skip)
	|   partition (part::rest) fnd skip =
	let
	    val SectionPart {left, right, pos} = part
	    val f = List.find (fn n => kw_match n left) names
	in
	    case f of
	      NONE       => partition rest fnd (part::skip)
	    | SOME canon => partition rest ((canon, right, pos)::fnd) skip
	end
    in
	partition parts [] []
    end


    and kw_match a b = (upperCase a = upperCase b)


    and sameNode (NodeConfig {path=p1, ...})
                 (NodeConfig {path=p2, ...}) = sameNodePath p1 p2


    (*	See if the proposed new path is already in use.
    *)
    and duplicate_path path =
    let
	fun same (NodeConfig {path=p1, ...}) = sameNodePath p1 path
    in
	List.exists same (!cf_nodes)
    end


    and findNodeConfig node_path =
    let
	fun same (NodeConfig {path=p1, ...}) = sameNodePath p1 node_path
    in
	List.find same (!cf_nodes)
    end
    handle _ => NONE


(*------------------------------------------------------------------------------*)

    (*	Value processing *)


    and expect_one_string [l] pos = lit_as_string l
    |   expect_one_string  _  pos =
    (
	Log.errorP pos ["The parameter takes one word or string value"];
	raise Bad
    )


    and expect_opt_string [l] pos = SOME(lit_as_string l)
    |   expect_opt_string []  pos = NONE
    |   expect_opt_string  _  pos =
    (
	Log.errorP pos ["The parameter takes at most one word or string value"];
	raise Bad
    )


    and expect_one_integer [l] pos = lit_as_int l
    |   expect_one_integer  _  pos =
    (
	Log.errorP pos ["The parameter takes one integer value"];
	raise Bad
    )


    and lit_as_string (LitIsString (s, _)) = s
    |   lit_as_string (LitIsInt (n, pos))  =
    (
	Log.errorP pos ["The value must be a string, not a number"];
	raise Bad
    )


    and lit_as_int (LitIsInt (n, pos))    = n
    |   lit_as_int (LitIsString (_, pos)) =
    (
	Log.errorP pos ["The value must be a number, not a string"];
	raise Bad
    )


    (*	Check that a disk path is absolute. *)

    and expect_absolute path pos =
    (
	if Files.absPath path
	then
	    ()
	else
	(
	    Log.errorP pos [path, " is not absolute"];
	    raise Bad
	)
    )


    (*	Check that a disk path is accessible for reading and searching. *)

    and expect_accessible_dir path pos =
    (
	if Files.accessibleDir path
	then
	    ()
	else
	(
	    Log.errorP pos [path, " is not accessible"];
	    raise Bad
	)
    )


    (*	Check that we can read from a regular file.
    *)

    and expect_readable_file path opt_pos =
    let
	val logger =
		case opt_pos of
		  NONE     => Log.error
		| SOME pos => Log.errorP pos
    in
	if Files.readableReg path
	then
	    ()
	else
	(
	    if Files.exists path
	    then
		logger [path, " is not readable"]
	    else
		logger [path, " does not exist"];

	    raise Bad
	)
    end


    (*	Check that we can execute a regular file.
    *)

    and expect_execable_file path pos =
    (
	expect_exists path pos;		(* a common error *)

	if Files.execableReg path
	then
	    ()
	else
	(
	    Log.errorP pos [path, " is not executable"];
	    raise Bad
	)
    )


    and expect_exists path pos =
    (
	if Files.exists path
	then
	    ()
	else
	(
	    Log.errorP pos [path, " does not exist"];
	    raise Bad
	)
    )



    (*	Check that we can write to a file either because it exists and
	is writable or else we can create it in its directory.  The path
	should be absolute.
    *)

    and expect_writable_file path pos =
    (
	if Files.exists path
	then
	(
	    if Files.writableReg path
	    then
		()
	    else
	    (
		Log.errorP pos [path, " is not writable."];
		raise Bad
	    )
	)
	else
	if Files.canCreateInDir(Files.dirName path)
	then
	    ()
	else
	(
	    Log.errorP pos ["Cannot create in ", Files.dirName path];
	    raise Bad
	)
    )


    and expect_positive value pos =
    (
	if value > 0
	then
	    ()
	else
	(
	    Log.errorP pos ["The value must be greater than zero."];
	    raise Bad
	)
    )


    and expect_range value min max pos =
    (
	if value >= min andalso value <= max
	then
	    ()
	else
	(
	    Log.errorP pos
		(if max < valOf(Int.maxInt)
		 then
		    ["The value must be greater than or equal to ",
		    	   Int.toString min]
		 else
		    ["The value must be in the range ",
			Int.toString min, " to ", Int.toString max]
		);
	    raise Bad
	)
    )


    and warn_duplicate name pos =
    (
	Log.logP Log.Warn pos
	    (TF.concat ["A duplicate ", name, " parameter has been ignored"])
    )

(*------------------------------------------------------------------------------*)

    (*	Mime file reading.

	This only happens at initialisation time so we won't be aborting.
    *)

    and process_mime_file() =
    let
	val ServerConfig {mime_file, ...} = getServerConfig()
    in
	if Files.readableReg mime_file
	then
	    FileIO.withTextIn (Abort.never())
	    	mime_file () (read_mime mime_file)
	else
	    Log.error ["The MIME types file is not readable: ", mime_file]
    end



    and read_mime mime_file stream : unit =
    let
	fun loop lnum =
	    (
	     case TextIO.inputLine stream of
		 NONE => ()
	       | SOME line => (do_line lnum line; loop (lnum+1))
	    )

	(*  Skip blank lines and those that start with #. The rest look like
		application/news-transmission
		application/octet-stream	bin dms lha lzh exe class
	*)
	and do_line lnum line =
	let
	    fun insert pair ext =
		    STRT.insert mime_table (upperCase(SS.string ext), pair)
	in
	    case SS.tokens Char.isSpace (SS.full line) of
	      [] => ()			(* is blank *)

	    | (f1::rest) =>
	    (
		if SS.isPrefix "#" f1
		then
		    ()			(* skip comments *)
		else
		(
		    case SS.fields (isVal #"/") f1 of
		      [maj, min] =>
			  app (insert (SS.string maj, SS.string min)) rest

		    | _ => Log.error ["Invalid MIME type on line ",
			    Int.toString lnum, " in file ", mime_file]
		)
	    )
	end
    in
	loop 1
    end


    and lookupMime ext = STRT.find mime_table (upperCase ext)

(*------------------------------------------------------------------------------*)
end
