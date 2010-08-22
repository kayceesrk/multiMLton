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


(* Copyright (c) 2001 Anthony L Shipman *)

(* This implements a store node that represents a directory. *)


structure DirNodeHandler: NODE_HANDLER =
struct
  
  structure M      = Mailbox
  structure TF     = TextFrag
  structure Cfg    = Config
  structure Req    = HTTPMsg
  structure Status = HTTPStatus
  structure U      = ResponseUtils
  structure N      = Node
  structure Cmn    = Common		     


    (*	This is actually constant configuration info. *)
  datatype State = State of {
	   dir:    string	(* the directory path *)
  }
    			    
  type CreateArg  = string	(* the directory path *)
		    
		    
  (* We can take the rest in order to try to create a chain of 
   * child directories. *)
  fun canTakeRest _ = true
  fun canTakeLast _ = true
		      
		      
  (* Read the .swerve file for the directory.
   * Spawn a thread to serve messages from the master.
   * This may raise exceptions out of processNodeFile. *)
    fun init dir_path =
	let	val file = Files.appendFile dir_path ".swerve"
	    val opt_config = 
		if Files.exists file
		then Cfg.processNodeFile file
	    else NONE
		 
	    val state = State {dir = dir_path}
	    val mbox  = M.mailbox()
	in
	    CML.spawn (server mbox state);
	    (mbox, opt_config)
	end




    and server mbox init_state () =
    let
	(*  Actually the state, as it is at the moment, does not change.
	*)
	fun loop state =
	let
	    val new_state =
		case M.recv mbox of
		  msg as N.HndReq _ => handle_request msg state
	in
	    loop new_state
	end
    in
	loop init_state
    end



    and handle_request 
	    (msg as N.HndReq {factory, config, options, request, segs, rchan})
	    state =
    let
	val Cfg.NodeConfig {path = node_path, ...} = config
	val Req.Request  {url, abort, ...} = request
	val URL.HTTP_URL {path = url_path, ...} = url


	val () = Log.testInform Globals.TestStoreProto Log.Debug
	    (fn() => TF.concat [
		    "dir_node handle request node_path=",
		    Cfg.listToString node_path,
		    " url path=",
		    URL.pathToString url_path
		    ])

	(*  The segment may be a directory or a regular file.
	    If it is a directory then sprout a child and the request
	    will get routed to it later.

	    No parameters are relevant.  We'll ignore them.
	*)
	fun do_file dir_only file =
	let
	    val State {dir, ...} = state
	    val N.Options {follow_sym, with_subdirs, ...} = options

	    val file_path = Files.appendFile dir file

	    val () = Log.testInform Globals.TestStoreProto Log.Debug
		(fn() => TF.concat ["Looking at file ", file_path]);
	in
	    if not follow_sym andalso Files.isSym file_path
	    then
		bad()
	    else
	    if not dir_only andalso Files.isReg file_path
	    then
	    (
		if Files.readableReg file_path
		then
		    reply_response(send_file file_path request)
		else
		    reply_response(U.mkForbidden())
	    )
	    else
	    if Files.isDir file_path
	    then
	    (
		if with_subdirs andalso Files.accessibleDir file_path
		then
		    let
			val new_node_path = node_path @ [file]
		    in
			case sprout_child factory new_node_path file_path of
			  NONE       => reply_response(U.mkServerFail())
			| SOME child => reply_sprout child
		    end
		else
		    reply_response(U.mkForbidden())
	    )
	    else
		bad()
	end


	(*  If there are no segments left then we should be looking
	    for a file index.html in this directory. If it is not found
	    then we should be generating a fancy index for the directory.
	    REVISIT - Have a flag to disable fancy indexing for security.
	*)

	and index_dir() =
	let
	    val State {dir, ...} = state
	    val N.Options { follow_sym, ... } = options
	    val Cfg.ServerConfig {dir_index, ...} = Cfg.getServerConfig()

	    val file_path = Files.appendFile dir dir_index

	    val () = Log.testInform Globals.TestStoreProto Log.Debug
		(fn() => TF.concat ["Indexing directory ", file_path]);
	in
	    if not follow_sym andalso Files.isSym file_path
	    then
		bad()
	    else
	    if Files.isReg file_path
	    then
		reply_response(send_file file_path request)
	    else
		reply_response(fancy_index abort url dir)
	end


	and reply_response response =
	(
	    CML.send(rchan, N.HndResponse(msg, response))
	)


	and reply_sprout node =
	(
	    CML.send(rchan, N.HndSprout(msg, node))
	)


	and bad() = reply_response(U.mkNotFound())


	(*  If we are not at the end of a path then we can only
	    try to sprout a child directory.
	*)
	and do_segs []           = index_dir()
	|   do_segs [file]       = do_file false file
	|   do_segs (file::rest) = do_file true file

    in
	do_segs segs;
	state
    end



    (*	Sprout a child node named after the path with the given
	directory path.  This may return NONE if the factory fails.

	We inherit the with_subdirs option. The others must be overridden
	in each directory. REVISIT - Perhaps the follow_sym option should
	be automatically inherited?
    *)

    and sprout_child factory node_path dir_path : Node.Node option =
    let
	val options = N.Options {
	    exec_cgi	    = false,
	    follow_sym	    = false,
	    with_subdirs    = true
	    }

	val child_config = Cfg.NodeConfig {
		path	= node_path,
		kind	= Cfg.NodeIsDir {path = dir_path},
		options = [],		(* passed directly to the factory *)
		auth    = Cfg.NodeNoAuth
		}
    in
	factory {
	    config    = child_config,
	    children  = [],
	    options   = options
	    }
    end

(*------------------------------------------------------------------------------*)

    (*	Generate a response that returns a regular file.  *)
    and send_file file_path req : Req.Response =
    let
	val () = Log.testInform Globals.TestStoreProto Log.Debug
	    (fn() => TF.concat ["dir_node sends file ", file_path]);

	fun file_response() =
	let
	    val entity = Entity.Entity {
			 info = Info.emptyInfo,
			 body = Entity.fileProducer file_path
			 }
	in
	    Req.Response {
		status  = Status.OK,
		headers = [],
		entity  = entity
		}
	end

    in
	if Files.readableReg file_path
	then
	    file_response()
	else
	    U.mkForbidden()
    end



    (*	This lists the files in the directory.  It's not all that
	fancy.  It is similar to Netscape's listing with some
	icons.  The icons are always taken from the virtual path /icons.
    *)
    and fancy_index abort url dir : Req.Response =
    let
	val URL.HTTP_URL {host, port, userinfo, path = url_path, ...} = url
	val URL.URLPath  {segs, absolute} = url_path


	fun build entries =
	let
	    val text = TF.seq [header(), translate entries, trailer()]
	in
	    U.mkHTML Status.OK text
	end


	and translate entries : TF.t =
	let
	    val sorted = ListMergeSort.sort String.> entries
	    val file_width = 35	(* width for the file name and size *)

	    (*	This generates n blanks as a frag. *)
	    fun pad n = TF.seq (List.tabulate (n, fn _ => TF.sp))

	    fun cvt entry =
	    let
		val entry_text =
		    if size entry > file_width
		    then
			String.substring(entry, 0, file_width-3) ^ "..."
		    else
			entry

		val abs_path  = Files.appendFile dir entry
		val file_size = getOpt(FileIO.fileSize abs_path, 0)

		val (fs_text, suffix) = 
		    if file_size < 1024
		    then
			(Int64.toString file_size, " bytes")
		    else
			(Int64.toString (file_size div 1024), " Kb   ")

		(*  Padding between the entry name and the size. We want at 
		    least 1.
		*)
		val padding = Int.max(1, file_width - (size entry_text) - (size fs_text))

		val mod_time = getOpt(FileIO.modTime abs_path, Time.zeroTime)
		val mt_text  = Date.toString(Date.fromTimeUniv mod_time)

		val (file_type, icon) =
		    if Files.isDir abs_path
		    then
			("Directory", "dir.gif")
		    else
		    if Files.isReg abs_path
		    then
			("", "text.gif")
		    else
			("", "unknown.gif")

		fun mk_icon name = 
		let
		    val icon_url = segs_to_url ["icons", name]
		in
		    TF.concat ["<IMG ALIGN=BOTTOM BORDER=0 SRC=\"", 
		               URL.URLToString icon_url,
			       "\">"]
		end
		
	    in
		TF.seq [TF.str "<A HREF=\"",
		      TF.str (URL.URLToString (file_to_url entry)),
		      TF.str "\">", 
		      mk_icon icon,
		      TF.str entry_text,
		      TF.str "</A>", pad padding, TF.sp,
		      TF.str fs_text, TF.str suffix, TF.sp,
		      TF.str mt_text, TF.sp, TF.str file_type,
		      TF.nl
		     ]
	    end
	in
	    TF.seq(map cvt sorted)
	end


	and header() : TF.t =
	let
	    val msg = TF.concat ["Directory listing of ", dir]
	in
	    TF.seq [TF.str "<TITLE>", msg, TF.str "</TITLE>", TF.nl,
	          TF.str "<H1>", msg, TF.str "</H1>", TF.nl,
	          TF.str "<PRE><A HREF=\"",
		      TF.str(URL.URLToString (make_parent_url())),
		      TF.str "\">Up to higher level directory</A>", TF.nl
		 ]
	end


	and trailer() : TF.t = TF.seq [TF.str "</PRE>", TF.nl]


	(*  REVISIT - These look like they should be utility functions
	    in the URL module.
	*)
	and make_parent_url() =
	let
	    val parent_segs = List.take(segs, length segs - 1)
	    val parent_urlpath = URL.URLPath {segs=parent_segs, absolute=absolute}
	in
	    make_url parent_urlpath
	end


	and file_to_url file =
	let
	    val file_segs = segs @ [URL.Segment {part=file, params=[]}]
	    val file_path = URL.URLPath {segs=file_segs, absolute=absolute}
	in
	    make_url file_path
	end


	and segs_to_url seg_list =
	let
	    fun mk seg = URL.Segment {part=seg, params=[]}
	    val path = URL.URLPath {segs=map mk seg_list, absolute=absolute}
	in
	    make_url path
	end


	and make_url path =
	(
	    URL.HTTP_URL {
		host = host,
		port = port,
		userinfo = userinfo,
		path = path,
		query = NONE,
		fragment = NONE
		}
	)
    in
	Log.testInform Globals.TestStoreProto Log.Debug (fn() => TF.concat [
		    "dir_node accessibleDir of ", dir, 
		    " is ", Bool.toString(Files.accessibleDir dir)
		    ]);

	if Files.accessibleDir dir
	then
	(
	    (build(FileIO.listDir abort dir))
		handle _ => U.mkServerFail()
	)
	else
	    U.mkForbidden()
    end

(*------------------------------------------------------------------------------*)

end
