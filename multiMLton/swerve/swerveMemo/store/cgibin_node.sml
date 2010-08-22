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

structure CgiNodeHandler: NODE_HANDLER =
struct

  structure Cmn      = Common
		       
  structure M      = Mailbox
  structure TF     = TextFrag
  structure SS     = Substring
		     
  structure Cfg    = Config
  structure Req    = HTTPMsg
  structure Hdr    = HTTPHeader
  structure Status = HTTPStatus
  structure E      = Entity
  structure U      = ResponseUtils
		     
    (*	This is raised if an abort condition is detected while
     * talking to the script. *)
  exception Aborted

  type CreateArg  = string
		    
  fun canTakeLast _ = true
  fun canTakeRest _ = true
		      
		      
  fun init script =
      let val mbox  = M.mailbox()
      in
	  CML.spawn (server mbox script);
	  (mbox, NONE)
      end
      
      
      
  and server mbox script () =
      let
	  fun loop() =
	      (
	       case M.recv mbox of
		   msg as Node.HndReq _ => handle_request msg script 
				      
	     ; loop()
	      )
      in
	  loop()
      end

    and handle_request 
	    (msg as Node.HndReq {config, rchan, request, segs, ...})
	    script =
    let
	val env  = build_environ config request (length segs)
	val resp = run_script script env request
    in
	CML.send(rchan, Node.HndResponse(msg, resp))
    end





    (*	We set the environment variables from scratch.

	num_left is just used to tell us how much of the tail of the URL 
	hasn't been interpreted yet. 
    *)
    and build_environ config request num_left =
    let
	val Cfg.NodeConfig {auth, ...} = config

	val Req.Request {url, headers, method, protocol, client, ...}
							  = request
	val URL.HTTP_URL {path, query, fragment, ...} = url
	val URL.URLPath {segs, ...} = path

	val script_path = URL.URLPath {
			    segs = List.take(segs, length segs - num_left),
			    absolute = false
			    }

	val trail_path = URL.URLPath {
			    segs = List.drop(segs, length segs - num_left),
			    absolute = false
			    }

	(*  Copy across approved variables.
	*)
	fun copy n =
	(
	    case OS.Process.getEnv n of
	      NONE   => NONE
	    | SOME v => SOME(concat[n, "=", v])
	)

	val copied = List.mapPartial copy
			["PATH", "HOSTNAME", "LANG", "LOGNAME", 
		         "HOME", "LD_LIBRARY_PATH", "SHELL"]

	(*  Add in the CGI environment variables.

	    Note that the perl CGI.pm module expects that QUERY_STRING is
	    omitted if there is no query.
	*)
	val Cfg.ServerConfig {server_name, listen_port, ...} =
		Cfg.getServerConfig()

	val basics = [
		"SERVER_NAME="     ^ server_name,
		"SERVER_PORT="     ^ (Int.toString listen_port),
		"SERVER_SOFTWARE=" ^ Globals.cgi_version,
		"REQUEST_METHOD="  ^ (Req.methodToString method),
		"SERVER_PROTOCOL=" ^ protocol,
		"GATEWAY_INTERFACE=CGI/1.1",
		"PATH_INFO="       ^ (URL.pathToString trail_path),
		"SCRIPT_NAME="     ^ (URL.pathToString script_path),

		(*  We don't set REMOTE_HOST, the script can find
		    it if it wants.
		*)
		"REMOTE_ADDR="     ^ (NetHostDB.toString client)
		]

	(*  REVISIT - We only know the authorisation
	    directly on this node, not on higher nodes.
	*)
	val auth_env : string list =
		case auth of
		  Cfg.NodeNoAuth  => []
		| Cfg.NodeBasic _ => ["AUTH_TYPE=Basic"]

	val user_env : string list =
		case Hdr.getAuth headers of
		  NONE => []
		| SOME (Hdr.AuthBasic (opt_id, pwd)) =>
		(
		    case opt_id of
		      NONE => []
		    | SOME id => ["REMOTE_USER=" ^ id]
		)

	val ctype_env : string list =
		case Hdr.getContentType headers of
		  NONE       => []
		| SOME mtype => [ "CONTENT_TYPE=" ^
				  ( TF.toString ( MimeType.format mtype ) ) ]

	val clen_env : string list =
		case Hdr.getContentLength headers of
		  NONE     => []
		| SOME len => [ "CONTENT_LENGTH=" ^ ( Int.toString len ) ]

	val query_env : string list =
		case query of
		  NONE   => []
		| SOME q => ["QUERY_STRING=" ^ q]


	(*  Copy all other request headers that we haven't dealt with above.
	*)
	fun hdr_copy (Hdr.HdrAuthorization _) = NONE
	|   hdr_copy (Hdr.HdrConLen _)        = NONE
	|   hdr_copy (Hdr.HdrConType _)       = NONE
	|   hdr_copy (Hdr.HdrChallenge _)     = NONE
	|   hdr_copy (Hdr.HdrBad _)           = NONE
	|   hdr_copy header = 
	let
	    (*  Find the initial colon, split off any white space after it.
	     * Header names become uppercase with hyphens mapped to
	     * underscores. *)
	    val text = SS.full ( TF.toString ( TF.seq [Hdr.formatHeader header, TF.crlf ] ))
	    val (left, right) = SS.splitl ( Common.isntVal #":" ) text

	    fun cvt #"-" = "_"
	    |   cvt c    = str ( Char.toUpper c )

	    val ename  = SS.translate cvt left
	    val evalue = SS.dropl Char.isSpace ( SS.triml 1 right )
	in
	    SOME ( concat [ "HTTP_", ename, "=", SS.string evalue ] )
	end

	val other_headers = List.mapPartial hdr_copy headers

	val final_headers = List.concat [ copied, basics, auth_env,
					  user_env, ctype_env, clen_env, query_env,
					  other_headers ]
    in
	final_headers
    end

(*------------------------------------------------------------------------------*)

    and run_script script env request : Req.Response =
    let
	(*  The Aborted exception can be raised in here. *)
	val Req.Request {abort, ...} = request


	fun talk holder =
	let
	    val (proc, _) = ExecReader.get holder
	    val ()        = send_entity abort proc request
	    val headers   = get_headers abort proc script

	    (*  We don't pass these to the client.
		The last four are handled by the Entity Info.
	    *)
	    fun select (Hdr.HdrStatus _)	= false
	    |   select (Hdr.HdrConType _)	= false
	    |   select (Hdr.HdrConLen _)	= false
	    |   select (Hdr.HdrConEnc _)	= false
	    |   select (Hdr.HdrLastModified _)	= false
	    |   select _ = true


	    val status =
		case Hdr.getStatus headers of
		  NONE   => Status.OK
		| SOME s => s


	    (*  This includes error responses from the script.
	    *)
	    fun normal_response() =
	    let
		val () = Log.testInform Globals.TestCGIProto Log.Debug
				(fn()=>TF.str "CGI normal_response")

		val entity = Entity.Entity {
				info = Hdr.toEntityInfo headers,
				body = Entity.procProducer holder
				}
	    in
		Req.Response {
		    status  = status,
		    headers = List.filter select headers,
		    entity  = entity
		    }
	    end
	in
	    normal_response()
	end
	handle _ =>
	    (
		kill (#1(ExecReader.get holder));
		U.mkServerFail()	(* REVISIT - should be ReqTimeout *)
	    )

    in
	(*  The holder will be closed in procProducer after the response
	    body has been delivered.  If there is an error then the
	    holder will eventually be finalised.
	*)
	case ExecReader.openIt abort (script, [], env) of
	  NONE        => U.mkServerFail()  (* error already reported *)
	| SOME holder => talk holder
    end



    (*	This is used to abort the child process. *)
    and kill proc = Unix.kill(proc, Posix.Signal.term)


    (*	This sends the entity concurrently. It may block if
	the script is not reading stdin.  It will be reaped
	by the GC if the entity is never accepted.

	If there is no body, which is the usual case, then only a
	XferDone message will be received.

	The child will be killed on the reading path if there is an abort.
    *)
    and send_entity abort proc request =
    let
	val Req.Request {entity, ...} = request
	val (_, ostream) = Unix.streamsOf proc

	val consumer = CML.channel()

	val () = Log.testInform Globals.TestCGIProto Log.Debug
		    (fn()=>TF.str "CGI send_entity")

	fun send_it() =
	(
	    case CML.recv consumer of
	      E.XferInfo _ => send_it()

	    | E.XferBytes vec => 
		(
		    TextIO.output(ostream, Byte.bytesToString vec);
		    send_it()
		)

	    | E.XferDone  => done()
	    | E.XferAbort => done()
	)

	and done() =
	(
	    TextIO.closeOut ostream
	)
    in
	E.startProducer abort (SOME entity) consumer;
	CML.spawn send_it;
	()
    end



    (*	Read the returned headers.
	This leaves the input stream at the beginning of the entity body.
	(It is readAllHeaders that stops at the blank line after the headers).
	Bad headers are filtered out.
    *)
    and get_headers abort proc script =
    let	val (istream, _) = Unix.streamsOf proc
	val () = Log.testInform Globals.TestCGIProto Log.Debug
				( fn ()=> TF.str "CGI get_headers" )
		 
	(*  This must match Connect.readLine.
	 * We strip the terminating \r\n. *)
	fun readLine() = 
	    if Abort.aborted abort
	    then NONE
	    else case TextIO.inputLine istream of
		     NONE => NONE			      
		   | SOME line => let val l = size line
				  in
				      if l > 1 andalso String.sub(line, l-2) = #"\r"
				      then SOME(String.substring(line, 0, l-2))
				      else if l > 0 andalso String.sub(line, l-1) = #"\n"
				      then SOME(String.substring(line, 0, l-1))
				      else SOME line
				  end		 

	(*  Log any bad headers and discard them. *)
	fun check [] out = out
	|   check ((Hdr.HdrBad h)::rest) out =
	(
	    Log.error ["CGI ", script, " returned bad header: ", h];
	    check rest out
	)
	|   check (h::rest) out = check rest (h::out)


	(*  Try to read some headers. This will return early on
	    an abort.
	*)
	val headers = Hdr.readAllHeaders readLine
    in
	if Abort.aborted abort
	then
	    raise Aborted
	else
	    check headers []
    end


(*------------------------------------------------------------------------------*)

end

