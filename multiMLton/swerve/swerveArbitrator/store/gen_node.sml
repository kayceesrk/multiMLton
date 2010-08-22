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

(*  This contains the common machinery for the different kinds of nodes.

 * When a directory node is created it must initialise itself with
 * information from the .swerve file. This may change the security
 * configuration and the option flags.  The node cannot continue
 * processing, even forwarding messages, until the initialisation
 * is complete so the initialisation will be done synchronously.

 * While operating there is a mailbox between the generic or "master"
 * node and the handler thread.  This allows multiple requests to
 * be handled concurrently by a smart-enough handler.  They can be
 * responded to in any order. *)

signature GENERIC_NODE =
sig

    (*	This is some arg that is specific to the kind of node. *)
    type CreateArg

    (*	This creates a generic node which is implemented by the handler thread. *)
    val create:	{ name:     string, (* segment for the node name *)
		  arg:      CreateArg,
		  config:   Config.NodeConfig,
		  factory:  Node.NodeCreator,
		  options:  Node.Options,
		  children: Node.Node list
		} -> Node.Node option

end




functor GenericNodeFn(
    structure Handler: NODE_HANDLER
    ): GENERIC_NODE =
struct

    structure H      = Handler

    structure M      = MLton.PCML.Mailbox
    structure TF     = TextFrag
    structure Cfg    = Config
    structure Req    = HTTPMsg
    structure Status = HTTPStatus
    structure Hdr    = HTTPHeader
    structure U      = ResponseUtils
    structure G      = Globals

    open Common
    open Node


(*------------------------------------------------------------------------------*)

    type CreateArg = H.CreateArg

    (*	This is constant configuration for a node at the generic level.
    *)
    datatype NodeImpl = NodeImpl of {
	name:		string,
	in_mbox:	NodeMsg M.mbox,
	hnd_mbox:	HndMbox,	    (* msgs to the handler *)
	hnd_reply:	HndReply MLton.PCML.chan,  (* replies from the handler *)
	config:		Cfg.NodeConfig,	    (* the original *)
	auth:		Cfg.NodeAuth,
	options:	Node.Options,
	factory:	Node.NodeCreator
	}


    (*	Node state at the generic level.

	Here we assume that there won't be a large fanout at each node.
	A linear search will do.  For directory nodes we will only count
	the sub-directories in the directory as children.

    *)
    and GenState = GenState of {
	children:	Node list
	}


(*------------------------------------------------------------------------------*)

    (*	This may raise an exception out of H.init. *)
    fun create {name, arg, config, factory, options, children} =
    let
	val Cfg.NodeConfig {auth, ...} = config

	val in_mbox = M.mailbox()
	val node    = Node {name = name, in_mbox = in_mbox}

	val (h_mbox, h_config_opt) = H.init arg

	(*  Update the options and authorisation from the node
	    handler.
	*)
	val (final_auth, final_options) =
	    case h_config_opt of
	      NONE     => (auth, options)
	    | SOME cfg => merge_config cfg auth options

	val impl = NodeImpl {
			name     = name,
			in_mbox  = in_mbox,
			hnd_mbox = h_mbox,
			hnd_reply= MLton.PCML.channel(),
			config   = config,
			auth     = final_auth,
			options  = final_options,
			factory  = factory
			}

	val gstate = GenState {
			children = children
			}
    in
	MLton.PCML.spawn (node_server impl gstate);
	SOME node
    end
    handle x => (Log.logExn x; NONE)



    and merge_config swerve_config old_auth old_options =
    let
	val Cfg.SwerveConfig {auth, options} = swerve_config

	(*  The authorisation is replaced as a whole.
	*)
	val final_auth =
	    case auth of
	      Cfg.NodeNoAuth => old_auth
	    | a => a

	val final_options = Node.performFormula old_options options
    in
	(final_auth, final_options)
    end



    and node_server impl gstate () =
    let
	val NodeImpl {in_mbox, hnd_reply, ...} = impl

	fun loop (state: GenState) =
	let
	    val new_state =
		MLton.PCML.select[
		    MLton.PCML.wrap(M.recvEvt in_mbox,
			MyProfile.timeIt "GenNode request"
			    (handle_request impl state)),

		    MLton.PCML.wrap(MLton.PCML.recvEvt hnd_reply, handler_reply impl state)
		    ]
	in
	    loop new_state
	end
    in
	loop gstate
    end



    (*	There must be an authentication check at each node that
	the request passes through.

	If the segs list is empty then this is the node that handles
	the request. If there is one entry left then this could be a
	directory node that handles a file.

	This doesn't actually change the state.
    *)

    and handle_request impl gstate in_msg : GenState =
    let
	val HTTPRequest {request, segs} = in_msg

	val NodeImpl {name, config, factory, options,
			hnd_mbox, hnd_reply, auth, ...} = impl

	val () = Log.testInform G.TestStoreProto Log.Debug
		    (fn() => TF.concat ["Node ", name, " gets a request"])

	fun handle_it() =
	let
	    val () = Log.testInform G.TestStoreProto Log.Debug
			(fn() => TF.concat ["Node ", name, " handles it"])

	    val msg = HndReq {
			factory	    = factory,
			config	    = config,
			options	    = options,
			request	    = request,
			segs	    = segs,
			rchan	    = hnd_reply
			}
	in
	    M.send(hnd_mbox, msg)
	end



	and forward_child key rest =
	let
	    val () = Log.testInform G.TestStoreProto Log.Debug
		(fn() => TF.concat [
		    "Forwarding to child for key ", key,
		    ", rest=", Cfg.listToString rest])

	    val GenState {children, ...}  = gstate

	    fun match (Node {name, ...}) = (name = key)
	in
	    case List.find match children of
	      NONE       => no_child key rest
	    | SOME child => pass_to child request rest
	end



	and no_child key rest =
	let
	    val () = Log.testInform G.TestStoreProto Log.Debug
	    	(fn() => TF.concat [
		    "No child for key ", key,
		    ", rest=", Cfg.listToString rest])
	in
	    if H.canTakeRest config orelse
		(null rest andalso H.canTakeLast config)
	    then
		handle_it()
	    else
		U.return request (U.mkNotFound())
	end

    in
	NodeAuth.checkAuth auth request;    (* raises Respond on error *)

	case segs of
	  []          => handle_it()
	| (key::rest) => forward_child key rest;

	gstate
    end
    handle
      Respond resp =>
	let
	    val HTTPRequest {request, ...} = in_msg
	in
	    U.return request resp;
	    gstate
	end

    | x =>
	let
	    val HTTPRequest {request, ...} = in_msg
	in
	    Log.logExn x;
	    U.return request (U.mkServerFail());
	    gstate
	end



    and handler_reply impl gstate reply : GenState =
    let
	val NodeImpl {name, config, factory, options, ...} = impl
    in
	case reply of
	  HndResponse (h_req, resp) =>
	    let
		val HndReq {request, ...} = h_req
	    in
		U.return request resp;
		gstate
	    end

	| HndSprout (h_req, child) =>
	    let
		val HndReq {request, segs, ...} = h_req

		val GenState {children} = gstate

		val new_gstate = GenState {
			children = child::children
			}

		val rest = if null segs then [] else tl segs
	    in
		pass_to child request rest;
		new_gstate
	    end

    end



    (*	Pass the original request down to the child node.
    *)
    and pass_to node req rest : unit =
    let
	val new_msg = HTTPRequest {
			request = req,
			segs    = rest
			}
    in
	Node.send node new_msg
    end

(*------------------------------------------------------------------------------*)

end
