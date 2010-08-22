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

(* $Id: node.sml,v 1.13 2001/08/16 21:56:12 felix Exp $ *)

(*  This represents store nodes of different kinds.

@#34567890123456789012345678901234567890123456789012345678901234567890
*)

structure Node =
struct

    (*	These are the messages that a node can receive.

	The segment list is the list of uninterpreted segments that
	remain as the message trickles down through the nodes.

    *)

    datatype NodeMsg =
	    HTTPRequest of {
		request:    HTTPMsg.Request,
		segs:	    string list	    (* remaining path segments *)
		}


    (*	All nodes appear as just an input channel, with unlimited
	buffering. This prevents a slow node from congesting the
	tree above it.

	The name is the last part of the path that leads
	to the node.  For the root node it is the empty string.
    *)
    datatype Node = Node of {
	    name:	string,
	    in_mbox:    NodeMsg MLton.PCML.Mailbox.mbox
	    }


    (*	These are node flags that are computed from the configuration
	formulas.
    *)
    datatype Options = Options of {
	    exec_cgi:	    bool,
	    follow_sym:	    bool,
	    with_subdirs:   bool
	    }


    (*	It's convenient to generate an error response in the middle of
	node processing by raising this.  See GenericNode.
    *)
    exception Respond of HTTPMsg.Response


    (*	This is the creator in the node factory.
	It may return NONE if the create fails.
    *)
    type NodeCreator = {
	    config:	Config.NodeConfig,  (* URL path that reaches this node. *)
	    children:	Node list,	    (* child nodes *)
	    options:	Options
	    } ->
	    Node option


    (*	This is the request passed to the handler's mailbox.
	It should not be modified in the reply.
    *)
    datatype HndMsg = HndReq of {
		factory:    NodeCreator,
		config:     Config.NodeConfig,
		options:    Options,
		request:    HTTPMsg.Request,
		segs:	    string list,    (* remaining path segments *)
		rchan:	    HndReply MLton.PCML.chan
		}


    (*	If the handler wants to return a HTTP response it returns
	a HndResponse through the rchan of the above request.  It is
	returned to the master just in case it has some post-processing
	to do.	Otherwise we could return it directly to the client
	connection.

	The handler may create a child node and have the request forwarded
	to the child by replying with HndSprout. It must return a copy
	of the original message.

    *)
    and HndReply =
	    HndResponse of HndMsg * HTTPMsg.Response
	|   HndSprout   of HndMsg * Node


    type HndMbox = HndMsg MLton.PCML.Mailbox.mbox



    (*	Send a request to the node.
    *)
    fun send (Node {in_mbox, ...}) msg =
    let
      val _ = Debug.debug' "sending a request"
    in
      MLton.PCML.Mailbox.send(in_mbox, msg)
    end

(*------------------------------------------------------------------------------*)

    local
	structure Cfg = Config
	open Common
    in
	fun initOptions flag =
	(
	    Options {
		exec_cgi	= flag,
		follow_sym	= flag,
		with_subdirs    = flag
		}
	)


	and performFormula parent_options [] =
	(
	    initOptions false			(* empty formula = none *)
	)
	|   performFormula parent_options (formula: Cfg.NodeOptionFormula list) =
	let
	    fun match_all Cfg.NOFAll = true
	    |   match_all _        = false

	    fun match_inherit Cfg.NOFInherit = true
	    |   match_inherit _            = false

	    fun match_none Cfg.NOFNone = true
	    |   match_none _         = false


	    fun put (Cfg.NOFAdd opt, options) = set options opt true
	    |   put (Cfg.NOFSub opt, options) = set options opt false
	    |   put (_, options) = options

	    and set options Cfg.NodeExecCGI flag        = setExecCGI options flag
	    |   set options Cfg.NodeFollowSymLinks flag = setFollowSymLinks options flag
	    |   set options Cfg.NodeWithSubDirs flag    = setWithSubDirs options flag


	    val start =
		if List.exists match_all formula
		then
		    initOptions true
		else
		if List.exists match_none formula
		then
		    initOptions false
		else
		if List.exists match_inherit formula
		then
		    parent_options
		else
		    raise InternalError "Node,performFormula"

	    val result = foldl put start formula
	in
	    result
	end



	and setExecCGI (Options {exec_cgi, follow_sym, with_subdirs}) flag =
	(
	    Options {
		exec_cgi	= flag,
		follow_sym	= follow_sym,
		with_subdirs    = with_subdirs
		}
	)


	and setFollowSymLinks (Options {exec_cgi, follow_sym, with_subdirs}) flag =
	(
	    Options {
		exec_cgi	= exec_cgi,
		follow_sym	= flag,
		with_subdirs    = with_subdirs
		}
	)


	and setWithSubDirs (Options {exec_cgi, follow_sym, with_subdirs}) flag =
	(
	    Options {
		exec_cgi	= exec_cgi,
		follow_sym	= follow_sym,
		with_subdirs    = flag
		}
	)

    end

(*------------------------------------------------------------------------------*)

end


