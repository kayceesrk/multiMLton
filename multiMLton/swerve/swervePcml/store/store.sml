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

(* $Id: store.sml,v 1.14 2002/03/10 17:18:25 felix Exp $ *)

(*  This describes the resource store.  It implements a singleton thread
    for the root of the store.

@#345678901234567890123456789012345678901234567890123456789012345
*)

signature STORE =
sig

    (*	This passes in a request to the store which will pass it
	on to a handler.
    *)
    val deliver:    HTTPMsg.Request -> unit

end


structure Store: STORE =
struct

    structure TF     = TextFrag
    structure Cfg    = Config
    structure Req    = HTTPMsg
    structure U      = ResponseUtils

    open Common

(*------------------------------------------------------------------------------*)

    (*	Hold the root as a singleton. *)
    val root_node: Node.Node option ref = ref NONE


    fun get_root() =
    let
	val url_path = URL.URLPath {segs = [], absolute = true}
    in
	case !root_node of
	  NONE =>
	    let
		val node = build_node_tree()
	    in
		root_node := SOME node;
		node
	    end

	| SOME n => n
    end


    and deliver req =
    let
	val Req.Request  {url, ...}  = req
	val URL.HTTP_URL {path, ...} = url

	val msg = Node.HTTPRequest {
		    request = req,
		    segs    = Cfg.URLPathToList path
		    }
    in
	Node.send (get_root()) msg
    end

(*------------------------------------------------------------------------------*)

    (*	Build the tree of known nodes from the bottom up.
	This requires sorting the node configuration records.

	If there is a gap, e.g. there are configs for /a and /a/b/c
	but not /a/b then we insert a pass-thru node for /a/b.

	The result will be the root node.
    *)

    and build_node_tree() : Node.Node =
    let
	fun add_paths (c as Cfg.NodeConfig {path, ...}) = (path, c)

	val all_configs = Cfg.getNodeConfigs()
	val with_paths  = map add_paths all_configs
	val root        = build_level [] (Node.initOptions false) with_paths
    in
	root
    end



    (*  This builds a tree node. The path argument is the path down to this node.
	The config pairs node configuration records that have path as a prefix.

	The node options are computed while doing this since they need a top-down
	traversal of the configuration to inherit the flags.

	We sort on the initial parts and split the configs into groups for
	these initial parts.  Each group will become a node.  The result
	will be the children for the parent.

	A string table will be convenient for sorting.
    *)
    and build_level path options config_pairs : Node.Node =
    let
	val () = Log.testInform Globals.TestStoreBuild Log.Debug
		    (fn() => TF.concat ["Installing resource ",
				   Cfg.listToString path]);

	type Pair = (string list * Cfg.NodeConfig)

	val table: Pair list STRT.hash_table = STRT.mkTable(101, NotFound)

	fun add (remainder, config) =
	(
	    case remainder of
	      [] => ()

	    | (h::t) => 
	    (
		case STRT.find table h of
		  NONE     => STRT.insert table (h, [(t, config)])
		| SOME lst => STRT.insert table (h, (t, config)::lst)
	    )
	)

	(*  If there is no config for this node then we fake a node
	    that always rejects an attempt to access it.

	    There may not be a config if this level is an intermediate segment
	    of a config path.  E.g. node /cgi/env {...}
	*)
	val self_config =
	    case Cfg.findNodeConfig path of
	      NONE   => U.builtinConfig path "reject"
	    | SOME c => c


	(*  Compute the option flags for this node.
	*)
	val self_options =
	    let
		val Cfg.NodeConfig {options = formula, ...} = self_config
	    in
		Node.performFormula options formula
	    end


	fun build (name, pairs) =
	let
	    val prefix = path @ [name]
	in
	    build_level prefix self_options pairs
	end

	val () = app add config_pairs;
	val items: (string * Pair list) list = STRT.listItemsi table
	val children = map build items

	val the_node =
		NodeFactory.create {
		    config    = self_config,
		    children  = children,
		    options   = self_options
		    }
    in
	getOpt(the_node, make_rejecter path)
    end



    and make_rejecter path =
    let
	val node =
	    NodeFactory.create {
		config    = U.builtinConfig path "reject",
		children  = [],
		options   = Node.initOptions false
		}
    in
	case node of
	  NONE => (	(* something wrong if we can't do this *)
	    Log.fatal ["Cannot create a rejecter node."];
	    Common.fail()
	    )

	| SOME n => n
    end

(*------------------------------------------------------------------------------*)

end
