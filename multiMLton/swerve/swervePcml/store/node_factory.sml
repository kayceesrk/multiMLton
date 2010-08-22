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

(* $Id: node_factory.sml,v 1.11 2001/09/20 20:27:01 felix Exp $ *)

(* This creates store nodes of different kinds.			       
 * The possible kinds are:
 * directory	- backed by a disk directory
 * built-in	- delegate to an internal handler
 * cgi-bin		- delegate to a CGI script *)

signature NODE_FACTORY =
sig

    (*	Create a new node.  The kind is obtained from the configuration. *)
    val create: Node.NodeCreator
		
end



structure NodeFactory: NODE_FACTORY =
struct
  
  structure TF  = TextFrag
  structure Cfg = Config
		  
  structure DirNode = GenericNodeFn( structure Handler = DirNodeHandler )
		      
  structure CgiNode = GenericNodeFn ( structure Handler = CgiNodeHandler )
		      
  structure SimpleBuiltinNode = GenericNodeFn ( structure Handler = SimpleBuiltinHandler )


  fun create {config, children, options} =
      let val Cfg.NodeConfig {path, kind, ...} = config
						
	  val () = Log.testInform Globals.TestStoreBuild Log.Debug
				  (fn() => TF.concat ["Creating node ", Cfg.listToString path]);
	      
	  (*  This name is used for locating children from path segments.
	   * See GenericNodeFn.forward_child. *)
	  val node_name = if null path then "/" else List.last path
      in
	  case kind of
	      Cfg.NodeIsDir {path} =>
	      DirNode.create {
	      name     = node_name,
	      arg      = path,
	      config   = config,
	      options  = options,
	      factory  = create,
	      children = children
	      }
	      
	    | Cfg.NodeIsBuiltin {name} =>
	      SimpleBuiltinNode.create {
	      name     = node_name,
	      arg      = name,
	      config   = config,
	      options  = options,
	      factory  = create,
	      children = children
	      }
	      
	    | Cfg.NodeIsScript {path} =>
	      CgiNode.create {
	      name     = node_name,
	      arg      = path,
	      config   = config,
	      options  = options,
	      factory  = create,
	      children = children
	      }
      end
      
end
