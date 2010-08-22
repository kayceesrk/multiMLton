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

(* $Id: resp_utils.sml,v 1.11 2002/01/19 16:02:11 felix Exp $ *)

(*  

@#34567890123456789012345678901234567890123456789012345678901234567890
*)

signature RESP_UTILS =
sig
    (*	This creates a dummy configuration for a built-in
	handler that we want to insert at some node path.
    *)
    val builtinConfig:	Config.NodePath -> string -> Config.NodeConfig

    (*	This returns a response back to the client's connection. It
	marks the end of the resource store's involvement with the request.
    *)
    val return:		HTTPMsg.Request -> HTTPMsg.Response -> unit

    (*	These functions generate useful responses. 
    *)
    val mkNotFound:	unit -> HTTPMsg.Response
    val mkServerFail:	unit -> HTTPMsg.Response
    val mkForbidden:	unit -> HTTPMsg.Response

    val mkHelloWorld:	unit -> HTTPMsg.Response

    (*	This makes a response out of some piece of HTML.
    *)
    val mkHTML:	HTTPStatus.Status -> TextFrag.t -> HTTPMsg.Response
end


structure ResponseUtils: RESP_UTILS =
struct

    open Common

    structure TF     = TextFrag
    structure Req    = HTTPMsg
    structure Status = HTTPStatus
    structure Hdr    = HTTPHeader

(*------------------------------------------------------------------------------*)

    fun builtinConfig node_path name =
    (
	Config.NodeConfig {
	    path	= node_path,
	    kind	= Config.NodeIsBuiltin {name = name},
	    options	= [],
	    auth	= Config.NodeNoAuth
	    }
    )


    fun return req response =
    let
	val Req.Request {rvar, ...} = req
    in
	SyncVar.iPut(rvar, response)
    end


(*------------------------------------------------------------------------------*)

    and mkHelloWorld() : Req.Response =
    let
	val info = Info.Info { etype	   = SOME (MimeType.simpleType "text" "plain"),
			       encoding    = NONE,
			       length      = NONE,
			       last_mod    = NONE }

	val entity = Entity.Entity 
			 { info = info,
			   body = Entity.textProducer ( TF.seq [ TF.str "hello world", TF.nl ] ) }
    in
	Req.Response {
	status  = Status.OK,
	headers = [],
	entity  = entity
	}
    end

(*------------------------------------------------------------------------------*)

    and mkNotFound() =
    (
	mkHTML Status.NotFound 
	    (lines_to_text [
		"<html><body>", 
		"<em>Not Found</em>",
		"</body></html>"
		])
    )



    and mkServerFail() =
    (
	mkHTML Status.ServerFail 
	    (lines_to_text [
		"<html><body>", 
		"<em>Internal Server Error, Tough Luck</em>",
		"</body></html>"
		])
    )



    and mkForbidden() =
    (
	mkHTML Status.Forbidden 
	    (lines_to_text [
		"<html><body>", 
		"<em>Access Denied</em>",
		"</body></html>"
		])
    )


    and lines_to_text lst =
	TF.seq ( map ( fn l => TF.seq [ TF.str l, TF.nl ] ) 
			lst )


    and mkHTML status frag =
    let val info = Info.Info { etype       = SOME ( MimeType.simpleType "text" "html" ),
			       encoding    = NONE,
			       length      = NONE,
			       last_mod    = NONE }
		   
	val entity = Entity.Entity { info = info,
				     body = Entity.textProducer frag }
    in
	Req.Response { status  = status,
		       headers = [],
		       entity  = entity }
    end


(*------------------------------------------------------------------------------*)

end
