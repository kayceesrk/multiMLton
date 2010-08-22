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

(* This defines the messages between the http protocol manager
 * and the store.       
 * The differences between v1.0 and v1.1 are mainly in the headers. *)

signature HTTP_MSG =
sig

    datatype Method = GET | HEAD | POST

    datatype Request = Request of 
	     { method:	    Method,
	       url:	    URL.URL,
	       protocol:    string,	(* e.g. "HTTP/1.0" *)
	       headers:     HTTPHeader.Header list,
	       entity:	    Entity.entity,
	       
	       (*  Information from the connection. *)
	       port:	    int,
	       client:	    NetHostDB.in_addr,
	       
	       (*  The store sends its response here. *)
	       rvar:	    Response SyncVar.ivar,
	       
	       (*  This is the abort object from the connection. *)
	       abort:	    Abort.t }
	     

				   
	 and Response =
	     Response of 
	     { status:	    HTTPStatus.Status,
	       headers:     HTTPHeader.Header list,
	       entity:	    Entity.entity }
	     

    (*	For CGI-BIN use. *)
    val methodToString:	Method -> string


    (*	This dumps debugging information about the request to the log.
    *)
    val dumpRequest:	Request -> unit

end



structure HTTPMsg: HTTP_MSG =
struct

    structure TF = TextFrag
    structure Status = HTTPStatus
    structure Hdr = HTTPHeader

(*------------------------------------------------------------------------------*)

    datatype Request = Request of {
		method:	    Method,
		url:	    URL.URL,
		protocol:   string,	(* e.g. "HTTP/1.0" *)
		headers:    HTTPHeader.Header list,
		entity:	    Entity.entity,

		port:	    int,
		client:	    NetHostDB.in_addr,

		rvar:	    Response SyncVar.ivar,
		abort:	    Abort.t
		}

    and Method = GET | HEAD | POST

    and Response = 
	    Response of {
    		status:	    HTTPStatus.Status,
		headers:    HTTPHeader.Header list,
		entity:	    Entity.entity
		}

(*------------------------------------------------------------------------------*)


    fun methodToString GET  = "GET"
    |   methodToString HEAD = "HEAD"
    |   methodToString POST = "POST"


    fun dumpRequest (Request {method, url, protocol, headers, ...}) =
    let
	(*  We omit the body for now *)

	val tf_method = TF.seq [TF.str "Method: ", TF.str (methodToString method)]

	val tf_url = TF.concat [ "Url: ", URL.URLToString url ] 
	val tf_hdrs = TF.seqSep TF.nl ( List.map Hdr.formatHeader headers )
    in
	Log.inform Log.Debug ( fn () => TF.seq [ TF.str "Request is", TF.nl,
						 tf_method, TF.nl, 
						 tf_url, TF.nl,
						 TF.concat [ "Protocol: ", protocol ], TF.nl,
						 tf_hdrs, TF.nl ] )
    end

(*------------------------------------------------------------------------------*)

end
