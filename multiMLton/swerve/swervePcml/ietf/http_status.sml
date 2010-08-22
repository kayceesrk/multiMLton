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

(* $Id: http_status.sml,v 1.5 2001/08/28 19:49:29 felix Exp $ *)

(*  This implements the request status for v1.0 and v1.1.
    The v1.1 status codes are included so that we can reject requests
    that require v1.1.

    Not all codes are included.

@#345678901234567890123456789012345678901234567890123456789012345
*)

signature HTTP_STATUS =
sig

    datatype Severity =
	Info | Success | Redirect | ClientError | ServerError

    type Status

    val	OK:		Status		(* 200 *)
    val	Created:	Status		(* 201 *)
    val	Accepted:	Status		(* 202 *)
    val	NoContent:	Status		(* 204 *)

    val	MovedPerm:	Status		(* 301 *)
    val	MovedTemp:	Status		(* 302 *)
    val	NotModified:	Status		(* 304 *)

    val	BadRequest:	Status		(* 400 *)
    val	UnAuth:		Status		(* 401 *)
    val	Forbidden:	Status		(* 403 *)
    val	NotFound:	Status		(* 404 *)
    val	BadMethod:	Status		(* 405 v1.1 *)
    val	NotAccept:	Status		(* 406 v1.1 *)
    val	ReqTimeout:	Status		(* 408 v1.1 *)
    val	ReqTooLarge:	Status		(* 413 v1.1 *)
    val	UnrecMedia:	Status		(* 415 v1.1 *)

    val	ServerFail:	Status		(* 500 *)
    val	NotImpl:	Status		(* 501 *)
    val	ServUnavail:	Status		(* 503 *)
    val	UnrecVersion:	Status		(* 505 v1.1 *)

    (*	Format for sending to the client. *)
    val formatStatus:	Status -> string

    val severity:	Status -> Severity
    val code:		Status -> int

    val isV11:		Status -> bool
    val same:		Status -> Status -> bool

    val fromInt:	int -> Status

    (*	This tests if the response needs a body according to the
	status code.  See section 7.2 of RFC1945.
    *)
    val	needsBody:	Status -> bool

end


structure HTTPStatus: HTTP_STATUS =
struct

(*------------------------------------------------------------------------------*)

    datatype Severity =
	Info | Success | Redirect | ClientError | ServerError

    datatype Status = Status of {
    	sev:	Severity,
	ver:    Version,
	code:	int			(* full 3-digit code *)
	}

    and Version = V10 | V11

(*------------------------------------------------------------------------------*)

    val	OK	    = Status {sev = Success, ver=V10, code = 200}
    val	Created	    = Status {sev = Success, ver=V10, code = 201}
    val	Accepted    = Status {sev = Success, ver=V10, code = 202}
    val	NoContent   = Status {sev = Success, ver=V10, code = 204}

    val	MovedPerm   = Status {sev = Redirect, ver=V10, code = 301}
    val	MovedTemp   = Status {sev = Redirect, ver=V10, code = 302}
    val	NotModified = Status {sev = Redirect, ver=V10, code = 304}
 
    val	BadRequest  = Status {sev = ClientError, ver=V10, code = 400}
    val	UnAuth      = Status {sev = ClientError, ver=V10, code = 401}
    val	Forbidden   = Status {sev = ClientError, ver=V10, code = 403}
    val	NotFound    = Status {sev = ClientError, ver=V10, code = 404}
    val	BadMethod   = Status {sev = ClientError, ver=V11, code = 405}
    val	NotAccept   = Status {sev = ClientError, ver=V11, code = 406}
    val	ReqTimeout  = Status {sev = ClientError, ver=V11, code = 408}
    val	ReqTooLarge = Status {sev = ClientError, ver=V11, code = 413}
    val	UnrecMedia  = Status {sev = ClientError, ver=V11, code = 415}

    val	ServerFail  = Status {sev = ServerError, ver=V10, code = 500}
    val	NotImpl     = Status {sev = ServerError, ver=V10, code = 501}
    val	ServUnavail = Status {sev = ServerError, ver=V10, code = 503}
    val	UnrecVersion= Status {sev = ServerError, ver=V11, code = 505}

    val all_status = [
	    OK,
	    Created,
	    Accepted,
	    NoContent,
	    MovedPerm,
	    MovedTemp,
	    NotModified,
	    BadRequest,
	    UnAuth,
	    Forbidden,
	    NotFound,
	    BadMethod,
	    NotAccept,
	    ReqTimeout,
	    ReqTooLarge,
	    UnrecMedia,
	    ServerFail,
	    NotImpl,
	    ServUnavail,
	    UnrecVersion
	    ]

    fun formatStatus (Status {code, ...}) =
    (
	concat[Int.toString code, " ", description code]
    )


    and description 200		= "OK"
    |   description 201		= "Created"
    |   description 202		= "Accepted"
    |   description 204		= "No Content"

    |   description 301		= "Moved Permanently"
    |   description 302		= "Moved Temporarily"
    |   description 304		= "Not Modified"

    |   description 400		= "Bad Request"
    |   description 401		= "Unauthorized"
    |   description 403		= "Forbidden"
    |   description 404		= "Not Found"
    |   description 405		= "Method Not Allowed"
    |   description 406		= "Not Acceptable"
    |   description 408		= "Request Timeout"
    |   description 413		= "Request Entity Too Large"
    |   description 415		= "Unsupported Media Type"

    |   description 500		= "Internal Server Error"
    |   description 501		= "Not Implemented"
    |   description 503		= "Service Unavailable"
    |   description 505		= "HTTP Version Not Supported"

    |   description _		= ""


    and severity (Status {sev, ...})  = sev
    and code     (Status {code, ...}) = code
    and isV11    (Status {ver, ...})  = (ver = V11)

    and same (Status {code = a, ...}) (Status {code = b, ...}) = (a = b)


    and fromInt n =
    let
	fun match (Status {code, ...}) = code = n
    in
	case List.find match all_status of
	  NONE => Status {code = n, sev = ServerError, ver = V11}  (* guess *)
	| SOME s => s
    end



    fun	needsBody (Status {code, ...}) =
    (
	code < 200 orelse code = 204 orelse code = 304
    )

(*------------------------------------------------------------------------------*)

end
