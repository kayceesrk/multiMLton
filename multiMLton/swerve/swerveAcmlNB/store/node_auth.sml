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

(* $Id: node_auth.sml,v 1.8 2001/09/23 20:18:34 felix Exp $ *)

(*  This contains the common machinery for the different kinds of nodes.

@#34567890123456789012345678901234567890123456789012345678901234567890
*)

signature NODE_AUTH =
sig

    (*	This checks that the request satisifies the authorisation
	If OK then it returns.  If there is a failure then it raises
	Node.Respond.
    *)
    val checkAuth:  Config.NodeAuth -> HTTPMsg.Request -> unit

end


structure NodeAuth: NODE_AUTH =
struct

    structure TF     = TextFrag
    structure Cfg    = Config
    structure Req    = HTTPMsg
    structure Status = HTTPStatus
    structure Hdr    = HTTPHeader
    structure SS     = Substring
    structure G      = Globals

    open Common

(*------------------------------------------------------------------------------*)

    (*	Check that the request satisfies the authorisation.
	If not, then raise a HTTP response to either request
	more authorisation or fail.
    *)
    fun checkAuth auth (req: Req.Request) =
    let
    in
	case auth of
	  Cfg.NodeNoAuth    => ()	(* pass *)
	| Cfg.NodeBasic au  => validate_basic au req
    end



    (*	The node's configuration requires some authorisation.
	Check that the request satisfies it.  We require a user id.
    *)
    and validate_basic
	    (auth as {realm, user_file, group_file, users, groups})
	    req
	    : unit =
    let
	val () = Log.testInform G.TestAuth Log.Debug (fn() => TF.concat [
		    "Basic auth for realm ", realm])

	val Req.Request {headers, abort, ...} = req


	(*  Generate a challenge response to prompt for a password.  *)
	fun challenge() =
	let
	    val () = Log.testInform G.TestAuth Log.Debug (fn() => TF.concat [
			"Returning challenge for realm ", realm])

	    val resp = Req.Response {
		    status  = Status.UnAuth,
		    headers = [Hdr.HdrChallenge(Hdr.ChallBasic realm)],
		    entity  = Entity.None
		    }
	in
	    raise Node.Respond resp
	end


	fun reject() =
	let
	    val resp = Req.Response {
		    status  = Status.UnAuth,
		    headers = [],
		    entity  = Entity.None
		    }
	in
	    raise Node.Respond resp
	end

    in
	case Hdr.getAuth headers of
	  NONE => challenge()

	| SOME (Hdr.AuthBasic (opt_id, pwd)) =>
	(
	    case opt_id of
	      NONE    => reject()
	    | SOME id => validate_user abort auth reject id pwd
	)
    end



    and validate_user 
	    abort
	    {realm, user_file, group_file, users, groups}
	    rejecter id pwd 
	    : unit =
    let
	val () = Log.testInform G.TestAuth Log.Debug (fn() => TF.concat [
		    "Validate user=", id, " for realm=", realm])

	val all_users = add_group_users abort users group_file groups
    in
	if List.exists (isVal id) all_users
	   andalso validate_pwd abort user_file id pwd
	then
	    ()
	else
	    rejecter()
    end



    (*	Add the users in the groups to the users list.
    *)
    and add_group_users abort users group_file groups =
    let
	fun loop lnum users_in strm =
	let
	    val line = TextIO.inputLine strm
	in
	    case line of
		NONE => users_in		(* eof *)
	      | SOME line => loop (lnum+1) (split_line line lnum users_in) strm
	end
	handle x => (Log.logExn x; TextIO.closeIn strm; users)


	and split_line line lnum users_in =
	let
	    val tokens = String.tokens Char.isSpace line
	in
	    (*	Allow white space before the colon. *)
	    case tokens of
	      (grp :: ":" :: rest) => check_group grp rest users_in

	    | (grp :: rest) =>
	    (
		if String.sub(grp, size grp - 1) = #":"
		then
		    check_group
			(String.substring(grp, 0, size grp - 1))
			rest users_in
		else
		(
		    bad_line lnum;
		    users_in
		)
	    )

	    | _ => (bad_line lnum; users_in)
	end


	and bad_line lnum =
	(
	    Log.error [group_file, ", at line ", Int.toString lnum,
	    	       "is invalid"]
	)


	and check_group grp members users_in =
	(
	    if List.exists (isVal grp) groups
	    then
		flush members users_in
	    else
		users_in
	)

	and flush []     r = r
	|   flush (h::t) r = flush t (h::r)

    in
	FileIO.withTextIn abort group_file users (loop 1 users)
    end


    (*	Check the password against the user file entry.
	We don't allow any white space around the colon here.
    *)

    and validate_pwd abort user_file id pwd : bool =
    let	val () = Log.testInform G.TestAuth 
				Log.Debug 
				( fn() => TF.concat [ "Validate pwd for user=", 
						      id, " pwd=", pwd ] )
		 
	fun loop lnum strm =
	    ( case TextIO.inputLine strm of
		  NONE => false		(* eof so failed *)
		| SOME line => if check_line line lnum
			       then true
			       else loop (lnum+1) strm )
	
	and check_line line lnum =
	    let	val (left, right) =
		    SS.splitl (isntVal #":") (SS.full line)
		    
		fun clean s = SS.dropr Char.isSpace (SS.dropl Char.isSpace s)
			      
		(*	Trim off leading and trailing white space from the names. *)
		val user     = SS.string(clean left)
		val password = SS.string(clean(SS.triml 1 right))
			       
		val () = Log.testInform G.TestAuth 
					Log.Debug 
					( fn () => TF.concat [ "Found user=", 
							       user, " pwd=", password ] )
	    in
		user = id andalso password = pwd
	    end
    in
	FileIO.withTextIn abort user_file false (loop 1)
    end

(*------------------------------------------------------------------------------*)


end

