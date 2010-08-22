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

signature HTTP_HEADER =
sig

    datatype AuthChallenge =
	     ChallBasic of string    (* the realm *)

    datatype Authorization =
	     AuthBasic of (string option * string)  (* user id and password *)
    
    (*	Well known headers are separated out. *)
    datatype Header = 
	     HdrDate of Date.date
	   | HdrPragma of string
			  
	   | HdrAuthorization of Authorization
	   | HdrFrom of string
	   | HdrIfModified of Date.date
	   | HdrReferer of string
	   | HdrUserAgent of string
			     
	   | HdrConEnc of Encoding.t    (* content encoding *)
	   | HdrConLen of int		(* content length *)
	   | HdrConType of MimeType.t   (* mime type *)
	   | HdrLastModified of Date.date
				
	   | HdrChallenge of AuthChallenge
			     
	   (*  These can appear in CGI script output. *)
	   | HdrStatus of HTTPStatus.Status
	   | HdrLocation of URL.URL
			    
	   | HdrExt of (string * string)	(* extensions *)
	   | HdrBad of string		(* unparsable junk *)
		       
		       
    val readAllHeaders:	(unit -> string option) -> Header list

    val parseHeader:	string -> Header

    val formatHeader: Header -> TextFrag.t

    (*	These functions retrieve well-known headers. *)

    val getContentLength:   Header list -> int option
    val getContentType:     Header list -> MimeType.t option
    val getContentEnc:      Header list -> Encoding.t option
    val getContentMod:      Header list -> Date.date option
    val getAuth:	    Header list -> Authorization option
    val getStatus:	    Header list -> HTTPStatus.Status option
    val getLocation:	    Header list -> URL.URL option

    (*	This extracts the relevant headers to build the entity info
	record.
    *)
    val toEntityInfo:	    Header list -> Info.t

    (*	This overrides one set of headers with another. *)
    val overrideHeaders:    Header list -> Header list -> Header list

    (*	This excludes a set of headers.  The excluded set is
	demonstrated by sample headers in the first list.
    *)
    val excludeHeaders:	    Header list -> Header list -> Header list

end


structure HTTPHeader: HTTP_HEADER =
struct

    open Common
    structure SS = Substring

    structure IETF = IETFLine
    structure IP = IETFPart
    structure TF = TextFrag

(*------------------------------------------------------------------------------*)

    (*	Well known headers are separated out. 
    *)

    datatype Header = 
	    HdrDate of Date.date
	|   HdrPragma of string

	|   HdrAuthorization of Authorization
	|   HdrFrom of string
	|   HdrIfModified of Date.date
	|   HdrReferer of string
	|   HdrUserAgent of string

	|   HdrConEnc of Encoding.t     (* content encoding *)
	|   HdrConLen of int		(* content length *)
	|   HdrConType of MimeType.t    (* mime type *)
	|   HdrLastModified of Date.date

	|   HdrChallenge of AuthChallenge

	(*  These can appear in CGI script output. *)
	|   HdrStatus of HTTPStatus.Status
	|   HdrLocation of URL.URL

	|   HdrExt of (string * string)	(* extensions *)
	|   HdrBad of string	    (* unparsable junk *)


    and Authorization =
	    AuthBasic of (string option * string)  (* user id and password *)

    and AuthChallenge =
	    ChallBasic of string    (* the realm *)

    exception Bad
    
(*------------------------------------------------------------------------------*)

    (*  Read all lines up to the blank one that terminates 
	the headers.  Note that readLine trims off the trailing
	\r\n so the resulting lines are guaranteed to be of non-zero
	length unless they are blank.
    *)

    fun readAllHeaders readLine : Header list =
    let fun loop lines : string list =
	    case readLine() of
		NONE      => rev lines
	      | SOME line =>
		(
		 Log.testInform Globals.TestShowRequest Log.Debug
				( fn()=> TF.concat [ "'", line, "'" ] );
		 
		 if size line > 0
		 then
		     loop (line::lines)
		 else 
		     lines
		)	

	(*  Merge continuations. 
	    parts accumulates the parts of a header.
	    The original leading white space of continuation lines
	    is elided.
	*)
	fun merge [] (parts: string list) rslt = rev(new_header parts rslt)

	|   merge (line::rest) parts rslt =
	let
	    val (space, sbody) = SS.splitl Char.isSpace (SS.full line)
	    val body = SS.string sbody
	in
	    if SS.isEmpty space
	    then
		merge rest [body] (new_header parts rslt)
	    else
		merge rest (body::parts) rslt
	end


	and new_header [] rslt = rslt
	|   new_header parts rslt = ( concat ( rev parts ) ) :: rslt


	fun show_lines lines =	
	    Log.inform Log.Debug ( fn () => TF.seqSep TF.nl ( List.map TF.str lines ))

	val lines = loop []
	val hdr_lines = merge lines [] []
	val headers = map parseHeader hdr_lines
    in
	(* show_lines hdr_lines; *)
	headers
    end

    (* The lexer isn't used until the header name has been recognised.
     * Some headers don't want the value split into tokens etc. *)
    and parseHeader line : Header =
    let	val dispatch = 
	    [ ("DATE",		    parse_date HdrDate),
	      ("PRAGMA",	    parse_pragma),
	      ("AUTHORIZATION",	    parse_auth),
	      ("FROM",		    parse_from),
	      ("IF-MODIFIED-SINCE", parse_if_modified),
	      ("REFERER",	    parse_referer),
	      ("USER-AGENT",	    parse_useragent),
	      ("CONTENT-ENCODING",  parse_cont_encoding),
	      ("CONTENT-LENGTH",    parse_cont_length),
	      ("CONTENT-TYPE",      parse_cont_type),
	      ("LAST-MODIFIED",     parse_date HdrLastModified),
	      ("WWW-AUTHENTICATE",  parse_challenge),
	      ("LOCATION",    	    parse_location),
	      ("STATUS",    	    parse_status) ]
	    
	(*  The value has the leading and trailing white space removed. *)
	fun parse sstoken ssvalue =
	let
	    val value = (SS.string(SS.dropl Char.isSpace
	    				(SS.dropr Char.isSpace ssvalue)))
	    val token  = SS.string sstoken
	    val utoken = upperCase token
	in
	    case List.find (fn (n, _) => n = utoken) dispatch of
	      NONE => HdrExt (token, value)

	    | SOME (n, f) => f value
	end


	(*  The common characters are caught early for speed. *)
	fun is_token c = Char.isAlphaNum c orelse c = #"-" orelse
			 Char.contains "!#$%&'*+.^_`|~" c orelse
			 (ord c >= 128)


	val (name, rest) = SS.splitl is_token (SS.full line)
    in
	(*  Expect a token, colon and more parts. *)
	if not (SS.isEmpty name) andalso SS.sub(rest, 0) = #":"
	then
	    parse name (SS.triml 1 rest)
	else
	    HdrBad line
    end


    (*	The only recognised pragma is no-cache and it is deprecated in
	HTTP1.1.  We just reconstitute it as a string.
    *)
    and parse_pragma value =
    let
	val hparts = IETF.split value
    in
	case strip_ws hparts of
	  [IP.Token s] =>
	(
	    if field_match s "no-cache"
	    then
		HdrPragma "no-cache"
	    else
		HdrBad value
	)

	| _ => HdrBad value
    end


    (*	We don't interpret e-mail addresses, etc. *)

    and parse_from value = HdrFrom value

    and parse_useragent value = HdrUserAgent value


    (*	According to the RFC this just takes a date. But Netscape
     * sometimes sends a "; length=nnn" suffix which I simply ignore.
     * Presumably the server is to check if the length has changed as
     * well. *)
    and parse_if_modified value =
    let
	(*  drop from the semicolon onwards *)
	fun drop [] rslt = rev rslt
	|   drop ((IP.Spec #";")::rest) rslt = rev rslt
	|   drop (h::rest) rslt = drop rest (h::rslt)

	val hparts = IETF.split value
	val date = IETF.join(drop hparts [])
    in
	parse_date HdrIfModified date
    end


    (*	We are supposed to validate the URL.  It must not
	have a fragment.
    *)
    and parse_referer url =
    let
    in
	(case URL.parseURL url of
	  URL.HTTP_URL {fragment = SOME _, ...} => HdrBad url
	| _                                     => HdrReferer url)
	handle URL.BadURL _ => HdrBad url
    end



    and parse_cont_encoding value =
    let
	val hparts = IETF.split value
    in
	case strip_ws hparts of
	  [IP.Token s] => check_content_encoding s
	| _            => HdrBad value
    end



    and parse_cont_length value =
    let
	val hparts = IETF.split value
    in
	case strip_ws hparts of
	  [IP.Token s] =>
	(
	    case Int.fromString s of
	      NONE   => HdrBad value
	    | SOME n => HdrConLen n
	)

	| _ => HdrBad value
    end



    (*	This doesn't have a lot of validity checking for the numeric values.
    *)
    and parse_date (constr: Date.date -> Header) value =
    let
	val hparts = IETF.split value

	fun build wkday day month year hh mm ss =
	let
	    val days = ["MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN",
			"MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", 
			"FRIDAY", "SATURDAY", "SUNDAY"]

	    val months = [
		("JAN",		Date.Jan),
		("FEB",		Date.Feb),
		("MAR",		Date.Mar),
		("APR",		Date.Apr),
		("MAY",		Date.May),
		("JUN",		Date.Jun),
		("JUL",		Date.Jul),
		("AUG",		Date.Aug),
		("SEP",		Date.Sep),
		("OCT",		Date.Oct),
		("NOV",		Date.Nov),
		("DEC",		Date.Dec)
		]

	    fun find_month() =
	    let
		val ucase = upperCase month
	    in
		case List.find (fn(n,_)=>(n = ucase)) months of
		  NONE => raise Bad
		| SOME (_,m) => m
	    end

	    (*	Check the week day but ignore it. *)
	    val () = 
		let
		    val ucase = upperCase wkday
		in
		    if List.exists (fn n => (n = ucase)) days
		    then
			()
		    else
			raise Bad
		end

	    val date = Date.date {
		    year   = valOf(Int.fromString year),
		    month  = find_month(),
		    day    = valOf(Int.fromString day),
		    hour   = valOf(Int.fromString hh),
		    minute = valOf(Int.fromString mm),
		    second = valOf(Int.fromString ss),
		    offset = SOME Time.zeroTime	    (* for GMT *)
		    }

	in
	    constr date
	end
	handle _ => HdrBad value
    in
	(* print "looking at the date parts ";
	   IETF.dump parts; print "\n"; *)

	case strip_ws hparts of
	  [IP.Token wkday,
	   IP.Spec #",",
	   IP.Token day,
	   IP.Token month,
	   IP.Token year,
	   IP.Token hh,
	   IP.Spec #":",
	   IP.Token mm,
	   IP.Spec #":",
	   IP.Token ss,
	   IP.Token "GMT"] => build wkday day month year hh mm ss

	| [IP.Token wkday,
	   IP.Spec #",",
	   IP.Token dmy,	(* hyphen isn't special *)
	   IP.Token hh,
	   IP.Spec #":",
	   IP.Token mm,
	   IP.Spec #":",
	   IP.Token ss,
	   IP.Token "GMT"] =>
	    (
		case String.fields (fn c => c = #"-") dmy of
		  [day, month, year] =>
		      build wkday day month ("19"^year) hh mm ss

		| _ => HdrBad value
	    )

	| [IP.Token wkday,
	   IP.Token month,
	   IP.Token day,
	   IP.Token hh,
	   IP.Spec #":",
	   IP.Token mm,
	   IP.Spec #":",
	   IP.Token ss,
	   IP.Token year] => build wkday day month year hh mm ss

	| _ => HdrBad value
    end



    and parse_auth value =
    let
	val hparts = IETF.split value

	fun basic_cred creds =
	let
	    (* Resplit the credentials on white space. *)
	    val joined = concat(map to_str creds)
	in
	    case String.tokens Char.isSpace joined of
		( cred :: _ ) => ( case Base64.decode cred of
				       NONE => HdrBad value
				     | SOME s => split s ) 
	      | _ => HdrBad value
	end


	and to_str (IP.Token s1) = s1
	|   to_str (IP.WS s)    = s
	|   to_str (IP.Spec c)	 = str c
	|   to_str _	         = ""


	and split str =
	(
	    case String.fields (fn c => c = #":") str of
	      [pwd]     => HdrAuthorization(AuthBasic (NONE, pwd))
	    | [id, pwd] => HdrAuthorization(AuthBasic (SOME id, pwd))
	    | _         => HdrBad value
	)
    in
	case strip_ws hparts of
	  (IP.Token kind)::rest =>
	(
	    if field_match kind "basic"
	    then
		basic_cred rest
	    else
		HdrBad value
	)

	| _ => HdrBad value
    end



    (*	We only allow basic authentication. The challenge must be a
	single string.
    *)
    and parse_challenge value =
    let
	val hparts = IETF.split value
    in
	case strip_ws hparts of
	  [IP.Token s] => HdrChallenge(ChallBasic s)
	| _            => HdrBad value
    end



    and parse_location url =
    let
	val url = URL.parseURL url
    in
	HdrLocation url
    end
    handle _ => HdrBad url



    and parse_status value =
    let
	val hparts = IETF.split value
    in
	case hparts of 
	  (IP.Token s)::rest =>
	      (HdrStatus(HTTPStatus.fromInt(valOf(Int.fromString s)))
		    handle _ => HdrBad value)
	| _ => HdrBad value
    end


(*------------------------------------------------------------------------------*)

    (*	It is easy to include the v1.1 encodings for future use. *)
    and check_content_encoding s : Header = 
	let
	    val ucase = upperCase s
			
	    val table = [ ("X-GZIP",      Encoding.GZip),
			  ("GZIP",        Encoding.GZip),
			  ("X-COMPRESS",  Encoding.Compress),
			  ("COMPRESS",    Encoding.Compress),
			  ("DEFLATE",     Encoding.Deflate),
			  ("IDENTITY",    Encoding.Identity) ]
	in
	    case List.find (fn (n,_) => n = ucase) table of
		NONE       => HdrConEnc (Encoding.Other s)
	      | SOME (_,e) => HdrConEnc e 
	end
	
	
	
    and parse_cont_type value : Header = 
	let
	    val hparts = IETF.split value
			 
	    fun check_params [] ( rslt: ( string * string ) list ) = rev rslt
	      |   check_params ( ( IP.Spec #";" ) ::
				 ( IP.Token n ) ::
				 ( IP.Spec #"=" ) ::
				 ( IP.Token v ) ::
				 rest ) rslt =
		  ( check_params rest ( ( n, v ) :: rslt ) )
	      |   check_params _ rslt = raise Bad
					      
	in
	    case strip_ws hparts of
		( ( IP.Token t) :: (IP.Spec #"/") :: (IP.Token st) :: rest ) =>
		( let val mparams = check_params rest []
		  in
		      HdrConType ( MimeType.mkMimeType t st mparams )
		  end
		  handle Bad => HdrBad value )		
	      | _ => HdrBad value
	end

(*------------------------------------------------------------------------------*)

    (*	Strip out the white space parts for interpretation. *)
    and strip_ws hparts =
	let fun f (IP.WS _) = false
	      |   f  _         = true
	in
	    List.filter f hparts
	end
			
    and field_match s1 s2 = (upperCase s1) = (upperCase s2)

(*------------------------------------------------------------------------------*)


    (*	Present the header for the response.
	This will omit the \r\n at the end to suit some callers.

	A server doesn't generate Authorization headers so they are omitted.
	Omitted headers raise Bad.

	The CGI module will also use this but edit the header name. So this
	format must be standard.

    *)


    and formatHeader (HdrDate date) =
    (
	format_date "Date: " date
    )

    |   formatHeader (HdrPragma pragma) =
    (
	TF.concat ["Pragma: ", IETF.quoteField pragma]
    )

    |   formatHeader (HdrAuthorization auth) =
    (
	case auth of
	  AuthBasic (id_opt, pwd) =>
	    let	val s = concat[getOpt(id_opt, ""), ":", pwd]
	    in
		TF.concat ["Authorization: ", Base64.encode s]
	    end
    )

    |   formatHeader (HdrFrom from) =
    (
	TF.concat ["From: ", IETF.quoteField from]
    )

    |   formatHeader (HdrIfModified date) =
    (
	format_date "If-Modified-Since: " date
    )

    |   formatHeader (HdrReferer referer) =
    (
	TF.concat ["Referer: ", IETF.quoteField referer]
    )

    |   formatHeader (HdrUserAgent agent) =
    (
	TF.concat ["User-Agent: ", IETF.quoteField agent]
    )

    |   formatHeader (HdrConEnc enc) =
    (
	case enc of
	  Encoding.None => TF.empty
	| _ => TF.seq [TF.str "Content-Encoding: ", Encoding.format enc]
    )

    |   formatHeader ( HdrConLen len ) =
    (
	TF.concat ["Content-Length: ", Int.toString len]
    )

    |   formatHeader (HdrConType typ) = 
	if MimeType.isUnknown typ 
	then TF.empty
	else TF.seq [ TF.str "Content-Type: ", MimeType.format typ ]

    |   formatHeader (HdrLastModified date) =
    (
	format_date "Last-Modified: " date
    )

    |   formatHeader (HdrChallenge chal) =
    (
	case chal of
	  ChallBasic realm =>
	    TF.concat ["WWW-Authenticate: Basic realm=", IETF.quoteField realm]
    )

    |   formatHeader (HdrStatus status) =
    (
	TF.concat ["Status: ", HTTPStatus.formatStatus status]
    )

    |   formatHeader (HdrLocation url) =
    (
	TF.concat ["Location: ", URL.URLToString url]
    )

    |   formatHeader (HdrExt (tok, ext)) =
    (
	TF.concat [tok, ": ", IETF.quoteField ext]
    )

    |   formatHeader _ = TF.str "X-Bad-Header: bad"



    (*	Produce something like Sun, 06 Nov 1994 08:49:37 GMT
	All dates must be in GMT.
    *)
    and format_date prefix date =
    let
	fun wkday Date.Mon = "Mon"
	|   wkday Date.Tue = "Tue"
	|   wkday Date.Wed = "Wed"
	|   wkday Date.Thu = "Thu"
	|   wkday Date.Fri = "Fri"
	|   wkday Date.Sat = "Sat"
	|   wkday Date.Sun = "Sun"

	fun mon Date.Jan = "Jan"
	|   mon Date.Feb = "Feb"
	|   mon Date.Mar = "Mar"
	|   mon Date.Apr = "Apr"
	|   mon Date.May = "May"
	|   mon Date.Jun = "Jun"
	|   mon Date.Jul = "Jul"
	|   mon Date.Aug = "Aug"
	|   mon Date.Sep = "Sep"
	|   mon Date.Oct = "Oct"
	|   mon Date.Nov = "Nov"
	|   mon Date.Dec = "Dec"

	fun int2 n = (if n < 10 then "0" else "") ^ (Int.toString n)
    in
	TF.concat [ prefix, wkday(Date.weekDay date), ", ",
		    int2(Date.day date), " ",
		    mon(Date.month date), " ",
		    Int.toString(Date.year date), " ",
		    int2(Date.hour date), ":",
		    int2(Date.minute date), ":",
		    int2(Date.second date), " GMT" ]
    end



(*------------------------------------------------------------------------------*)

    fun getContentLength []                 = NONE
    |   getContentLength ((HdrConLen n)::_) = SOME n
    |   getContentLength (_::rest)          = getContentLength rest

    fun getContentType []                  = NONE
    |   getContentType ((HdrConType x)::_) = SOME x
    |   getContentType (_::rest)           = getContentType rest

    fun getContentEnc []                 = NONE
    |   getContentEnc ((HdrConEnc x)::_) = SOME x
    |   getContentEnc (_::rest)          = getContentEnc rest

    fun getContentMod []                       = NONE
    |   getContentMod ((HdrLastModified x)::_) = SOME x
    |   getContentMod (_::rest)                = getContentMod rest

    fun getAuth []                        = NONE
    |   getAuth ((HdrAuthorization x)::_) = SOME x
    |   getAuth (_::rest)                 = getAuth rest

    fun getStatus []                 = NONE
    |   getStatus ((HdrStatus x)::_) = SOME x
    |   getStatus (_::rest)          = getStatus rest

    fun getLocation []                   = NONE
    |   getLocation ((HdrLocation x)::_) = SOME x
    |   getLocation (_::rest)            = getLocation rest


    (*	This builds an Info record from the headers. *)
    fun toEntityInfo headers  =
	let val len = case getContentLength headers of
			  NONE => NONE
			| SOME l => SOME (Int64.fromInt l)
	in
	    Info.Info { etype       = getContentType headers,
			encoding    = getContentEnc headers,
			length      = len,
			last_mod    = getContentMod headers }
	end

    (*	override 1 with 2 *)
    fun overrideHeaders hdrs1 hdrs2 =
    let
	(*  remove from hdrs1 all those found in hdrs2 *)
	val pruned = excludeHeaders hdrs2 hdrs1
    in
	pruned @ hdrs2
    end



    (*  Remove from hdrs2 all those found in hdrs1 *)
    and excludeHeaders hdrs1 hdrs2 =
    let
	fun keep h = not(List.exists (same_header h) hdrs1)
    in
	List.filter keep hdrs2
    end


    and same_header (HdrDate _)         (HdrDate _)		= true
    |   same_header (HdrPragma _)       (HdrPragma _)		= true
    |   same_header (HdrAuthorization _)(HdrAuthorization _)	= true
    |   same_header (HdrFrom _)         (HdrFrom _)		= true
    |   same_header (HdrIfModified _)   (HdrIfModified _)	= true
    |   same_header (HdrReferer _)      (HdrReferer _)		= true
    |   same_header (HdrUserAgent _)    (HdrUserAgent _)	= true
    |   same_header (HdrConEnc _)       (HdrConEnc _)		= true
    |   same_header (HdrConLen _)       (HdrConLen _)		= true
    |   same_header (HdrConType _)      (HdrConType _)		= true
    |   same_header (HdrLastModified _) (HdrLastModified _)	= true
    |   same_header (HdrChallenge _)    (HdrChallenge _)	= true
    |   same_header (HdrStatus _)       (HdrStatus _)		= true
    |   same_header (HdrLocation _)     (HdrLocation _)		= true
    |   same_header (HdrExt (s1, _))	(HdrExt (s2, _)) = (upperCase s1 = upperCase s2)
    |   same_header _ _ = false

(*------------------------------------------------------------------------------*)

end
