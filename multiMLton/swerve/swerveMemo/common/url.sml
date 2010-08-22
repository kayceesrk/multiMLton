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

(* $Id: url.sml,v 1.13 2001/09/12 20:06:45 felix Exp $ *)

(*  This implements URL operations.

    The syntax for URLs is taken from RFC2396:

    URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
    absoluteURI   = scheme ":" ( hier_part | opaque_part )
    relativeURI   = ( net_path | abs_path | rel_path ) [ "?" query ]

    hier_part     = ( net_path | abs_path ) [ "?" query ]
    opaque_part   = uric_no_slash *uric

    uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@" |
		  "&" | "=" | "+" | "$" | ","

    net_path      = "//" authority [ abs_path ]
    abs_path      = "/"  path_segments
    rel_path      = rel_segment [ abs_path ]

    rel_segment   = 1*( unreserved | escaped |
		      ";" | "@" | "&" | "=" | "+" | "$" | "," )

    scheme        = alpha *( alpha | digit | "+" | "-" | "." )

    authority     = server | reg_name

    reg_name      = 1*( unreserved | escaped | "$" | "," |
		      ";" | ":" | "@" | "&" | "=" | "+" )

    server        = [ [ userinfo "@" ] hostport ]
    userinfo      = *( unreserved | escaped |
		     ";" | ":" | "&" | "=" | "+" | "$" | "," )

    hostport      = host [ ":" port ]
    host          = hostname | IPv4address
    hostname      = *( domainlabel "." ) toplabel [ "." ]
    domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
    toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
    IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit
    port          = *digit

    path          = [ abs_path | opaque_part ]
    path_segments = segment *( "/" segment )
    segment       = *pchar *( ";" param )
    param         = *pchar
    pchar         = unreserved | escaped |
		  ":" | "@" | "&" | "=" | "+" | "$" | ","

    query         = *uric

    fragment      = *uric

    uric          = reserved | unreserved | escaped
    reserved      = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
		  "$" | ","
    unreserved    = alphanum | mark
    mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
		  "(" | ")"

    escaped       = "%" hex hex
    hex           = digit | "A" | "B" | "C" | "D" | "E" | "F" |
			  "a" | "b" | "c" | "d" | "e" | "f"

    alphanum      = alpha | digit
    alpha         = lowalpha | upalpha

    lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" |
	     "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
	     "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
    upalpha  = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
	     "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
	     "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
    digit    = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
	     "8" | "9"


    The following line is the regular expression for breaking-down a URI
    reference into its components.

      ^(([^:/?#]+):)?(//([^/?#]* ))?([^?#]* )(\?([^#]* ))?(#(.* ))?
       12            3  4           5        6  7         8 9

    The numbers in the second line above are only to assist readability;
    they indicate the reference points for each subexpression (i.e., each
    paired parenthesis).  We refer to the value matched for subexpression
    <n> as $<n>.  For example, matching the above expression to

      http://www.ics.uci.edu/pub/ietf/uri/#Related

    results in the following subexpression matches:

      $1 = http:
      $2 = http
      $3 = //www.ics.uci.edu
      $4 = www.ics.uci.edu
      $5 = /pub/ietf/uri/
      $6 = <undefined>
      $7 = <undefined>
      $8 = #Related
      $9 = Related

    where <undefined> indicates that the component is not present, as is
    the case for the query component in the above example.  Therefore, we
    can determine the value of the four components and fragment as

      scheme    = $2
      authority = $4
      path      = $5
      query     = $7
      fragment  = $9



*)

signature URL =
sig

    (*	Host names can be in the dotted number form since
	the Sockets module uses the C gethostbyname() 
	function.

	Escapes have been removed.  Any structure inside a query is
	interpreted by the client e.g. CGI-BIN queries have a certain
	structure.  Similarly parameters are considered to be unstructured
	at this point.

	Note that a userinfo,host,port section is mutually exclusive with
	a relative path.  We don't really support relative paths anyway.
    *)

    datatype URL = HTTP_URL of {
	host:	    string option,
	port:	    int option,
	userinfo:   string option,	(* user name/password *)
	path:	    URLPath,
	query:	    string option,	(* in the undecoded form *)
	fragment:   string option	(* # suffix, undecoded *)
	}

    and URLPath = URLPath of {
	segs:	    Segment list,
	absolute:   bool
	}

    and Segment = Segment of {
	part:	    string,
	params:	    string list
	}


    exception BadURL of string		(* with a reason *)

    val emptyURL:   URL


    (*	This parses a general URL.  It raises BadURL if the syntax
	is not parsable as a HTTP URL.
    *)
    val parseURL:   string -> URL


    (*	This parses just the path part of a URL, excluding the fragment.
	It raises BadURL if the syntax is not parsable.
    *)
    val parseURLPath:   string -> URLPath


    (*	This parses just a simple path which contains no parameters.
	It raises BadURL if the syntax is not parsable.
    *)
    val parseSimplePath:   string -> URLPath


    (*	This tests if two paths match, ignoring parameters. *)
    val samePath:   URLPath -> URLPath -> bool

    (*	This splits a path into a prefix of part names and a final name.
	E.g. /a/b/c becomes SOME ([a,b], c) and / becomes NONE
    *)
    val splitPath:  URLPath -> (URLPath * string) option

    (*	Convert back to a valid URL in string form.

	This introduces escapes etc.  For now we only escape the
	"reserved" character class.  We could also escape the mark
	characters for safety.	Netscape does.

    *)
    val URLToString:	URL     -> string
    val pathToString:	URLPath -> string

    (*	This removes the % URL escapes from the string.
	Bad escapes are passed through unchanged.
    *)
    val unescapeURL:	string -> string

    (*	This escapes anything that is not an unreserved character in
	the string.
    *)
    val escapeURL:	string -> string

end



structure URL: URL =
struct

    open Common
    structure SS = Substring

(*------------------------------------------------------------------------------*)

    datatype URL = HTTP_URL of {
	host:	    string option,
	port:	    int option,
	userinfo:   string option,	(* user name/password *)
	path:	    URLPath,
	query:	    string option,	(* in the undecoded form *)
	fragment:   string option	(* # suffix, undecoded *)
	}

    and URLPath = URLPath of {
	segs:	    Segment list,
	absolute:   bool
	}

    and Segment = Segment of {
	part:	    string,
	params:	    string list
	}


    exception BadURL of string		(* with a reason *)


    val emptyURL = HTTP_URL {
	    host	= NONE,
	    port	= NONE,
	    userinfo	= NONE,
	    path	= URLPath {segs = [], absolute = true},
	    query	= NONE,
	    fragment	= NONE
	    }

(*------------------------------------------------------------------------------*)

    (*  The code here follows the regular expression algorithm described
	above.
    *)

    fun parseURL original: URL =
    let
	(*  Split off the scheme which looks like ([^:/?#]+):
	*)
	fun take_scheme input = 
	let
	    val (s, r) = SS.splitl
		(fn c => not (c = #":"
		       orelse c = #"/"
		       orelse c = #"?"
		       orelse c = #"#"))
		input
	in
	    if SS.isPrefix ":" r
	    then
		let
		    val (scheme, rest) = (s, SS.triml 1 r)
		in
		    if upperCase(SS.string scheme) = "HTTP"
		    then
			()
		    else
			raise BadURL "The scheme is not HTTP";

		    rest
		end
	    else
		input		(* it wasn't a scheme *)
	end

	(*  Look for the net_loc part, (//([^/?#]* )?
	*)
	and take_netloc input =
	(
	    if SS.isPrefix "//" input
	    then
		SS.splitl
		    (fn c => not (c = #"/"
			   orelse c = #"?"
			   orelse c = #"#"))
		    (SS.triml 2 input)
	    else
		(SS.full "", input)
	)

	(*  Look for the path part, ([^?#]* )
	*)
	and take_path input =
	(
	    SS.splitl
		(fn c => not (c = #"?"
		       orelse c = #"#"))
		input
	)

	(*  Look for the query part, ([^#]* )
	*)
	and take_query input =
	let
	    val (q, r) = SS.splitl (fn c => c <> #"#") input
	in
	    case SS.first q of
	      NONE      => (q, r)
	    | SOME #"?" => (SS.triml 1 q, r)
	    | _         => raise BadURL "junk in the query"
	end

	(*  Look for the fragment part, (#(.* ))
	*)
	and take_fragment input =
	(
	    case SS.first input of
	      NONE      => input
	    | SOME #"#" => SS.triml 1 input
	    | _         => raise BadURL "junk at the end"
	)

	(*  Split a netloc into userinfo, host and port. *)
	and split_netloc netloc =
	let
	    val (userinfo, n1) =
		let
		    val (u, r) = SS.splitl (fn c => c <> #"@") netloc
		in
		    if SS.isEmpty r
		    then
			(SS.full "", netloc)
		    else
			(u, SS.triml 1 r)
		end


	    val (host, port) =
		let
		    val (h, r) = SS.splitl (fn c => c <> #":") n1
		in
		    if SS.isEmpty r
		    then
			(n1, SS.full "")
		    else
			(h, SS.triml 1 r)
		end

	    val iport =
		if SS.isEmpty port
		then
		    NONE
		else
		(
		    case Int.fromString(SS.string port) of
		      NONE => raise BadURL (concat["Invalid port number ", 
		      				   SS.string port])
		    | SOME p => SOME p
		)
	in
	    (decodeOpt(sub_to_opt userinfo), sub_to_opt host, iport)
	end


	and sub_to_opt s =
	(
	    if SS.isEmpty s then NONE else SOME(SS.string s)
	)


	(*  This unescapes the string for final presentation.  *)
	and decodeOpt s = Option.map unescapeURL s


	val u1    	    = take_scheme (SS.full original)
	val (netloc, u3)    = take_netloc u1
	val (path,   u4)    = take_path u3
	val (query,  u5)    = take_query u4
	val fragment        = take_fragment u5

	val (userinfo, host, port) = split_netloc netloc
	val urlpath = parse_URLPath true path
    in
	HTTP_URL {
	    host	= host,
	    port	= port,
	    userinfo	= userinfo,
	    path	= urlpath,
	    query	= sub_to_opt query,
	    fragment	= sub_to_opt fragment
	    }
    end
    handle x => raise BadURL 
	(concat["URL exception ", exnName x, ": ",exnMessage x])



    and parseURLPath original    = parse_URLPath true (SS.full original)
    and parseSimplePath original = parse_URLPath false (SS.full original)



    and parse_URLPath with_params (original: substring) : URLPath =
    let
	val (abs, rel) =
	    case SS.first original of
	      NONE      => (true, SS.full "")	(* fake a "/" path *)
	    | SOME #"/" => (true, SS.triml 1 original)
	    | _         => (false, original)

	(*  When original="/" we will end up with an empty segment list.
	    If there is a trailing / then we want to delete the empty field.
	*)
	val segments =
	    let
		val fields = SS.fields (fn c => c = #"/") rel
	    in
		case rev fields of
		  []        => fields
		| (f::rest) => if SS.isEmpty f then rev rest else fields
	    end


	fun parse_params seg =
	let
	    val parts = SS.fields (fn c => c = #";") seg
	    val unesc = map (unescapeURL o SS.string) parts
	in
	    case unesc of
	      []     => Segment {part = "", params = []}
	    | (p::r) => Segment {part = p,  params = r}
	end


	and parse_no_params seg =
	(
	    Segment {part = unescapeURL(SS.string seg), params = []}
	)


	val segs =
	    if with_params
	    then
		map parse_params segments
	    else
		map parse_no_params segments
    in
	URLPath {
	    segs     = segs,
	    absolute = abs
	    }
    end
    handle x => raise BadURL 
	(concat["URL exception ", exnName x, ": ",exnMessage x])



    and samePath (URLPath {segs=seg1, absolute=abs1})
                 (URLPath {segs=seg2, absolute=abs2}) =
    let
	fun all_match [] [] = true
	|   all_match _  [] = false
	|   all_match [] _  = false
	|   all_match (s1::r1) (s2::r2) =
	let
	    val Segment {part = p1, ...} = s1
	    val Segment {part = p2, ...} = s2
	in
	    if p1 = p2 then all_match r1 r2 else false
	end
    in
	abs1 = abs2 andalso all_match seg1 seg2
    end



    and splitPath (URLPath {segs, absolute}) =
    (
	case rev segs of
	  [] => NONE

	| (last::rest) =>
	    let
		val prefix = URLPath {segs=rev rest, absolute=absolute}
		val Segment {part, ...} = last
	    in
		SOME (prefix, part)
	    end
    )



    and URLToString url =
    let
	val HTTP_URL {host, port, userinfo, path, query, fragment} = url

	val netloc =
	    concat[
		case userinfo of
		  NONE => ""
		| SOME u => ((escapeURL u) ^ "@"),
		
		case host of
		  NONE => ""
		| SOME h => h,

		case port of
		  NONE => ""
		| SOME p => (":" ^ (Int.toString p))
		]
    in
	concat[
	    "http:",
	    if netloc = "" then "" else ("//" ^ netloc),
	    pathToString path,

	    case query of
	      NONE => ""
	    | SOME q => ("?" ^ (escapeURL q)),

	    case fragment of
	      NONE => ""
	    | SOME f => ("#" ^ (escapeURL f))
	    ]
    end


    and pathToString (URLPath {segs, absolute}) =
    let
	fun do_segs [] = []
	|   do_segs [s] = [do_seg s]
	|   do_segs (s::rest) = (do_seg s)::"/"::(do_segs rest)

	and do_seg (Segment {part, params}) =
	(
	    concat((escapeURL part) :: (map do_param params))
	)

	and do_param p = ";" ^ (escapeURL p)
    in
	concat(if absolute then "/"::(do_segs segs) else do_segs segs)
    end



    (*	This removes the % URL escapes from the string.
	Bad escapes are passed through unchanged.
    *)
    and unescapeURL str =
    let
	fun loop []                   result = implode(rev result)

	|   loop (#"%"::x1::x2::rest) result =
	let
	    val hex = implode [x1, x2]
	in
	    case StringCvt.scanString (Int.scan StringCvt.HEX) hex of
	      SOME ch => loop rest ((chr ch)::result)
	    | NONE    => loop (x1::x2::rest) (#"%"::result)
	end

	|   loop (c::rest) result = loop rest (c::result)
    in
	loop (explode str) []
    end


    (*	This escapes anything that is not an unreserved character in
	the string.

    *)
    and escapeURL string =
    let
	fun cvt c =
	(
	    if Char.isAlphaNum c orelse Char.contains "-_.!~*'()" c
	    then
		str c
	    else
		let
		    val x = Int.fmt StringCvt.HEX (ord c)
		    val hex = if size x = 1 then "0" ^ x else x
		in
		    "%" ^ hex
		end
	)
    in
	String.translate cvt string
    end

(*------------------------------------------------------------------------------*)

end
