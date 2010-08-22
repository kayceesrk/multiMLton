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

(*  Copyright (c) 2001 Anthony L Shipman *)

(* $Id: t_headers.sml,v 1.1 2001/06/11 12:26:58 felix Exp $ *)

structure t_Headers =
struct

    open Common
    open HTTPHeader

    structure TF = TextFrag
    structure TU = TestUtils


    val date1 = Date.date {
	    year    = 1960,
	    month   = Date.Apr,
	    day     = 15,
	    hour    = 6,
	    minute  = 23,
	    second  = 8,
	    offset  = SOME(Time.fromSeconds 0)	(* GMT *)
	    }


    fun showHeader h = TF.toString TF.UseLf (formatHeader h)

(*------------------------------------------------------------------------------*)


    fun test_hdr_date() =
    let
	fun check hdr dt =
	(
	    case parseHeader hdr of
	      HdrDate d =>
		(
		    if Date.compare(d, dt) = EQUAL
		    then
			()
		    else
			TU.fail(concat[hdr, " returned the date ", Date.toString d])
		)

	    | HdrBad s => TU.fail(concat[s, " returned Bad"])

	    | _ => TU.fail(concat[hdr, " did not parse"])
	)
    in
	print "Testing date headers.\n";

       (*
       Sun, 06 Nov 1994 08:49:37 GMT    ; RFC 822, updated by RFC 1123
       Sunday, 06-Nov-94 08:49:37 GMT   ; RFC 850, obsoleted by RFC 1036
       Sun Nov  6 08:49:37 1994         ; ANSI C's asctime() format
       *)


	check "Date: Fri, 15 Apr 1960 06:23:08 GMT"  date1;
	check "Date: Friday, 15-Apr-60 06:23:08 GMT" date1;
	check "Date: Fri Apr 15 06:23:08 1960"       date1;
	()
    end



    and test_ifmodify() =
    let
	fun check hdr dt =
	(
	    case parseHeader hdr of
	      HdrIfModified d =>
		(
		    if Date.compare(d, dt) = EQUAL
		    then
			()
		    else
			TU.fail(concat[hdr, " returned the date ", Date.toString d])
		)

	    | HdrBad s => TU.fail(concat[s, " returned Bad"])

	    | _ => TU.fail(concat[hdr, " did not parse"])
	)
    in
	print "Testing If-Modified-Since headers.\n";

	check "If-Modified-Since: Fri, 15 Apr 1960 06:23:08 GMT" date1;
	()
    end




    and test_lastmodify() =
    let
	fun check hdr dt =
	(
	    case parseHeader hdr of
	      HdrLastModified d =>
		(
		    if Date.compare(d, dt) = EQUAL
		    then
			()
		    else
			TU.fail(concat[hdr, " returned the date ", Date.toString d])
		)

	    | HdrBad s => TU.fail(concat[s, " returned Bad"])

	    | _ => TU.fail(concat[hdr, " did not parse"])
	)
    in
	print "Testing Last-Modified headers.\n";

	check "Last-Modified: Fri, 15 Apr 1960 06:23:08 GMT" date1;
	()
    end


(*------------------------------------------------------------------------------*)

    and test_pragma() =
    let
	val text = "Pragma: no-cache"
    in
	print "Testing the pragma headers.\n";

	case parseHeader text of
	  HdrPragma s => TU.expectS "no-cache" s "pragma"
	| _           => TU.fail "pragma did not parse";

	TU.expectS text (showHeader(parseHeader text))
    end



    and test_from() =
    let
	val text = "From: Fred <fred@ducky.com>"
    in
	print "Testing the From headers.\n";

	case parseHeader "From: Fred <fred@ducky.com>" of
	  HdrFrom s => TU.expectS "Fred <fred@ducky.com>" s "From"
	| _           => TU.fail "From did not parse";

	TU.expectS text (showHeader(parseHeader text))
    end



    and test_referer() =
    let
	val text = "Referer: http://www.nowhere.com/db.php? name =fred"
    in
	print "Testing the Referer headers.\n";

	case parseHeader text of
	  HdrReferer s =>
	    TU.expectS "http://www.nowhere.com/db.php? name =fred" s "Referer"
	| _ => TU.fail "Referer did not parse";

	TU.expectS text (showHeader(parseHeader text))
    end



    and test_useragent() =
    let
	val text = "User-Agent: Mozilla v1.0\t  "
    in
	print "Testing the User-Agent headers.\n";

	case parseHeader text of
	  HdrUserAgent s =>
	    TU.expectS "Mozilla v1.0" s "User-Agent"
	| _ => TU.fail "User-Agent did not parse";

	TU.expectS text (showHeader(parseHeader text))
    end



    and test_auth() =
    let
	val text = "Authorization: basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
    in
	print "Testing the Auth headers.\n";

	case parseHeader text of
	  HdrAuthorization(AuthBasic(id_opt, pwd)) =>
	(
	    TU.expectSOpt "Aladdin" id_opt "Authorization id";
	    TU.expectS "open sesame" pwd "Authorization pwd"
	)
	| _ => TU.fail "Auth did not parse";

	TU.expectS text (showHeader(parseHeader text))
    end




    and test_exten() =
    let
	val text = "X-Extension: the quick brown fox!  "
    in
	print "Testing the Extension headers.\n";

	case parseHeader text of

	  HdrExt (t, s) =>
	(
	    TU.expectS "X-Extension"          t "Extension name";
	    TU.expectS "the quick brown fox!" s "Extension value"
	)

	| _ => TU.fail "Extension did not parse";

	TU.expectS text (showHeader(parseHeader text))
    end




    and test_bad() =
    let
    in
	print "Testing Bad headers.\n";

	case parseHeader "the quick brown fox!  " of
	  HdrBad s => TU.expectS "the quick brown fox!  " s "bad 1"

	| _ => TU.fail "Bad header 1 was not recognised";

	case parseHeader "the:quick brown fox!  " of
	  HdrBad s => TU.expectS "the:quick brown fox!  " s "bad 2"

	| _ => TU.fail "Bad header 2 was not recognised"
    end

(*------------------------------------------------------------------------------*)

    and test_encoding() =
    let
	fun check text req_enc =
	let
	    val hdr = concat["Content-Encoding: ", text, " "]
	in
	    case parseHeader hdr of
	      HdrConEnc enc =>
	    (
		if enc = req_enc
		then
		    ()
		else
		    TU.fail (concat[hdr, " returned the wrong encoding"])
	    )

	    | _ => TU.fail (concat[hdr, " did not parse"]);

	    TU.expectS hdr (showHeader(parseHeader hdr))
	end
    in
	print "Testing the Content-Encoding headers.\n";

	check "x-gzip"     Entity.EncGZip;
	check "gzip"       Entity.EncGZip;
	check "x-compress" Entity.EncCompress;
	check "compress"   Entity.EncCompress;
	check "deflate"    Entity.EncDeflate;
	check "identity"   Entity.EncIdentity;
	check "unknown"    (Entity.EncOther "unknown");
	()
    end



    and test_mediatype() =
    let
	fun check text req_type req_subtype req_params =
	let
	    val hdr = concat["Content-Type: ", text, " "]
	in
	    case parseHeader hdr of
	      HdrConType(Entity.MType {mtype, msubtype, mparams}) =>
	    (
		if mtype = req_type andalso
		   msubtype = req_subtype andalso
		   match mparams req_params
		then
		    ()
		else
		    TU.fail (concat[hdr, " returned the wrong type"])
	    )

	    | _ => TU.fail (concat[hdr, " did not parse"]);

	    TU.expectS hdr (showHeader(parseHeader hdr))
	end


	and match params req_params =
	let
	    fun pair ((p1, v1), (p2, v2)) = (p1 = p2) andalso (v1 = v2)
	in
	    length params = length req_params andalso
	    List.all pair (ListPair.zip(params, req_params))
	end
    in
	print "Testing the Content-Type headers.\n";

	check "text/html"  "text" "html" [];
	check "image/jpeg;x=y" "image" "jpeg" [("x", "y")];
	()
    end



    and test_contlength() =
    let
	val text = "Content-Length: 1000  "
    in
	print "Testing the Content-Length headers.\n";

	case parseHeader text of
	  HdrConLen 1000 => ()
	| HdrConLen _    => TU.fail "Content-Length returned the wrong value"
	| _ => TU.fail "Content-Length did not parse";

	TU.expectS text (showHeader(parseHeader text))
    end



(*------------------------------------------------------------------------------*)


    fun run() =
    (
	test_hdr_date();
	test_ifmodify();
	test_lastmodify();

	test_pragma();
	test_from();
	test_referer();
	test_useragent();

	test_encoding();
	test_mediatype();
	test_contlength();

	test_auth();
	test_exten();
	test_bad();
	()
    )

end
