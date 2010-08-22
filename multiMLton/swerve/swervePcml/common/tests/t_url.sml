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

(* $Id: t_url.sml,v 1.4 2001/05/28 23:30:51 felix Exp $ *)

signature T_URL =
sig
    val run:	unit -> unit
end



structure t_URL: T_URL =
struct

    open TestUtils
    open URL


    fun expectPath abs nsegs (URLPath {absolute, segs}) msg =
    (
	if abs = absolute then () else fail (msg ^ ", abs/rel mismatch");
	if length segs = nsegs then () else fail (msg ^ ", seg count mismatch")
    )


    fun expectAbs (URLPath {absolute, ...}) msg =
    (
	if absolute then () else fail msg
    )


    (*	The first segment is numbered 1. *)
    fun expectSeg n (xpart, xparams) (URLPath {segs, ...}) msg =
    (
	if length segs >= n
	then
	    let
		val Segment {part, params} = List.nth(segs, n-1)
	    in
		expectS xpart part;

		if length xparams = length params
		then
		    app (fn (p1, p2) => expectS p1 p2 (msg ^ ", parameter"))
			(ListPair.zip(xparams, params))
		else
		    fail (msg ^ ", parameter length mismatch")
	    end
	else
	    fail (concat[msg, ", no segment ", Int.toString n])
    )


    fun dump_url url =
    let
	val HTTP_URL {host, port, userinfo, path, query, fragment} = url

	fun opt_string NONE = "NONE"
	|   opt_string (SOME s) = s

	fun opt_int NONE = "NONE"
	|   opt_int (SOME i) = Int.toString i

	fun dump_path (URLPath {absolute, segs}) =
	let
	in
	    if absolute then print "/" else ();
	    app dump_seg segs;
	    print "\n"
	end

	and dump_seg (Segment {part, params}) =
	let
	in
	    print part;
	    app (fn p => (print ";"; print p)) params;
	    print "/"
	end
    in
	print "host      ";	print(opt_string host);	    print "\n";
	print "port      ";	print(opt_int port);	    print "\n";
	print "userinfo  ";	print(opt_string userinfo); print "\n";
	print "path      ";	dump_path path;
	print "query     ";	print(opt_string query);    print "\n";
	print "fragment  ";	print(opt_string fragment);    print "\n"
    end


(*------------------------------------------------------------------------------*)

    fun test1() =
    let
	val url = "http://www.ics.uci.edu/pub/ietf/uri/#Related"
	val purl as HTTP_URL {host, port, userinfo, path, query, fragment} =
		parseURL url
    in
	print "\n"; print url; print "\n"; dump_url purl;
	print "toString: "; print(URLToString purl); print "\n";

	expectSOpt "www.ics.uci.edu" host "test 1, host name";
	expectEmpty port		"test 1, port";
	expectEmpty userinfo		"test 1, userinfo";
	expectEmpty query		"test 1, query";
	expectSOpt "Related" fragment   "test 1, fragment";

	expectPath true 3 path		"test 1";

	expectSeg 1 ("pub",  []) path	"test 1, seg 1";
	expectSeg 2 ("ietf", []) path	"test 1, seg 2";
	expectSeg 3 ("uri",  []) path	"test 1, seg 3";

	()
    end
    handle BadURL s => fail (concat["test 1, bad URL: ", s])


    fun test2() =
    let
	val url = "http://www.porn.com:1000/coor;param1;param2/?query"
	val purl as HTTP_URL {host, port, userinfo, path, query, fragment} =
		parseURL url
    in
	print "\n"; print url; print "\n"; dump_url purl;
	print "toString: "; print(URLToString purl); print "\n";

	expectSOpt "www.porn.com" host  "test 2, host name";
	expectIOpt 1000 port		"test 2, port";
	expectEmpty userinfo		"test 2, userinfo";
	expectSOpt "query" query	"test 2, query";
	expectEmpty fragment   		"test 2, fragment";

	expectPath true 1 path		"test 2";

	expectSeg 1 ("coor", ["param1", "param2"]) path    "test 2, seg 1";

	()
    end
    handle BadURL s => fail (concat["test 2, bad URL: ", s])


    fun test3() =
    let
	val url = "http://user:pwd@:1000"
	val purl as HTTP_URL {host, port, userinfo, path, query, fragment} =
		parseURL url
    in
	print "\n"; print url; print "\n"; dump_url purl;
	print "toString: "; print(URLToString purl); print "\n";

	expectEmpty host  		    "test 3, host name";
	expectIOpt 1000		port	    "test 3, port";
	expectSOpt "user:pwd"	userinfo    "test 3, userinfo";
	expectEmpty query		    "test 3, query";
	expectEmpty fragment   		    "test 3, fragment";

	expectPath true 0 path		    "test 3";

	()
    end
    handle BadURL s => fail (concat["test 3, bad URL: ", s])




    fun test4() =
    let
	val url = "http://www.nowhere.com/db.php? name =fred"
	val purl as HTTP_URL {host, port, userinfo, path, query, fragment} =
		parseURL url
    in
	print "\n"; print url; print "\n"; dump_url purl;
	print "toString: "; print(URLToString purl); print "\n";

	expectSOpt "www.nowhere.com" host   "test 4, host name";
	expectEmpty port	            "test 4, port";
	expectEmpty userinfo		    "test 4, userinfo";
	expectSOpt  " name =fred" query	    "test 4, query";
	expectEmpty fragment   		    "test 4, fragment";

	expectPath true 1 path		    "test 4";
	expectSeg 1 ("db.php",  []) path    "test 4, seg 1";

	()
    end
    handle BadURL s => fail (concat["test 4, bad URL: ", s])



    fun run() =
    (
	test1();
	test2();
	test3();
	test4()
    )

(*------------------------------------------------------------------------------*)

end
