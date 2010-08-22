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

(* $Id: t_utils.sml,v 1.1 2001/06/11 12:26:58 felix Exp $ *)

structure t_Utils =
struct

    structure TU = TestUtils
    structure IP = IETF_Part

(*------------------------------------------------------------------------------*)


    (*	Test base64 decoding.
    	REVISIT - test with 0, 1 and 3 = padding.
    *)


    fun test_b64() =
    (
	print "Testing base64 decoding.\n";

	TU.expectSOpt "Aladdin:open sesame"  
		    (IETF_Utils.base64Decode "QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
		   "base64 a";


	TU.expectS "QUI="
		    (IETF_Utils.base64Encode "AB")
		   "base64 b";

	TU.expectS "QUJD"
		    (IETF_Utils.base64Encode "ABC")
		   "base64 c";

	TU.expectS "QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
		    (IETF_Utils.base64Encode "Aladdin:open sesame")
		   "base64 d";
	()
    )


    fun test_split() =
    let
	val _     = print "Testing line splitting.\n";
	val text  = " abc/def;x=\" y\"\t"
	val parts = IETF_Line.split text
    in

	if parts = [
		IP.TWh   " ",
		IP.Token "abc",
		IP.TSpec #"/",
		IP.Token "def",
		IP.TSpec #";",
		IP.Token "x",
		IP.TSpec #"=",
		IP.Token " y",
		IP.TWh   "\t"
		]
	then
	    ()
	else
	    TU.fail "split 1";

	if IETF_Line.join parts = " abc/def;x=\" y\""
	then
	    ()
	else
	    TU.fail "split 2";

	()
    end

(*------------------------------------------------------------------------------*)


    fun run() =
    (
	test_b64();
	test_split();

	()
    )

end
