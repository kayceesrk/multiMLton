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

(* These are a few global flags which are typically set from the command line. *)

signature GLOBALS =
sig
    val version: string
    val cgi_version: string

    val set_testing:	string -> unit
    val testing: 	int -> bool

    (*  Test flags. *)

    val TestConnect:	    int		(* show connection activity *)
    val TestTmpFile:	    int		(* show temp file activity *)
    val TestShowRequest:    int		(* show headers of a request *)
    val TestShowResponse:   int		(* show content of a response *)
    val TestStoreBuild:	    int		(* show building of the store *)
    val TestStoreProto:	    int		(* show messages in the store *)
    val TestCGIProto:	    int		(* show CGI activity in the store *)
    val TestOpenMgr:	    int		(* show OpenMgr activity *)
    val TestFinalise:	    int		(* show finaliser activity *)
    val TestAuth:	    int		(* show authorisation activity *)

    val TestProfile:	    int		(* profile the code if compiled in *)
    val TestTiming:	    int		(* log timing messages *)

    val TestTimeout:	    int		(* show timeout activity *)

end

structure Globals: GLOBALS =
struct

  val version = "@(#)swerve 0.1"
  val cgi_version = "Swerve/0.1"	(* advertise to CGI *)

  (* Command line testing flags. 
   * For maximum speed I do unsafe indexing into an array. *)
		    
  val TestConnect		= 0
  val TestTmpFile		= 1
  val TestShowRequest		= 2
  val TestShowResponse	        = 3
  val TestStoreBuild		= 4
  val TestStoreProto		= 5
  val TestCGIProto		= 6
  val TestOpenMgr		= 7
  val TestFinalise		= 8
  val TestAuth		        = 9
  val TestProfile		= 10
  val TestTiming		= 11
  val TestTimeout		= 12
  val TestMAXCODE		= 200

  fun code_of_test "TMPFILE"			= TestTmpFile
    |   code_of_test "SHOWREQUEST"		= TestShowRequest
    |   code_of_test "SHOWRESPONSE"		= TestShowResponse
    |   code_of_test "STOREBUILD"		= TestStoreBuild
    |   code_of_test "STOREPROTO"		= TestStoreProto
    |   code_of_test "CGIPROTO"			= TestCGIProto
    |   code_of_test "CONNECT"			= TestConnect
    |   code_of_test "OPENMGR"			= TestOpenMgr
    |   code_of_test "FINALISE"			= TestFinalise
    |   code_of_test "AUTH"			= TestAuth
    |   code_of_test "PROFILE"			= TestProfile
    |   code_of_test "TIMING"			= TestTiming
    |   code_of_test "TIMEOUT"			= TestTimeout
    |   code_of_test _				= 0
						  
						  
  val zero = #"0"
	     
  val test_flags =
      let val f = Unsafe.CharArray.create (TestMAXCODE+1)
	  fun fill n = (Unsafe.CharArray.update(f, n, #"1"); (* RPR was zero *)
	    		if n = 0 then () else fill (n-1))
      in
	  fill TestMAXCODE;
	  f
      end
      
  fun set_testing ( flag: string ) =
      let val code = code_of_test ( Common.upperCase flag )
      in
	  if code = 0
	  then Common.printToErr ( concat [ "Unrecognised test flag ", flag, "\n" ] )
	  else Unsafe.CharArray.update ( test_flags, code, #"1" )
      end
      
  fun testing query = Unsafe.CharArray.sub ( test_flags, query ) <> zero
		      
end
