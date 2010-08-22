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

(* This contains a copy of the Compiler.Profile.report function that can
 * be linked with CML's TextIO. *)

signature MYPROFILE =
sig
    
    val	start:	unit -> unit
    val	stop:	unit -> unit
			
    (*	This times the execution of the function in real-time and logs
     * the result as a Debug message under the Timing flag. *)
    val timeIt:	string -> ('a -> 'b) -> 'a -> 'b
end

structure MyProfile: MYPROFILE =
struct

    structure TF = TextFrag

    type entry = string * Time.time

    (* For the least overhead in timing the entries are stuffed into
     * a list in the heap. This is not thread-safe but will probably
     * work well enough given the CML scheduling algorithms (up until 
     * a GC happens). *)
    val entries: entry list ref = ref []
				  
    fun timeIt name func arg =
	let val now = Time.now ()
	in
	    func arg before ( if Globals.testing Globals.TestTiming
			      then let val diff = Time.- ( Time.now (), now )
				   in
				       Log.inform Log.Debug (fn () => TF.concat ["Timed ", name, " in ", Time.toString diff, "."])
(*				       entries := ( name, diff ) :: ( !entries ) *)
				   end handle x => Log.logExn x
			      else () )
	end

    fun flushIt() =
	let fun put ( name, diff ) = 
		TextIO.output ( TextIO.stdOut, 
				concat [ "Timing ", name, " ",
					 LargeInt.toString(Time.toMicroseconds diff),
					 "\n" ] )
	in
	    app put (rev(!entries));
	    entries := []
	end

    fun field (st, w) = StringCvt.padLeft #" " w st

    (* take a string of digits and a number of decimal places, and return the
     * digits with the decimal point added in the right place. *)
    fun decimal (st, w) = 
	let val len = size st
	in
	    if (len <= w)
	    then String.concat [".", StringCvt.padLeft #"0" w st]
	    else String.concat [ substring(st, 0, len-w), 
				 ".", 
				 substring(st, len-w, w) ]
	end

    fun report() = ()
(*    let
	val biglist' = Compiler.Profile.reportData()

	fun toMicr t = Real.fromLargeInt(Time.toMicroseconds t)
	val tot = List.foldr (fn ({time=a,...},b)=> (toMicr a)+b) 0.0 biglist'

	val maxc = List.foldr (fn ({count=a,...},b)=> Int.max(a,b)) 0 biglist'

	val digits_cum = 8
	val digits_cnt = 8
	fun pr s = TextIO.output(TextIO.stdErr, s)

	fun printlines ({time,name,count}::rest, cum) =
	let
	    val tm = toMicr time
	in
	    pr(field(Real.fmt (StringCvt.FIX(SOME 2)) (100.0*tm/tot), 6));
	    pr(field(Int.toString count, digits_cnt));
	    pr "  "; pr name; pr "\n";
	    printlines(rest,cum+tm)
	end
	| printlines (nil, _) = ()

    in
    	pr(field("%time",6));
	(*pr(field("cumsec",7));*)
	pr(field("#call",digits_cnt));
	pr("  name\n");
	printlines(biglist',0.0);
	TextIO.flushOut TextIO.stdErr
    end *)



    fun start() = ()
    (* if Globals.testing Globals.TestProfile
     * then Compiler.Profile.setTimingMode true
     * else () *)

    fun stop() = ()
(*    (
	flushIt();

	if Globals.testing Globals.TestProfile
	then
	(
	    Compiler.Profile.setTimingMode false;
	    report()
	)
	else
	    ()
    ) *)

end
