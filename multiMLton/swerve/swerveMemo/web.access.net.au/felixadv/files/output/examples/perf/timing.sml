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


(*  This is common code for timing functions.
*)

signature TIMING =
sig
    val timeIt:	string -> (unit -> 'a) -> 'a
    val report: unit -> unit
end


structure Timing: TIMING =
struct

    (*	To minimise the measurement overhead the entries are 
	saved here. There is no thread-safety but I'll probably
	get away with it given the way the CML scheduling works.
    *)
    type TimeEntry = string * Time.time

    val	queue: TimeEntry list ref = ref []


    fun timeIt name func =
    let
	val now = Time.now()
    in
	func() before
	    let
		val diff = Time.-(Time.now(), now)
	    in
		queue := (name, diff) :: (!queue)
	    end
    end


    fun report() =
    let
	fun show (name, diff) =
	(
	    print(concat["Timing ", name, " ",
		LargeInt.toString(Time.toMicroseconds diff), "\n"])
	)
    in
	app show (rev(!queue))
    end

end



(*  This provides coarser CPU based timing in milliseconds with 10ms resolution.
*)

signature CPU_TIMING =
sig
    val timeIt:	string -> (unit -> 'a) -> 'a
end


structure CPUTiming =
struct

    fun timeIt name func =
    let
	val timer = Timer.startCPUTimer()
    in
	func() before
	    let
		val {usr, sys, gc} = Timer.checkCPUTimer timer
	    in
		print(concat["CPU Timing ", name, " usr=",
		    LargeInt.toString(Time.toMilliseconds usr),
		    "ms GC=",
		    LargeInt.toString(Time.toMilliseconds gc),
		    "ms\n"])
	    end
    end

end

