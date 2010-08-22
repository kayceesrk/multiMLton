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


(*  This implements a mutex to lock access to a critical section. The section
    is represented by a function.
*)

signature MUTEX =
sig
    type Mutex

    (*	Mutex values can be saved over an exportML so you can
	"statically" create them.
    *)
    val create:	    unit -> Mutex

    (*	This runs the function in the critical section.
	This will work with CML running or not.
    *)
    val lock:	    Mutex -> (unit -> 'a) -> 'a
end



structure Mutex: MUTEX =
struct
    structure SV = SyncVar

    type Mutex = bool SV.mvar


    fun create() = SV.mVarInit true


    fun lock mutex func =
    (
	SV.mTake mutex;
	let
	    val r = func()
	in
	    SV.mPut(mutex, true);
	    r
	end
	handle x => (
	    SV.mPut(mutex, true);
	    raise x
	    )
    )
end




signature SEMAPHORE =
sig
    type Sema

    val new:	    int -> Sema 
    val acquire:    Sema -> unit
    val value:	    Sema -> int
    val release:    Sema -> unit

end



structure Sema: SEMAPHORE =
struct
    structure SV = SyncVar

    datatype Sema = Sema of {
	rsrc:	int SV.mvar,	(* count of resources avail *)
	cond:	unit CML.chan	(* signals a resource is avail *)
	}

    fun new n =
    (
	Sema {
	    rsrc = SV.mVarInit(Int.max(0, n)),
	    cond = CML.channel()
	    }
    )


    fun acquire (sema as Sema {rsrc, cond}) =
    let
	val n = SV.mTake rsrc
    in
	if n = 0
	then
	    (SV.mPut(rsrc, n); CML.recv cond; acquire sema)
	else
	    SV.mPut(rsrc, n-1)
    end


    (*	A race could lead to several releases before an acquirer can 
	get to the cond. The releases will block on the send so none
	get lost.  

	Spawning now negates the cheapness of mvars.

    *)

    fun release (Sema {rsrc, cond}) = 
    let
	fun notify() = ignore(CML.spawn(fn() => CML.send(cond, ())))
	val n = SV.mTake rsrc
    in
	SV.mPut(rsrc, n+1);
	if n = 0 then notify() else ()
    end

    fun value (Sema {rsrc, ...}) = SV.mGet rsrc

end (* of structure Sema *)





structure Main =
struct
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)


    fun test1() =
    let
	val sema = Sema.new 1
    in
	print "Test 1\n";
	check sema "1";
	Sema.acquire sema;
	check sema "1";
	Sema.release sema;
	check sema "1";
	()
    end

    and check sema n =
    (
	print(concat[
	    "Client ", n, ": the sema value is ",
	    Int.toString(Sema.value sema),
	    "\n"])
    )



    fun test2() =
    let
	val sema = Sema.new 1
    in
	print "Test 2\n";
	Sema.acquire sema;
	check sema "1";
	grab sema "2";
	delay 1;
	print "Client 1 releases\n";
	Sema.release sema;
	check sema "1";
	delay 4;
	check sema "1";
	()
    end

    and grab sema n =
    let
	fun hold() =
	(
	    print(concat["Client ", n, " starts\n"]);
	    Sema.acquire sema;
	    check sema n;
	    delay 3;
	    Sema.release sema;
	    check sema n
	)
    in
	CML.spawn hold
    end

    and time_out t = CML.timeOutEvt(Time.fromSeconds t)
    and delay t    = CML.sync(time_out t)




    fun run() =
    let
    in
	TraceCML.setTraceFile TraceCML.TraceToOut;
	test1();
	test2();
	()
    end



    fun main(arg0, argv) =
    let
    in
	RunCML.doit(run, NONE);
        OS.Process.success
    end
    handle
      x =>
    (
	toErr(concat["Uncaught exception: ", exnMessage x, " from\n"]);
	app (fn s => (print "\t"; print s; print "\n")) (SMLofNJ.exnHistory x);
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("sema2", main)
end




