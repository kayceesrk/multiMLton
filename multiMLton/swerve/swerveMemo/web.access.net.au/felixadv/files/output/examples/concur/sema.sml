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


signature SEMAPHORE =
sig
    type Sema

    val new:	    int -> Sema 
    val acquireEvt: Sema -> unit CML.event
    val acquire:    Sema -> unit
    val value:	    Sema -> int
    val release:    Sema -> unit

end



structure Sema: SEMAPHORE =
struct
    structure SV = SyncVar

    (*	A reply channel and a nack event. *)
    type Client = (unit CML.chan * unit CML.event)

    datatype Request = 
	    ReqIsAcq of Client
	|   ReqIsRel
	|   ReqIsGet of int SV.ivar

    and Sema = Sema of Request CML.chan


    fun new init =
    let
	val req_chan = CML.channel()

	fun sema() =
	let
	    fun loop (value, pending: Client list) =
	    (
		case CML.recv req_chan of
		  ReqIsAcq client =>
		    let		(* FIFO order *)
			val new_pending = pending @ [client]
		    in
			if value <= 0
			then
			    loop (value, new_pending)
			else
			    loop (grant value new_pending)
		    end

		| ReqIsRel => loop (grant (value+1) pending)

		| ReqIsGet rpl_var =>
		(
		    SV.iPut(rpl_var, value);
		    loop (value, pending)
		)
	    )
	in
	    loop (init, [])
	end

	(*  Look for a pending client that will accept the grant.
	    Return the decremented value and the remaining pending 
	    clients if a client accepts the grant.
	*)
	and grant value [] = (value, [])	(* no pending clients *)

	|   grant value ((rpl_chan, nack_evt) :: rest) =
	let
	    fun accepted() = (value-1, rest)
	    fun nacked()   = (print "Got a nack\n"; grant value rest)
	in
	    CML.select [
		CML.wrap(CML.sendEvt(rpl_chan, ()), accepted),
		CML.wrap(nack_evt, nacked)
		]
	end

	val thread = CML.spawn sema
    in
	Sema req_chan
    end


    fun acquireEvt (Sema req_chan) =
    let
	fun sender nack_evt =
	let
	    val rpl_chan = CML.channel()
	in
	    CML.spawn(fn () =>
		CML.send(req_chan, ReqIsAcq (rpl_chan, nack_evt)));
	    CML.recvEvt rpl_chan
	end
    in
	CML.withNack sender
    end

    fun acquire l = CML.sync(acquireEvt l)

    fun release (Sema req_chan) = CML.send(req_chan, ReqIsRel)


    fun value (Sema req_chan) =
    let
	val rpl_var = SV.iVar()
    in
	CML.send(req_chan, ReqIsGet rpl_var);
	SV.iGet rpl_var
    end

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
	grab sema "2" 2;
	delay 1;
	Sema.release sema;
	check sema "1";
	delay 4;
	check sema "1";
	()
    end

    and grab sema n t =
    let
	fun hold() =
	(
	    check sema n;
	    delay 3;
	    Sema.release sema;
	    check sema n
	)

	fun timedout() = print(concat["Client ", n, " timed out\n"])
    in
	CML.spawn(fn () =>
	    CML.select[
		CML.wrap(Sema.acquireEvt sema, hold),
		CML.wrap(time_out t, timedout)
		]
	    )
    end

    and time_out t = CML.timeOutEvt(Time.fromSeconds t)
    and delay t    = CML.sync(time_out t)



    fun test3() =
    let
	val sema = Sema.new 1
    in
	print "Test 3\n";
	Sema.acquire sema;
	check sema "1";
	grab sema "2" 2;
	delay 3;		(* client 2 times out *)
	Sema.release sema;	(* sema attempts to grant *)
	check sema "1"
    end




    fun run() =
    let
    in
	TraceCML.setTraceFile TraceCML.TraceToOut;
	test1();
	test2();
	test3();
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

    val _ = SMLofNJ.exportFn("sema", main)
end




