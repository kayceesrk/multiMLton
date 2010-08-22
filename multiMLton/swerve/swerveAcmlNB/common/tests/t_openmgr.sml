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

(* $Id: t_openmgr.sml,v 1.5 2001/07/10 18:37:32 felix Exp $ *)

signature T_OPENMGR =
sig
    val run:	unit -> unit
end



structure t_OpenMgr: T_OPENMGR =
struct

    open TestUtils

    structure Ctr = OpenCounter

(*------------------------------------------------------------------------------*)


    (*	Try reading the sample1 file. *)

    fun test1() = 
    (
	expectI 3 (count_lines "sample1")	"test1 sample1"
    )


    and count_lines file =
    let
	fun run h =
	let
	    val strm = TextIOReader.get h

	    fun loop n =
	    (
		case TextIO.inputLine strm of
		  "" => n
		| _  => loop (n+1)
	    )
	in
	    loop 0
	end

    in
	case TextIOReader.openIt' file of
	  NONE => (fail (concat["count_lines couldn't open ", file]); 0)

	| SOME h => (run h) before (TextIOReader.closeIt h)
    end


(*------------------------------------------------------------------------------*)

    (*	Open a large number of files and check that some opens stay blocked
	until files are closed.
    *)


    and test2() =
    let
	val file = "sample1"

	(*  We have a server that counts successful opens.  *)
	datatype CntMsg = CntOpen | CntReport of int MLton.PCML.chan

	(*  An opener will open and hold files. *)
	datatype OpnMsg = OpnClose


	val cnt_chan: CntMsg MLton.PCML.chan = MLton.PCML.channel()


	fun counter() =
	let
	    fun loop num_open =
	    (
		case MLton.PCML.recv cnt_chan of
		  CntOpen => loop (num_open + 1)

		| CntReport rchan => (
		    MLton.PCML.send(rchan, num_open);
		    loop num_open
		    )
	    )
	in
	    loop 0
	end


	(*  This attempts to open a file and then waits for a message
	    to close it.  It reports the open when it succeeds.
	*)
	and opener n =
	let
	    val chan = MLton.PCML.channel()

	    fun run holder : unit =
	    let
		fun loop() =
		(
		    case MLton.PCML.recv chan of
		      OpnClose => ignore(TextIOReader.closeIt holder)
		)
	    in
		MLton.PCML.send(cnt_chan, CntOpen);
		loop()
	    end


	    and start() =
	    (
		case TextIOReader.openIt' file of
		  NONE =>
		    (
			fail (concat["test2, opener ", Int.toString n,
					": couldn't open ", file]);
			ignore(CML.recv chan)	(* wait for the OpnClose *)
		    )

		| SOME h => run h
	    )
	in
	    MLton.PCML.spawn start;
	    chan
	end



	and start_openers() =
	let
	    (*	This is the limit for the open manager. *)
	    val max_open = Int.div(SysWord.toInt(Posix.ProcEnv.sysconf "OPEN_MAX"), 2)

	    (*	Start all the openers with one extra which should block. *)
	    val openers = List.tabulate(max_open + 1, opener)

	    (*	Find out how many have started. *)
	    val started_1 = query_started()

	    val () = expectI max_open started_1 "test2 expect max_open"

	    (*	Check that we have 1 pending and the limit for the open files. *)
	    val () = check_counter max_open 1

	    (*	Tell one of the openers to close a file. The first in the 
		list should be one that has succeeded.
	    *)
	    val () = MLton.PCML.send(hd openers, OpnClose)

	    (*	One more should now have succeeded. *)
	    val started_2 = query_started()

	    val () = expectI (max_open+1) started_2 "test2 expect max_open+1"
	    val () = check_counter max_open 0

	    (*  Close all of the remaining files. *)
	    val () = app (fn ch => MLton.PCML.send(ch, OpnClose)) (tl openers)

	    (*	There should be zero opened. *)
	    val () = (delay 1; check_counter 0 0)
	in
	    ()
	end


	and query_started() =
	let
	    val ichan: int MLton.PCML.chan = MLton.PCML.channel()
	in
	    delay 1;
	    MLton.PCML.send(cnt_chan, CntReport ichan);
	    MLton.PCML.recv ichan
	end
    in
	MLton.PCML.spawn counter;
	start_openers();
	()
    end



    and delay n = MLton.PCML.sync(CML.timeOutEvt(Time.fromSeconds(LargeInt.fromInt n)))


    (*  Check that the counter matches with n opens. *)
    and check_counter nopen pending =
    let
	val (open_1, pending_1) = Ctr.stats()
    in
	expectI nopen   open_1    "test2 expect open_1";
	expectI pending pending_1 "test2 expect pending_1"
    end


(*------------------------------------------------------------------------------*)

    (* This test the retry.  A client pretends to fail the first time.
    *)
    and test3() =
    let
	val file = "sample1"
	val h1 = valOf(TextIOReader.openIt' file)
	val do_retry = ref true

	val did_retry = ref false
	val did_succ  = ref false


	fun client() =
	let
	    val schan = MLton.PCML.channel()

	    fun try() =
	    let
		val (alloc, rchan) = MLton.PCML.recv schan
	    in
		if !do_retry
		then
		(
		    MLton.PCML.send(rchan, Ctr.Retry alloc);
		    did_retry := true;
		    try()
		)
		else
		(
		    MLton.PCML.send(rchan, Ctr.Success);
		    Ctr.release alloc;
		    did_succ := true
		)
	    end
	in
	    Ctr.request(1, schan);
	    try()
	end


    in
	MLton.PCML.spawn client;
	delay 1;

	(*  Now closing h1 should trigger retry in the client. *)
	do_retry := false;
	TextIOReader.closeIt h1;
	delay 1;

	if !did_retry then () else fail "test3 did_retry";
	if !did_succ  then () else fail "test3 did_succ";

	()
    end

(*------------------------------------------------------------------------------*)


    fun run() =
    (
	print "Testing the Open Manager\n";

	(*  Show the too many open files message. *)
	Log.lowerLevel Log.Warn;

	test1();
	test2();
	test3();

	()
    )

(*------------------------------------------------------------------------------*)

end
