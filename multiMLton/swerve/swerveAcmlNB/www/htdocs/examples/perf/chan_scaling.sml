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


(*  This measures some channel communication times.

*)

structure Main =
struct
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    (*	The number of thousands of threads to spawn. *)
    val M = 5

    fun toTime n = Time.fromSeconds(LargeInt.fromInt n)

    (*----------------------------------------------------------*)

    datatype Msg = Msg of Time.time	(* time when the send started *)


    (*	This records a list of receiver data so that the receiver can
	run as fast as possible.
    *)

    type RxEntry = int * Time.time * Time.time

    val rx_data: RxEntry list ref = ref []

    fun report_rx() =
    let
	fun f (n, at, diff) =
	let
	in
	    print(concat["Pair ", Int.toString n, " receives at ",
		    Time.fmt 6 at, 
		    " after ",
		    LargeInt.toString(Time.toMicroseconds diff), "\n"])
	end
    in
	app f (rev(!rx_data))
    end

    (*----------------------------------------------------------*)

    fun receiver n chan () =
    let
	fun report (Msg start) =
	let
	    val diff = Time.-(Time.now(), start)
	in
	    rx_data := (n, start, diff) :: (!rx_data)
	end
    in
	(* MLton.PCML.sync(CML.wrap(CML.recvEvt chan, report)) *)
	report(CML.recv chan)
    end
    handle x => toErr(concat["Uncaught exception: ", exnMessage x,
    			     " from thread ", Int.toString n, "\n"])



    (*	A message on the trigger channel starts the send on chan. *)
    fun sender n delay chan () =
    let
	fun report() = print(concat["Pair ", Int.toString n, " finishes\n"])
    in
	MLton.PCML.sync(delay);
	MLton.PCML.send(chan, Msg(Time.now()))
    end



    (*	This models the behaviour of the web server which creates a time-out
	event and then spawns a thread to wait for it.
    *)
    fun spawner n =
    let
	(*  Make a list of pairs of id and channel. *)
	val pairs = List.tabulate(n, fn n => (n, MLton.PCML.channel()))

	(*  One timeout event will release all waiters. *)
	val delay = MLton.PCML.atTimeEvt(Time.+(Time.now(), toTime 5))

	(*  Start a receiver and sender on each channel. *)
	fun startRx (n, ch) = 
	let
	    val rname = concat["Rx ", Int.toString n]
	    val sname = concat["Sn ", Int.toString n]
	in
	    Timing.timeIt rname (fn() => ignore(CML.spawn (receiver n ch)));
	    Timing.timeIt sname (fn() => ignore(CML.spawn (sender n delay ch)));
	    ()
	end

    in
	app startRx pairs
    end


    fun run argv () =
    let
	val (num, a1) = 
	    case argv of
	      []       => (20, [])
	    | (arg::r) => (valOf(Int.fromString arg), r)

	fun finish when = 
	let
	in
	    report_rx();
	    Timing.report()
	end
    in
	ignore(RunCML.addCleaner("report", [RunCML.AtShutdown], finish));
	spawner num
    end



    fun main(arg0, argv) =
    let
    in
	MLton.RunPCML.doit(run argv, NONE);
        OS.Process.success
    end
    handle
      x =>
    (
	toErr(concat["Uncaught exception: ", exnMessage x, " from\n"]);
	app (fn s => (print "\t"; print s; print "\n")) (SMLofNJ.exnHistory x);
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("chan_scaling", main)
end


