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



structure Timeouts =
struct
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    fun delta() =
    let
	val tmevt = MLton.PCML.timeOutEvt (Time.fromSeconds 5)
    in
	print "Waiting for the timeout\n";
	MLton.PCML.sync tmevt;

	print "Delay\n";
	MLton.PCML.sync (CML.timeOutEvt (Time.fromSeconds 1));

	print "Waiting again for the timeout\n";
	MLton.PCML.sync tmevt;

	print "Done\n"
    end



    fun abs() =
    let
	val when = Time.+(Time.now(), Time.fromSeconds 5)
	val tmevt = MLton.PCML.atTimeEvt when
    in
	print "Waiting for the timeout\n";
	MLton.PCML.sync tmevt;

	print "Delay\n";
	MLton.PCML.sync (CML.timeOutEvt (Time.fromSeconds 2));

	print "Waiting again for the timeout\n";
	MLton.PCML.sync tmevt;

	print "Delay again\n";
	MLton.PCML.sync (CML.timeOutEvt (Time.fromSeconds 2));

	print "Waiting again for the timeout\n";
	MLton.PCML.sync tmevt;

	print "Done\n"
    end



    fun main(arg0, argv) =
    let
	fun run() =
	let
	in
	    case argv of
	      ["delta"] => delta()
	    | ["abs"]   => abs()
	    | _         => ()
	end
    in
	MLton.RunPCML.doit(run, NONE);
        OS.Process.success
    end
    handle
      x =>
    (
	toErr(concat["Uncaught exception: ", exnMessage x, " from\n"]);
	app (fn s => (print "\t"; print s; print "\n")) (SMLofNJ.exnHistory x);
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("timeouts", main)
end
