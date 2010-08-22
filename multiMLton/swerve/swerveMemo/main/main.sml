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

(*  This is main for the swerve server. *)



structure Main =
struct

  structure TF = TextFrag

  fun run() =
      (
       (* TraceCML.setUncaughtFn fatal_exception;  *)
       (* TraceCML.setTraceFile TraceCML.TraceToErr; *)

       Log.inform Log.Debug (fn () => TF.str "Starting up.");

       StartUp.startup();

       Log.inform Log.Debug (fn () => TF.str "Started.");

       Log.inform Log.Debug (fn () => TF.str "Starting listener.");
       Listener.run();


       Common.success()		(* shouldn't get here *)
      )


  (*	This reports on uncaught exceptions out of threads. *)
  and fatal_exception (thread, x) =
      let
	  fun describe() =
	      (
	       case x of
		   IO.Io {name, function, cause} =>
		   Common.printToErr(concat["IO Error ", name, " ", function, " ", exnMessage cause])

		 | FatalX => ()

(*		 | Common.InternalError msg => Common.printToErr ( concat [ "Internal error, ", msg ] ) *)

		 (* misc exception *)
		 | x => Common.printToErr ( concat [ "Uncaught exception ", exnName x, ": ",exnMessage x ] )
	      )
      in
	  describe();
	  Common.printToErr ( concat [ " in thread ", CML.tidToString thread, "\n" ] );
	  Common.fail()
      end

    fun usage () = (
	print "Usage: swerve [options]\n";
	print "Options:\n";
	print "  -f file     : (required) specify the server configuration file\n";
	print "  -h          : print this help message\n";
	print "  -T id       : enable test messages according to the id\n";
	print "  -D level    : set the logging level to Debug immediately\n";
	()
    )

    (*	Check for command line options. *)
    fun process_args(arglist: string list) : unit =
	let fun loop [] = (Log.inform Log.Debug (fn () => TF.str "No more Args.  Starting listener.\n"; run () ) )			(* used up all the args *)
	      | loop (arg::rest) =
		(
		 Log.inform Log.Debug (fn () => TF.concat ["Arg ", arg ]);
		 case arg of
		     "-T"	=> loop ( string_value ( arg, rest, Globals.set_testing ) )
		   | "-D"	=> (set_debugging(); loop rest)
		   | "-f"	=> loop ( string_value ( arg, rest, Config.processConfig ) )
		   | "-h"	=> ( usage(); Common.success () )
		   | _    	=> ( usage(); Common.fail () )
		)


	    (* Take one string as an arg and return the rest. *)
	    and string_value (name, [], _) =
		(
		 Common.printToErr (concat["Option ", name, " must have a value\n"]);
		 []
		)
	      |   string_value (name, arg::rest, func: string -> unit) =
		  (
		   Log.inform Log.Debug (fn () => TF.concat [ "Processing arg ", name, " with value ", arg, "."]);
		   func(arg);
		   rest
		  )

	    (* Set debugging messages on so that we can see stuff during config file
	     * parsing. *)
	    and set_debugging() = Log.setLevel Log.Debug
	in
	    Log.inform Log.Debug ( fn () => TF.str "Processing Args" );
	    loop arglist;
	    Log.inform Log.Debug ( fn () => TF.str "Finished Processing Args" )
	end
	handle x => fatal_exception(CML.getTid(), x)

    fun main (argv) =
	let
	    fun run() =
		(
		 Log.setLevel Log.Debug;
		 Log.inform Log.Debug (fn () => TF.str "Init SignalMgr");
		 SignalMgr.init();	(* required by OpenMgr *)
		 Log.inform Log.Debug (fn () => TF.str "Done Init SignalMgr");
		 process_args argv
		)
	in
	    RunCML.doit ( run, NONE)
(* RPR	    StartUp.finish () *)
	end

end

val _ = Main.main ( CommandLine.arguments () )

