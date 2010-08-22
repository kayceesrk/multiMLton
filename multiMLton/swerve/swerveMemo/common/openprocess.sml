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


structure Reap =
struct

val reapChan: Posix.Process.exit_status CML.chan = CML.channel ()

fun reapEvt _ = CML.recvEvt reapChan

end

(*  Read from child processes. *)

local
    structure E  = Posix.Error
    structure PP = Posix.Process

    structure Impl =
    struct

      val name = "ExecReader"
      type arg = string * string list * string list
      type opened = ( TextIO.instream, TextIO.outstream ) Unix.proc * string (* includes the script path *)
      type closed = int		    (* the exit status *)

      (* Four are used temporarily while opening and then 2 are closed
       * but we don't bother releasing 2. *)
      val num_fds = 4

      datatype result =
	       Success of opened
	     | Fail
	     | Retry

      val bad_status = 128

      fun openIt ( args as ( path, _, _ ) ) =
	  (
	   Success ( Unix.executeInEnv args, path )
	  ) handle x as IO.Io { cause = OS.SysErr ( _, SOME err ), ... } =>
		   (
		    if err = E.mfile orelse err = E.nfile
		    then Retry
		    else (
			Log.logExn x;	(* a real error *)
			Fail
			)
		   )
		 | x => ( Log.logExn x; Fail )

      fun reaped proc path exit_status =
	  case exit_status of
	      PP.W_EXITED => 0

	    | PP.W_EXITSTATUS w =>
	      let val ex = Word8.toInt w
	      in
		  Log.error [ "Child process ", path,
			      " exited with ", Int.toString ex ];
		  ex
	      end

	    | PP.W_SIGNALED signal =>
	      (
	       Log.error ["Child process ", path, " exited with signal ",
			  SysWord.toString(Posix.Signal.toWord signal)];
	       bad_status
	      )

	    | PP.W_STOPPED signal =>
	      (
	       Log.error [ "Child process ", path, " stopped with signal ",
			   SysWord.toString ( Posix.Signal.toWord signal ) ];
	       Unix.kill ( proc, Posix.Signal.kill );
	       bad_status
	      )


      (* This will wait for up to a second for the child to terminate.
       * If it doesn't it kills it and returns.  The reap will happen
       * asynchronously.
       * Errors are logged. *)
      fun closeIt ( proc, path ) =
	  let fun timeout() =
		  let val () = Unix.kill ( proc, Posix.Signal.kill )
		  in
		      Log.error [ "Killing stuck child process ", path ];
		      ignore ( CML.spawn ( fn () => ignore ( Unix.reap proc ) ) );
		      bad_status
		  end
	  in

	      (* Roll my own Unix.reapEvt by creating a channel and have "reaped" send to it. *)
          print "\nopen process";
	      CML.select [ CML.wrap ( Reap.reapEvt proc, reaped proc path ),  (* RPR was Unix.reapEvt proc, reaped ... *)
			   CML.wrap ( CML.timeOutEvt ( Time.fromSeconds 1 ), timeout ) ]
	  end handle x => ( Log.logExn x; bad_status )
    end
in
  structure ExecReader = OpenMgrFn ( structure Impl = Impl )
end

