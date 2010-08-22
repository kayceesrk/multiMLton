(*
Copyright (c) Ray Racine

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
*)

(*  This contains the logging facility.  It can write to stderr if
    * no logging file has been specified.

    * The facility maintains a buffer of log messages that are written to
    * the log file concurrently. This avoids holding up the server to log
    * messages and avoids races.  I'll use a Mailbox for the buffer and
    * not worry about it being unbounded.

    * I'll have to flush the log file periodically get the messages out
    * promptly.  A one second flush interval should be fine. *)

signature LOG =
sig

    type Level

    val Fatal: Level
    val Error: Level
    val Warn:  Level
    val Info:  Level
    val Debug: Level

    (* This writes a logging message.
     * Error messages are counted. *)
    val	log: Level -> TextFrag.t -> unit

    (* This writes a logging message related to some source file position.
     * Error messages are counted. *)
    val	logP: Level -> Common.SrcPos -> TextFrag.t -> unit

    (*	Shorthands. *)
    val	fatal:  string list -> unit
    val	error:	string list -> unit
    val	errorP:	Common.SrcPos -> string list -> unit

    (* This is for informational messages where we don't want to
     * evaluate the message much if the message won't be logged. So
     * we delay the evaluation with a function wrapper. *)

    val inform:	    Level -> ( unit -> TextFrag.t ) -> unit
    val testInform: int -> Level -> ( unit -> TextFrag.t ) -> unit

    (* This returns the count of error messages seen so far. *)
    val	numErrors:  unit -> int

    (* This waits until the log messages have drained. *)
    val flush:	    unit -> unit

    (* Set the file for error logging.  This is currently only
     * allowed to be set once.*)
    val setLogFile: string -> unit

    (* Set the level for error logging. *)
    val setLevel:   Level -> unit

    (* Set the level for error logging to be at least as low as the
     * given level. *)
    val lowerLevel:   Level -> unit

    (* Describe the level for printing. *)
    val formatLevel:	Level -> string

    (*	Log any kind of exception.
     * This is guaranteed not to raise any exception. *)
    val logExn:  exn -> unit

    (*	Log with some extra information e.g. a file name. *)
    val logExnArg:  string -> exn -> unit

end

structure Log : LOG =
struct

  structure Sy = SyncVar
  structure TF = TextFrag
  structure C = Common

  type Level = int

  (* Higher levels are more important. *)
  val Fatal: Level	= 8
  val Error: Level	= 7
  val Warn:  Level	= 6
  val Info:  Level	= 5
  val Debug: Level	= 4

  (* This is the current log level. *)
  val log_level = ref Error

  fun formatLevel 8 = "Fatal"
    | formatLevel 7 = "Error"
    | formatLevel 6 = "Warning"
    | formatLevel 5 = "Info"
    | formatLevel 4 = "Debug"
    | formatLevel _ = "?"

  (*	The internal logging protocol. *)
  datatype LogRequest =
	   ReqLog of Level * TF.t * CML.thread_id * Time.time
	 | ReqSetFile of string
	 | ReqNumErrors of int Sy.ivar
	 | ReqFlush of unit Sy.ivar	    (* flush with acknowledge *)

  fun logServer mbox () =
      (*  An imperative state will be OK in this small context. *)
      let val num_errors: int ref = ref 0
	  val log_strm  = ref TextIO.stdErr
	  val log_file  = ref ""

	  fun updateCounts level =
	      if level >= Error
	      then num_errors := !num_errors + 1
	      else ()

	  fun setLogFile file =
	      ( log_strm := MLton.Thread.atomically ( fn () => TextIO.openAppend file );
		log_file := file )
	      handle IO.Io {name, function, cause} =>
		     Common.printToErr ( concat [ "IO Error ", name, " ", function, " ",
						  exnMessage cause, "\n" ] )

	  fun internalLog level msg tid when =
	      let fun put s = MLton.Thread.atomically ( fn () => TextIO.output ( !log_strm, s ) )
		  val date  = Date.fromTimeLocal ( when )
		  val fdate = Date.fmt "%Y %b %d %H:%M:%S" date
		  val stid = CML.tidToString tid
	      in
		  put ( concat [ fdate, " ", stid, " ", formatLevel level, ": " ] );
		  put "\t";
		  put ( TF.toString msg );
		  put "\n";
		  updateCounts level
	      end

	  fun handleMsg msg =
	      case msg of
		  ReqLog ( level, msg, tid, time ) =>  internalLog level msg tid time
		| ReqSetFile file => (MLton.Thread.atomically (fn () => print ("Opening log file " ^ file ^ "\n"));
				      if !log_file = ""
				      then setLogFile file
				      else internalLog Error
						       ( TF.str "Attempted to set the log file twice" )
						       ( CML.getTid () )
						       ( Time.now () ) )
		| ReqNumErrors rvar => Sy.iPut ( rvar, !num_errors )
		| ReqFlush rvar => Sy.iPut ( rvar, () )

	  fun loop () =
	      let
            fun timeout () = MLton.Thread.atomically ( fn () => TextIO.flushOut ( !log_strm ))
	      in
		  CML.select [ CML.wrap ( Mailbox.recvEvt mbox, handleMsg ),
			       CML.wrap ( CML.timeOutEvt ( Time.fromSeconds 3 ), timeout ) ];
		  loop ()
	      end

      in
	  loop ()
      end

  structure Logger = Singleton ( type input = LogRequest Mailbox.mbox
                                 val  newInput = Mailbox.mailbox
			         val  object   = logServer )

  fun sendRequest req = Mailbox.send ( Logger.get (), req )

  fun flush () =
      let val rvar = Sy.iVar()
      in
	  sendRequest (ReqFlush rvar);
	  Sy.iGet rvar
      end

  fun log level (msg: TF.t) =
      ( if level >= (!log_level) orelse level = Fatal (* speed up check *)
	then sendRequest ( ReqLog ( level, msg, CML.getTid ( ), Time.now () ) )
	else ();

	if level = Fatal
	then ( flush();
	       Common.printToErr ( concat [ formatLevel level, ": ",
	    				    TF.toString msg, "\n" ] );
	       Common.fail () ) (* abandon execution *)
	  else () )

  (* This writes a logging message related to some source file position.
   * Error messages are counted. *)
  fun logP level pos msg = log level ( TF.seq [ TF.str (C.formatPos pos), TF.str ": ", msg ])

  fun fatal msg  = log Fatal ( TF.concat msg )
  fun error msg  = log Error ( TF.concat msg )
  fun errorP pos msg = logP Error pos ( TF.concat msg )

  fun inform level msg_func =
      if level >= ( !log_level )	(* speed up the check *)
      then sendRequest ( ReqLog ( level, msg_func (), CML.getTid (), Time.now () ) )
      else ()

  fun testInform test level msg_func =
      ( if Globals.testing test
	then inform level msg_func
	else () )

  (* This returns the count of error messages seen so far. *)
  fun numErrors () =
      let val rvar = Sy.iVar ()
      in
	  sendRequest ( ReqNumErrors rvar );
	  Sy.iGet rvar
      end

  fun setLogFile file = sendRequest ( ReqSetFile file )

  fun setLevel   level = log_level := level
  fun lowerLevel level = log_level := Int.min ( level, !log_level )

  (* Format the error from an OS.SysErr exception in a standard way.
   * Fatal and InternalErrors always abort the server. *)
  fun logSysErr arg ( msg: string, _: OS.syserror option ) = error [ arg, " ", msg ]

  fun log_any x = log Error  (TF.concat [ exnName x, ": ", exnMessage x ] )

  fun logExnArg arg x =
      ( case x of
	    OS.SysErr x         => logSysErr arg x
	  | IO.Io { cause, ... } => logExnArg arg cause
	  | C.InternalError msg => log Fatal ( TF.concat [ "Internal Error: ", msg ] )
	  | C.FatalX            => log Fatal ( TF.str "Fatal Error" )
	  | x                   => log_any x )
      handle _ => ()		(* don't raise any more exceptions *)

  fun logExn x = logExnArg "" x

end
