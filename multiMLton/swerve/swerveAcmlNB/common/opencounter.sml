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


structure OpenCounter: OPEN_COUNTER =
struct
    open Common

    structure Sy = MLton.PCML.SyncVar
    structure TF = TextFrag
    structure G  = Globals

    datatype allocation = Allocation of int

    datatype response =
	     Success
	   | Fail of allocation
	   | Retry of allocation

    type start = allocation * response MLton.PCML.chan

    (*	The protocol for the counter object. *)
    datatype ctrmsg =
	     CtrRequest of int * start MLton.PCML.chan
	   | CtrRelease of int
	   | CtrStats   of (int * int) Sy.ivar 	(* num open, num pending *)

    datatype pending = Pending of
	     { schan:	start MLton.PCML.chan,	(* where to restart the open *)
	       alloc:	allocation }	(* how much we allocated originally *)

    (* We only put out one of warning per second to avoid flooding
     * the log. At the same time we trigger a major collection in the
     * hope of collecting some lost open files via finalisation. *)
    datatype state = State of
    	     { inUse    : int,
	       pending  : pending list,
	       lastWarn : Time.time option }    (* time of the last warning *)

    (* functional record update *)
    fun << z =
	let fun r t = { inUse = #1 t, pending = #2 t, lastWarn = #3 t }
	    fun t { inUse = x1, pending = x2, lastWarn = x3 } = ( x1, x2, x3 )
	in
	    Fru.fru3 (r, r, t) z
	end

    infix <<

    fun incInUse (State state) n =
	State (state << ( #inUse, n ))

    fun formatState ( State { inUse, pending, lastWarn } ) =
	TF.concat [ "inUse=", Int.toString inUse,
		    " pending len=", Int.toString ( length pending ) ]

    (*  This will run the finalisation which will queue up
     * release messages to the counter. To be safe against
     * deadlock the GC is triggered from a separate thread. *)
    fun warnMax ( State st ) =
	let val now = Time.now ()
	    val { lastWarn, ... } = st
	in
	     if lastWarn = NONE orelse
		Time.toMilliseconds(Time.-(now, valOf(lastWarn))) >= 1000
	     then ( ignore ( MLton.PCML.spawn( fn () => (
					   Log.log Log.Warn ( TF.str "OpenMgr: Too many open files" );
                       print "\nToo many open files.. invoking GC.collect ()";
					   MLton.GC.collect ()) ) ) )
	     else ();
	    State ( st << ( #lastWarn, SOME now ) )
	end

    (* Queue a request to retry later after more file descriptors
     * are released.  No matter what the size of the allocation, a release
     * of a single descriptor may be enough to make a request succeed. *)
    fun defer ( state as State { pending, ... } ) alloc startChan =
	let val p = Pending { schan = startChan,
			      alloc = alloc }
	    val State s = state
	in
	    State ( s << ( #pending, p::pending ))
	end

    val maxOpen = Int.div ( SysWord.toInt ( Posix.ProcEnv.sysconf "OPEN_MAX" ), 1)

    fun server chan () =
	let val _ = Log.setLevel Log.Debug
	    val _ = Log.inform Log.Debug ( fn ()=>
						TF.concat [ "The open file limit will be ",
							    Int.toString maxOpen ] );

	    (* Since we only handle one open transaction at a time we just have
	     * the one response channel. *)
	    val respchan = MLton.PCML.channel()

	    (* The state must be immutable so that we can nicely recurse through
	     * runDefer. To be safe from deadlock the state transitions must
	     * proceed without any chance of an exception breaking them.  The
	     * client must be correct. *)
	    fun tryRequest ( state as State { inUse, ... } ) n startChan =
		let val alloc = Allocation n
		in
		    if n <= maxOpen - inUse
		    then ( MLton.PCML.send ( startChan, ( alloc, respchan ) );
			   case MLton.PCML.recv respchan of
			       Success => incInUse state n
			     | Fail _  => state		(* give up *)
			     | Retry alloc => defer state alloc startChan )
		    else warnMax ( defer state alloc startChan )
		end

	    (* Pending requests are processed in a round-robin fashion. *)
	    (* RPR FIXME - we can do better then a rev list impl here.  See Chris O. *)
	    fun runDefer ( state as State st ) =
		let val { pending, ... } = st
		in
		    case rev pending of
			[] => state
		      | ( p::rest ) => let val Pending { schan, alloc = Allocation n } = p
					   val newState = State ( st << ( #pending, rev rest ) )
				       in
					   tryRequest newState n schan
				       end
		end

	    fun loop state =
		let val _ = Log.inform Log.Debug (fn () => TF.str "Creating Server loop state.\n")
		    val state' =
			let val _ = Log.inform Log.Debug (fn () => TF.str "Reading server channel\n")
			    val msg = MLton.PCML.recv chan
			    val _ = Log.inform Log.Debug (fn () => TF.str "Got server channel msg.\n")
			in
			    case msg of
				CtrRequest ( n, startChan ) => ( Log.inform Log.Debug ( fn () => TF.str "CtrRequest\n" );
								 tryRequest state n startChan )
			      | CtrRelease n => ( Log.inform Log.Debug ( fn () => TF.concat [ "CtrRelease ", Int.toString n ] );
						  runDefer ( incInUse state ( ~n ) ) )
			      | CtrStats rvar => let val _ = Log.inform Log.Debug (fn () => TF.str "CtrStats\n" );
						     val State { inUse, pending, ... } = state
						 in
						     Sy.iPut( rvar, ( inUse, length pending ) );
						     state
						 end (* RPR handle Put exception ?? *)
			end
		in
		    loop state'
		end
	in
	    Log.setLevel Log.Debug;
	    Log.inform Log.Debug ( fn () => TF.str "Starting opencounter resource loop." );
	    loop (State {inUse = 0, pending = [], lastWarn = NONE })
	end

    structure Counter = Singleton ( type input    = ctrmsg MLton.PCML.chan
			            val  newInput = MLton.PCML.channel
			            val  object   = server)

    fun request ( n, schan ) = ( Log.inform Log.Debug (fn () => TF.concat ["Counter request: ", Int.toString n] );
				 MLton.PCML.send ( Counter.get (), CtrRequest ( n, schan ) ) )

    fun release ( Allocation n ) = ( Log.inform Log.Debug (fn () => TF.concat [ "Counter release: ", Int.toString n ] );
				     MLton.PCML.send ( Counter.get (), CtrRelease n ) )

    fun stats () =
	let val rvar = Sy.iVar ()
	in
	    Log.inform Log.Debug ( fn () => TF.str "Counter stats" );
	    MLton.PCML.send ( Counter.get (), CtrStats rvar );
	    Sy.iGet rvar
	end

end

