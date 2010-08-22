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


(*  This manages UNIX signals.

 * We provide two multicast channels. The first will carry SIGINT
 * and SIGTERM signals. The second will carry the sigGC signal that
 * indicates a GC has happened.  The open file manager wants GC signals. *)

signature SIGNAL_MGR =
sig

    (*	Each client must have its own port. *)
    type GcPort
    type IntPort

    datatype Interrupt = SIGINT | SIGTERM

    (*	This sets up the signal handling. *)
    val init:	unit -> unit

    (*	Create a new client port. *)
    val mkGcPort:   unit -> GcPort
    val mkIntPort:  unit -> IntPort

    (* This creates an event for the arrival of the
     * next GC signal. Call it anew for each GC. *)
    val gcEvt:	GcPort -> unit MLton.PCML.Multicast.event

    (* This creates an event for the arrival of the
     * next interrupting signal. *)
    val intEvt:	IntPort -> Interrupt MLton.PCML.Multicast.event

end


structure SignalMgr : SIGNAL_MGR =
struct
  open Common

  structure TF  = TextFrag
  structure Sig = MLton.Signal

  datatype Interrupt = SIGINT | SIGTERM

  type GcPort  = unit MLton.PCML.Multicast.port
  type IntPort = Interrupt MLton.PCML.Multicast.port

  val gc_mchan: unit MLton.PCML.Multicast.mchan option ref = ref NONE
  val int_mchan: Interrupt MLton.PCML.Multicast.mchan option ref = ref NONE

  (* FIXME RPR Do we need this?  Does MLton have one? *)
  (*    fun gcHandler thread =
	    (
	     Log.testInform Globals.TestTiming Log.Debug
			    ( fn () => TF.str "GC signalled" );
	     MLton.PCML.Multicast.multicast ( valOf ( !gc_mchan ), () );
	     thread
	    ) *)

  (* REVISIT - shutdown until we get a real use for this. *)
  fun intHandler thread =
      (
       fail ();
       MLton.PCML.Multicast.multicast ( valOf ( !int_mchan ), SIGINT );
       thread
      )

  fun termHandler thread =
      (
       fail ();
       MLton.PCML.Multicast.multicast ( valOf ( !int_mchan ), SIGTERM );
       thread
      )

  fun pipeHandler thread =
      (
       Log.error ["SIGPIPE in SignalMgr"];
       thread
      )

  fun init () =
      (
       gc_mchan  := SOME ( MLton.PCML.Multicast.mChannel () );
       int_mchan := SOME ( MLton.PCML.Multicast.mChannel () );

       (* FIXME RPR Sig.setHandler ( Sig.sigGC, Sig.handler gc_handler ); *)
       Sig.setHandler ( Posix.Signal.int,  Sig.Handler.handler intHandler  );
       Sig.setHandler ( Posix.Signal.term, Sig.Handler.handler intHandler  );
       Sig.setHandler ( Posix.Signal.pipe, Sig.Handler.handler pipeHandler )
      )

  fun mkGcPort() = MLton.PCML.Multicast.port  ( valOf ( !gc_mchan ) )
  fun mkIntPort() = MLton.PCML.Multicast.port ( valOf ( !int_mchan ) )

  fun gcEvt port  = MLton.PCML.Multicast.recvEvt port
  fun intEvt port = MLton.PCML.Multicast.recvEvt port

end
