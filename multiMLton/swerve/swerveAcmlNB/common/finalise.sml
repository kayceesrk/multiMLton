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



(* FIXME RPR Removed all weak GC support.  Need to come back and fix this. *)

(* This manages objects that need to be finalised when the garbage
 * collector takes them.

 * It is generic over the different types of objects.

 * We maintain a list of weak references to the files.  This will be
 * scanned periodically to finalise them.

 * While the client holds onto an Holder the finaliser will not be
 * run.  The client must remove the object if it finalises it itself.
 * The references are given an integer key so that they can be removed
 * again later.  There is no other protection against both the client
 * and the manager both finalising the object. *)


signature FINALISE_TYPE =
sig
    type t

    val finalise:   t -> unit
    val name:	    string
end

signature FINALISER =
sig
    (*	This is the value that is shared between the client
     * and the manager. *)
    type holder

    (*	This is the value in the holder that will be finalised.  *)
    type t

    val get: holder -> t

    (*	This adds a new T to the manager.  *)
    val	add: t -> holder

    val remove:	holder -> unit
end



functor FinaliseFn ( structure Type: FINALISE_TYPE ): FINALISER =
struct

  structure Sy = MLton.PCML.SyncVar
  structure TF = TextFrag
  structure G  = Globals
  structure W  = MLton.Weak

  type t = Type.t

  type pair = int * t

  (* We use a ref on the Pairs to ensure that they are
   * copied by reference. *)
  type holder = pair ref

  fun key ( holder: holder ) = #1 ( !holder )
  fun get ( holder: holder ) = #2 ( !holder )

  datatype Req =
	   ReqAdd of t * holder Sy.ivar
	 | ReqRemove of holder

  (* When the holder is collected we should have the last
   * strong ref to T which we finalise. *)
  type Wref  = int * t * (holder W.t)

  (* This requires a linear scan of all held objects which
   * shouldn't be a performance problem since GCs are
   * infrequent. *)
  type State = int * Wref list

  fun finalise ( state as ( tag_cnt, wrefs ) ) () : State =
      let val _ = Log.testInform G.TestFinalise Log.Debug
				 ( fn () => TF.concat [ "Finaliser ", Type.name, ": finalising" ] )

	  (*  Test if this wref should be kept or finalised. *)
	  fun keep ( _, value, wref ) =
	      (
	       case W.get wref of
		   NONE   => ( Type.finalise value; false )
		 | SOME _ => true
	      )
      in
	  ( tag_cnt, List.filter keep wrefs )
      end

  fun handle_msg ( state as ( tag_cnt, wrefs ) ) msg : State =
      ( case msg of
	    ReqAdd (value, rvar) =>
	    let val _ = Log.testInform G.TestFinalise Log.Debug
				       ( fn () => TF.concat [ "Finaliser ", Type.name, ": add" ] )
		val key = tag_cnt
		val holder = ref ( key, value )
		val new_state = ( tag_cnt + 1, ( key, value, W.new holder ):: wrefs)
	    in
		Sy.iPut ( rvar, holder );
		new_state
	    end

	  | ReqRemove holder =>
	    let val _ = Log.testInform G.TestFinalise
				       Log.Debug
				       ( fn () => TF.concat [ "Finaliser ", Type.name, ": remove" ] )

		val k = key holder
		fun keep ( key, _, _ ) = k = key
	    in
		( tag_cnt, List.filter keep wrefs )
	    end
      )

  fun server chan () =
      let val gc_port = SignalMgr.mkGcPort ()
	  fun loop state =
	      loop ( MLton.PCML.select [ MLton.PCML.wrap ( MLton.PCML.recvEvt chan, handle_msg state ),
				  MLton.PCML.wrap ( SignalMgr.gcEvt gc_port, finalise state) ] )
      in
	  loop ( 0, [] )
      end





  structure Mgr = Singleton ( type input    = Req MLton.PCML.chan
                              val  newInput = MLton.PCML.channel
                              val  object   = server )

  fun add value =
      let val rvar = Sy.iVar()
      in
	  MLton.PCML.send(Mgr.get(), ReqAdd (value, rvar));
	  Sy.iGet rvar
      end

  fun remove holder = MLton.PCML.send ( Mgr.get (), ReqRemove holder )

end
