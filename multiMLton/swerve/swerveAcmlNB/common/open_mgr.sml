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

(*  This is a generic open manager that is specialised on the different kinds
 * of things that can be opened. *)

(*  This specialises the generic open manager for each openable object. *)

signature OPEN_MGR_IMPL =
sig
    val name: string

    type arg
    type opened
    type closed

    (*	This is the number of file descriptors that are needed
     * by the open. *)
    val	num_fds:    int

    datatype result =
	     Success of opened
	   | Fail		(* give up totally *)
	   | Retry		(* should try again later *)

    val	openIt:  arg -> result
    val closeIt: opened -> closed

end


signature OPEN_MGR =
sig
    structure Impl: OPEN_MGR_IMPL

    (*	This describes what can be opened or closed. *)
    type arg = Impl.arg

    (*	This represents an open object. *)
    type opened = Impl.opened

    (*	This is the type returned from a close operation. *)
    type closed = Impl.closed

    (* This is a holder for the object.  The object will be
     * finalised if the caller loses its reference to the
     * object. *)
    type holder

    val get: holder -> opened

    (* Open/close the object.
     * This will return NONE if the open failed or was aborted. *)
    val openIt:	    Abort.t -> arg -> holder option
    val openIt':    arg -> holder option
    val closeIt:    holder -> closed
end

functor OpenMgrFn ( structure Impl: OPEN_MGR_IMPL ) : OPEN_MGR =
struct

  open Common

  structure TF = TextFrag

  structure Ctr  = OpenCounter
  structure Impl = Impl

  structure Fin = FinaliseFn ( structure Type =
			       struct
				 type t = Impl.opened * Ctr.allocation
				 fun finalise ( opn, _ ) = ignore ( Impl.closeIt opn )
				 val name = Impl.name
			       end )


  type arg    = Impl.arg
  type opened = Impl.opened
  type closed = Impl.closed
  type holder = Fin.holder


  fun openIt abort arg =
      let
	  val schan = MLton.PCML.channel()

	  (*  We may have to try several times.

	   * To be safe from deadlock there must be no possibility
	   * of an exception preventing the state transitions from
	   * completing. Otherwise the counter will block forever.

	   * So when we abort we must leave a thread behind to finish
	   * the handshaking. Trying to remove the pending request from
	   * the counter risks race conditions. *)
	fun try() =
	    let fun got_alloc (alloc, rchan) =
		    (
		     case Impl.openIt arg of
			 Impl.Success opn => ( MLton.PCML.send(rchan, Ctr.Success );
					       SOME ( opn, alloc ) )

		       | Impl.Fail =>  ( MLton.PCML.send ( rchan, Ctr.Fail alloc ); NONE )

		       | Impl.Retry => ( MLton.PCML.send ( rchan, Ctr.Retry alloc ); try () )
		    ) handle _ => ( MLton.PCML.send ( rchan, Ctr.Fail alloc ); NONE )

		fun got_abort () =
		    let fun dummy () =
			    let val ( alloc, rchan ) = MLton.PCML.recv schan
			    in
				MLton.PCML.send ( rchan, Ctr.Fail alloc )
			    end
		    in
			MLton.PCML.spawn dummy;
			NONE
		    end
	    in
		MLton.PCML.select [ MLton.PCML.wrap ( MLton.PCML.recvEvt schan, got_alloc ),
			     MLton.PCML.wrap ( Abort.evt abort, got_abort ) ]
	    end
      in
	  (*  Start trying *)
	  Ctr.request (Impl.num_fds, schan);

	  (*  Once opened, set up a finaliser on the Opened value. *)
	  case try() of
	      NONE     => NONE
	    | SOME farg => SOME(Fin.add farg)
      end


    fun openIt' arg = openIt ( Abort.never() ) arg

    fun closeIt holder =
    let
	val ( opn, alloc ) = Fin.get holder
    in
	Fin.remove holder;
	( Impl.closeIt opn ) before ( Ctr.release alloc )
    end



    fun get holder = #1(Fin.get holder)

end

