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

(* This describes a HTTP entity that is passed around and stored.

 * An entity has header information: type and encoding.  It has producers
 * and consumers. It can be generated lazily by a producer so the length
 * may not be known by the producer. The consumer will have to count
 * the size if it cares.

 * A file on disk will be represented as a producer of an entity so
 * there will be a need for concurrent production to different consumers.
 *)

(*
signature ENTITY =
sig

    structure Enc: ENCODING
    structure Mime: MIME_TYPE

    datatype info = Info of {
    	     etype: Mime.t option,
	     encoding: Enc.t option,
	     length: int option,
	     last_mod: Date.date option }

    (* A producer sends messages of this type to its consumer. *)
    datatype xferProto =
	     XferInfo  of Info	    	     (* send this first *)
	   | XferBytes of Word8Vector.vector (* then lots of these *)
	   | XferDone			     (* then one of these *)
	   | XferAbort			     (* or else one of these *)

    (*	The MKProducer function must start a thread that sends the entity
     * info and body to the consumer. The entity body is local to the
     * producer function.

     * The length is recalculated from the frag or file.  If a file
     * cannot be read then an error is logged and an empty entity
     * is sent.

     * The event will be enabled if the transfer is to be aborted.
     * The XferAbort will be sent instead of the XferDone in this case.

     * A thread id is returned so that the caller can wait until the producer
     * has finished. *)

    type consumer = XferProto MLton.PCML.chan
    type mkproducer = Abort.Abort -> info -> consumer -> MLton.PCML.thread_id

    datatype entity =
	     Entity of { info: info,
			 body: mkproducer }
	   | None

    (*	This creates a producer for an entity. *)
    val startProducer:	Abort.Abort -> entity -> consumer -> MLton.PCML.thread_id

    (* This is useful for a file producer which will fill in all of
     * the fields from the file. *)
    val emptyInfo: info

    (*	These make producers for particular kinds of sources.
     * The lines of fragments are considered to be separated by CRLF.
     * The length of the entity is defined by the body. The info length
     * is only used in the xfer protocol. *)

    val textProducer: TextFrag.Text -> mkproducer
    val tmpProducer:  TmpFile.TmpFile -> mkproducer
    val fileProducer: string -> mkproducer

    (*	Beware that process producers are one-shot.
     * The holder is closed after the entity has been produced. *)
    val procProducer:	ExecReader.Holder -> mkproducer

end
*)

structure Entity (* : ENTITY *) =
struct

    structure TF = TextFrag
    structure IETFLine = IETFLine
    structure Enc = Encoding
    structure MType = MimeType
    structure Info = Info

    (*	A producer sends messages of this type to its consumer.*)
    datatype XferProto =
	     XferInfo  of Info.t    	       (* send this first *)
	   | XferBytes of Word8Vector.vector   (* then lots of these *)
	   | XferDone			       (* then one of these *)
	   | XferAbort  		       (* or else one of these *)

    (* The MKProducer function must start a thread that sends the entity
     * info and body to the consumer. The entity body is local to the
     * producer function.  The length should be defined by the producer
     * rather than the entity info. *)
    type consumer = XferProto MLton.PCML.chan
    type mkproducer = Abort.t -> Info.t -> consumer -> MLton.PCML.thread_id

    datatype entity =
	     Entity of { info: Info.t,
			 body: mkproducer }
	   | None

    (*	For entity transfers. *)
    val file_chunk = 8192
    val pipe_chunk = 4096

    (*	Work out the mime type and make a header for it.
     * We'll use text/plain as the default which is OK
     * for most Unix files.  Apache tries harder by acting
     * like the Unix 'file' command. *)
    fun determineMimeType path =
	case Files.splitExt path of
	    (_, SOME ext) => (case Config.lookupMime ext of
				  NONE            => MType.simpleType "text" "plain"
				| SOME (maj, min) => MType.simpleType maj min)
	  | _ => MType.simpleType "text" "plain"


    fun startProducer abort entity consumer =
	(
     case entity of
	    NONE => MLton.PCML.spawn (fn () => MLton.PCML.send ( consumer, XferDone ))
	  | SOME ( Entity { info, body } ) => body abort info consumer
	  | SOME None => MLton.PCML.spawn (fn () => MLton.PCML.send (consumer, XferDone )))
    (* RPR - Added this as all cases not covered.  Do we need "None" in DT?? *)

    (*	This creates a producer for a TextFrag. *)
    fun textProducer frag abort einfo consumer =
	let val len = (Int64.fromInt ( TF.length frag ))
	    fun send str = MLton.PCML.send ( consumer, XferBytes ( Byte.stringToBytes str ) )
	    fun producer() = ( MLton.PCML.send ( consumer,
					  XferInfo ( Info.updateLength einfo len ) );
			       TF.app send frag;
			       MLton.PCML.send ( consumer, XferDone ) )
	in
	    MLton.PCML.spawn producer
	end


    structure NB = MLton.PCML.NonBlocking
    val proc = valOf (NB.createProcessor ())

    (*	There is no inputNEvt() available so we can only poll once
     * the transfer has started.*)
    fun fileProducer name abort old_info consumer =
	let fun sendFile(name) =
		let
          val arIn = MLton.PCML.channel ()
          val arOut = MLton.PCML.channel ()
          fun arbitrator () = MLton.PCML.select[
                MLton.PCML.wrap (MLton.PCML.recvEvt arIn,
                        fn chunk => MLton.PCML.send (consumer, chunk)),
                MLton.PCML.wrap (Abort.evt abort,
                        fn () => (MLton.PCML.aSync (MLton.PCML.aSendEvt (arOut, ()));
                                  MLton.PCML.send (consumer, XferAbort)))]
          fun sendChunk (chunk) =
            MLton.PCML.aSync (MLton.PCML.aWrap (MLton.PCML.aSendEvt (arIn, XferBytes chunk), arbitrator))

		  fun loop strm =
		    let
              val chunk = NB.executeOn proc (fn () => BinIO.inputN(strm, file_chunk))
			in
				 if Word8Vector.length chunk = 0
				 then (MLton.PCML.send(consumer, XferDone))
				 else (MLton.PCML.select [
                        MLton.PCML.recvEvt arOut,
                        MLton.PCML.wrap (MLton.PCML.alwaysEvt (),
                                fn () => (sendChunk (chunk);
                                          loop strm))])
			end
          val _ = MLton.PCML.aSync (MLton.PCML.aWrap (MLton.PCML.aRecvEvt (arIn),
                                                      fn chunk => MLton.PCML.send (consumer, chunk)))
		in
		    case BinIOReader.openIt abort name of
			NONE   => ()
		      | SOME h => let
                            val _ = loop (BinIOReader.get h)
                            val _ = BinIOReader.closeIt h
                          in
                            ()
                          end
		end handle x => (Log.logExn x; ())

	    fun producer() =
		(* All of the info fields are regenerated from the
		 * file at the time we send it.*)
		let val opt_len = FileIO.fileSize name
		    val modt = Option.map Date.fromTimeUniv ( FileIO.modTime name )

		    val info = Info.Info { etype     = SOME ( determineMimeType name ),
					   encoding  = NONE,
					   length    = opt_len,
					   last_mod  = modt }
		in
		    MLton.PCML.send ( consumer, XferInfo info );
		    (* NONE if can't access the file *)
		    case opt_len of
			NONE     => MLton.PCML.send ( consumer, XferDone )
		      | SOME len => sendFile(name)
		end
	in
	    MLton.PCML.spawn producer
	end

    fun tmpProducer tmpfile = fileProducer (TmpFile.getName tmpfile)

    (*	The length is taken from the info.*)
    fun procProducer (holder: ExecReader.holder) abort einfo consumer =
	let val opened as (proc, _) = ExecReader.get holder
	    val (strm, _) = Unix.streamsOf proc

	    fun sendFile () =
		(* See sendFile above
		 * PROBLEM: MLton.PCML timeouts don't seem to interrupt the inputN
		 * operation. *)
		if Abort.aborted abort
		then MLton.PCML.send ( consumer, XferAbort )
		else let val chunk = TextIO.inputN (strm, pipe_chunk )
		     in
			 if chunk = ""
			 then MLton.PCML.send( consumer, XferDone )
			 else ( MLton.PCML.send(consumer, XferBytes(Byte.stringToBytes chunk ) );
				sendFile () )
		     end
		 handle x => ( Log.logExn x; () )

	    fun producer() = ( MLton.PCML.send(consumer, XferInfo einfo );
			       sendFile ();
			       ExecReader.closeIt holder;
			       () )

	in
	    MLton.PCML.spawn producer
	end

end
