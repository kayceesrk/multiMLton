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


(* This manages the creation of temporary files which typically
 * hold the bodies of long requests.

 * Body files are distinguished by their local port numbers. These
 * numbers are unique only within the protocol family but we only use
 * the tcp protocol.  By using distinct numbers then we don't have to
 * try too hard to ensure that we get unique file names.

 * We use a manager thread to remember all of the body files and to
 * clean them up on a timeout. *)

signature TMPFILE =
sig

    type TmpFile

    (* Allocate a new file for the given port number.
     * If we give up trying to create the file then we return NONE. *)
    val newBodyFile: Abort.t -> string -> int -> int -> TmpFile option

    (* Get the file name. *)
    val getName:	TmpFile -> string

    (* This releases the files associated with the port number.
     * They will be deleted. *)
    val releasePort:	int -> unit

    (* This sets the temp file disk space limit. It must be
     * called before any temp files are created, preferably
     * from the config.  The size is in bytes.
     * The limit must be no larger than 10^9 bytes. *)
    val setDiskLimit:	int -> unit

end

structure TmpFile: TMPFILE =
struct

  structure Sy     = MLton.PCML.SyncVar
  structure TF     = TextFrag

  (*	The internal allocation protocol. *)

  datatype TmpFile = TmpFile of
	   { id: int,
	     port: int,
	     file: string,	(* absolute file path *)
	     len: int		(* a size estimate *)
	   }

       and AllocRequest =
	   Record of Pending
	 | Release of int		(* release all files on the port *)
	 | Undo of TmpFile		(* undo one allocation *)


       and Reply = Success of TmpFile | Reject

       (* This state could allow multiple files on the
	* same port number.  It must be pure to allow
	* recursion through allocate(). *)
       and State = State of
	   { tmps:	TmpFile list,	   (* successfully allocated *)
	     pending:	Pending list,
	     used:	int,		    (* disk space used in bytes *)
	     id_cnt:	int,		    (* to allocate ids *)
	     last_warn:	Time.time option }  (* time of the last warning *)

  (* A pending request has the port, file and length
   * and a reply channel. *)
  withtype Pending = int * string * int * Reply Sy.ivar

  val disk_limit = ref ( valOf Int.maxInt )

  fun setDiskLimit n = ( disk_limit := n )

  fun alloc_server ch () : unit =
      let fun loop state =
	      let val new_state =
		      case MLton.PCML.recv ch of
			  Record  arg => allocate state arg
			| Release p   => release state p
			| Undo tmp    => undo state tmp
	      in
		  loop new_state
	      end

	  val init_state =
	      State { tmps    = [],
		      pending = [],
		      used    = 0,
		      id_cnt  = 0,
		      last_warn = NONE }
      in
	  loop init_state
      end




  and allocate
	  (state as State {tmps, pending, used, id_cnt, last_warn})
	  (pend as (port, file, len, rvar)) =
      let val _ = Log.testInform Globals.TestTmpFile Log.Debug
				 ( fn ()=> TF.concat [ "TmpFile allocate file ", file ] )
      in
	  if used + len <= !disk_limit
	  then let val tmp = TmpFile {
			     id	    = id_cnt,
			     port    = port,
			     file    = file,
			     len     = len
			     }
	       in
		   Sy.iPut(rvar, Success tmp);

		   State { tmps    = tmp::tmps,
			   pending = pending,
			   used    = used + len,
			   id_cnt  = id_cnt + 1,
			   last_warn = last_warn }
	       end
	  else let val now = Time.now()
	       in
		   if (* last_warn = NONE orelse
		      (* Time.toMilliseconds(Time.-(now, valOf(last_warn))) *)
		      >= 1000 *) false
		   then Log.log Log.Warn
				(TF.str "TmpFile: Tmp disk space limit exceeded")
		   else ();

		   State {
		   tmps    = tmps,
		   pending = pend::pending,
		  used    = used,
		   id_cnt  = id_cnt,
		   last_warn = SOME now
		   }
	       end
      end
      handle _ =>	(* e.g. integer overflow *)
	     ( Sy.iPut(rvar, Reject);
	       Log.testInform Globals.TestTmpFile Log.Debug
			      ( fn() => TF.concat [ "TmpFile allocation error on port ", Int.toString port ] );
	       state )

  (*  Remove all those files on the port. *)
  and release state the_port =
      let fun keep (TmpFile {port, ...}) = (the_port <> port)
      in
	  remove state keep
      end



  (*	Remove based on the id. *)
  and undo state tmp =
      let val TmpFile {id = tmp_id, ...} = tmp
	  fun keep (TmpFile {id, ...}) = (tmp_id <> id)
      in
	  remove state keep
      end



  (* Remove files according to a filter function.
   * Then rerun the pending requests. *)
  and remove (state as State {tmps, pending, used, id_cnt, last_warn})
	     keep  =
      let val _ = Log.testInform Globals.TestTmpFile Log.Debug
				 ( fn () => TF.str "TmpFile removing" )

	  (* First remove files.  Calculate the new used space.
	   * The pending list is separated out. *)
	  fun filter [] new_tmps new_used =
	      ( State {
		tmps    = new_tmps,
		pending = [],
		used    = new_used,
		id_cnt  = id_cnt,
		last_warn = last_warn
		} )
	    |   filter (tmp::rest) new_tmps new_used =
		( if keep tmp
		  then filter rest (tmp::new_tmps) new_used
		  else let   (* This raises nothing. *)
			  val TmpFile {file, len, ...} = tmp
		      in
			  FileIO.removeFile file;
			  filter rest new_tmps (new_used - len)
		      end
		)


	  (*  Retry all of the pending requests. Any that can't
	   * be satisfied will end up in the pending list again.
	   * We use a FIFO order for rerunning the requests. *)
	  fun retry []        new_state = new_state
	    |   retry (p::rest) new_state = retry rest (allocate new_state p)

	  val filtered_state = filter tmps [] used
	  val final_state = retry (rev pending) filtered_state
      in
	  final_state
      end

  structure Alloc = Singleton(
    			    type input    = AllocRequest MLton.PCML.chan
			    val  newInput = MLton.PCML.channel
			    val  object   = alloc_server
			    )


  fun newBodyFile abort tmp_dir len port =
      (*  If we get a name clash then add _ suffixes. *)
      let fun try n =
	      let val base = concat [ "port", Int.toString port,
				      if n = 0 then "" else ("_" ^ (Int.toString n)) ]

		  val file = Files.appendFile tmp_dir base

		  val () = Log.testInform Globals.TestTmpFile Log.Debug
					  ( fn () => TF.concat [ "newBodyFile trying ", file ] );
	      in
		  if FileIO.exclCreate file
		  then allocate port file len
		  else ( if n > 10
			 then ( Log.error ["File name clashes for file ", file];
				NONE		(* give up *)
			      )
			 else try (n+1) )
	      end

	  (*  Wait until the disk space is allocated.
	   * If we have to abort then leave a dummy thread to keep the
	   * protocol straight.  This avoids the race conditions we
	   * would have if we tried to cancel an allocation request.

	   * If this is not aborted then we return SOME tmp.
	   * If it is aborted then remove the file again and return NONE. *)
	  and allocate port file len =
	      let val rvar = Sy.iVar()

		  fun got_reply (Success tmp) = SOME tmp
		    |   got_reply Reject        = (FileIO.removeFile file; NONE)

		  and got_abort () = ( MLton.PCML.spawn dummy; NONE )

		  (*	Run this to catch left-over allocation requests. *)
		  and dummy() =
		      (
		       case Sy.iGet rvar of
			   Success tmp => MLton.PCML.send(Alloc.get(), Undo tmp)
			 | Reject      => ()
		      )
	      in
		  MLton.PCML.send(Alloc.get(), Record(port, file, len, rvar));

		  MLton.PCML.select [ MLton.PCML.wrap ( Sy.iGetEvt rvar, got_reply ),
			       MLton.PCML.wrap ( Abort.evt abort, got_abort ) ]
	      end

      in
	  try 0
      end
      handle _ => NONE	(* e.g. from an error in exclCreate *)

  fun releasePort port = MLton.PCML.send(Alloc.get(), Release port)

  fun getName (TmpFile {file, ...}) = file

end
