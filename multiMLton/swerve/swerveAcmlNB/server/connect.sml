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

(* $Id: connect.sml,v 1.13 2001/09/26 21:39:17 felix Exp $ *)

(*  This represents an open socket connection.
    It is simpler to do our own buffering than use the
    general MLton.PCML IO streams.

@#34567890123456789012345678901234567890123456789012345678901234567890
*)

signature CONNECT =
sig

    type Conn

    (* This will be raised by any read or write operation that doesn't
     * complete within the timeout. *)
    exception Timeout

    (* This sets up a connection for a connected stream socket.
     * The response channel is shared only with the connection manager. *)
    val setupConn:
	{ socket:     Socket.active INetSock.stream_sock,
	  sock_addr:  INetSock.sock_addr,
	  timeout:    int option
	} -> Conn

    (*	Information about the connection. *)
    val	getPort:    Conn -> int
    val	getAddress: Conn -> NetHostDB.in_addr
    val	formatAddress: Conn -> string

    (*	This is imperative, updating the connection's state.

     * This may return less than the number of requested bytes.
     * The size of the string is returned to save the caller counting it.

     * This returns NONE if there is no data e.g. because of eof.
     * We don't use an exception for the eof condition since it may be
     * normal processing that we stop reading at the eof.

     * This raises Timeout *)
    val read:	    Conn -> int  -> (string * int) option

    (* This extends read by continuing to read until all of the requested bytes have
     * been read or eof is found.
     * This raises Timeout *)
    val readAll:    Conn -> int -> (TextFrag.t * int)

    (* This keeps reading until an entire line is read. The line
     * is terminated by either a \r\n or a \n. But we don't
     * include this in the returned string.

     * This returns NONE if eof.
     * This raises Timeout *)
    val readLine:   Conn -> string option

    (* This writes with blocking.
     * Writing will be unbuffered. The caller should send
     * large chunks where it matters.
     * This raises Timeout *)
    val write:	    Conn -> string -> unit

    val close:	    Conn -> unit

    (*	Connection-level Protocol Operations *)
    (*	This returns the abort object for synchronising. *)
    val getAbort:  Conn -> Abort.t

    (*	This tests if the timeout has occurred. *)
    val aborted:    Conn -> bool

end


structure Connect: CONNECT =
struct

  open Common

  structure TF  = TextFrag
  structure S   = Socket
  structure PF  = Posix.FileSys
  structure PIO = Posix.IO
  structure NB  = MLton.PCML.NonBlocking
  structure SS  = Substring

  datatype Conn = Conn of
	   { socket:	Socket.active INetSock.stream_sock,
	     port:	int,
	     addr:	NetHostDB.in_addr,

	     is_open:	bool ref,
	     rdbuf:	string ref,
	     rdlen:	int ref,	(* number of chars left *)
	     rdoff:	int ref,	(* offset to next avail char *)

	     (*	This transmits abort messages to all receivers. *)
	     abort:	Abort.t }

  exception Timeout

  fun aborted (Conn {abort, ...}) = Abort.aborted abort

  fun getAbort (Conn {abort, ...}) = abort

  fun not_crlf c = not (c = #"\r" orelse c = #"\n")

  (* Overwrite the buffer with the next chunk of text.
   * If rdlen stays 0 then we have an eof. *)
  fun fill_buf (Conn {socket, rdbuf, rdlen, rdoff, abort, ...}) =
      let val timeout : LargeInt.int = 10
	  fun takeVec v =
	      let val s = Byte.bytesToString v
	      in
		  rdbuf := s;
		  rdlen := size s;
		  rdoff := 0
	      end
      in
	  (* FIXME RPR - used to be MLton.PCML / Abort.t for timeout.
	   * That whole system used old SML/NJ API's i.e. Socket.recVecEvt
	   * For now just hack in a hardcode timeout. *)
	  let val {rds, ...} = S.select {rds = [S.sockDesc socket],
					wrs = [],
					exs = [],
					timeout = SOME (Time.fromSeconds timeout)}
	  in
	      if List.null rds
	      then raise Timeout
	      else let val bytes = S.recvVec ( socket, 1024 )
		   in
		       takeVec bytes
		   end
	  end
      end

  (* Socket handling.
   * There will be no eof marker on an input message. We have to avoid
   * blocking so that we can respond as soon as the last character
   * of a complete message is seen.	This works nicely since the
   * MLton.PCML implementation of sockets always sets up the socket in
   * non-blocking mode and we can select on data being ready. *)

  fun setupConn {socket, sock_addr, timeout} =
      let val _ = Log.inform Log.Debug (fn () => TF.str "Setting up connection.");

	  (* Apache has special linger handling but SO_LINGER works on Linux. *)
	  val _ = S.Ctl.setLINGER ( socket, SOME ( Time.fromSeconds 2 ) )

	  val _ = Log.inform Log.Debug (fn () => TF.str "Port from Address")

	  val ( addr, port ) = INetSock.fromAddr sock_addr

	  val abort =
	      case timeout of
		  NONE   => Abort.never()
		| SOME t => Abort.create t
      in

	  Log.inform Log.Debug (fn () => TF.str "Ctor Conn");

	  Conn { socket  = socket,
		 port    = port,
		 addr    = addr,
		 is_open = ref true,
		 rdbuf   = ref "",
		 rdlen   = ref 0,
		 rdoff   = ref 0,
		 abort   = abort }

      end

  fun getPort ( Conn { port, ... } ) = port

  fun getAddress ( Conn { addr, ... } ) = addr

  fun formatAddress conn = NetHostDB.toString ( getAddress conn )

  val proc = valOf (NB.createProcessor ())

  fun read' ( conn as Conn { is_open, rdbuf, rdlen, rdoff, ... } ) nbytes =
      let fun getn n =
	      let val s = String.substring (!rdbuf, !rdoff, n )
	      in
		  rdlen := ( !rdlen ) - n;
		  rdoff := ( !rdoff ) + n;
		  SOME ( s, n )
	      end
      in
	  if aborted conn
	  then
	      raise Timeout
	  else
	      (
	       if !is_open
	       then (
		   if !rdlen = 0
		   then fill_buf conn
		   else ();

		   if !rdlen = 0
		   then NONE	(* at eof *)
		   else getn(Int.min(nbytes, !rdlen))
		   )
	       else NONE
	      )
      end

  val read = fn x => fn y => NB.executeOn proc (fn () => read' x y)

  fun readAll ( conn as Conn { is_open, rdbuf, rdlen, rdoff, ... } ) nbytes =
      let fun loop 0 rslt = ( 0, rev rslt )
	    |   loop n rslt =
		     (
		      case read conn n of
			  NONE        => ( n, rev rslt )
			| SOME ( s, l ) => loop ( n - l ) ( s::rslt )
		     )

	  val ( left, chunks ) = loop nbytes []
      in
	  ( TF.concat chunks, nbytes - left )
      end

  (* Keep reading until an entire line is read. The line
   * is terminated by either a \r\n or a \n. But we don't
   * include this in the returned string. *)
  fun readLine (conn as Conn {is_open, rdbuf, rdlen, rdoff, ...}) =
      let val warned = ref false

	  (* Keep reading until we see a non crlf character after we have seen
	   * a cr. On eof, return the chunks that we have so far. *)
	  fun get_line chunks have_r line_len =
	      (
	       if !rdlen = 0 then fill_buf conn else ();

	       if !rdlen = 0
	       then return chunks		(* still nothing new *)
	       else get_chunk chunks have_r line_len
	      )

	  and return []   = NONE
	    | return chks = SOME(SS.concat(rev chks))

	  and get_chunk chunks have_r line_len =
	      let val buf = SS.substring(!rdbuf, !rdoff, !rdlen)

		  (* val _=print(concat["readLine buf='",SS.string buf,"'\n"]) *)
		  val (chunk, rest) = SS.splitl not_crlf buf

		  (* If we are over the line length limit then new chunks
		   * are discarded to prevent a DOS attack with an arbitrarily
		   * long line. *)
		  val (new_chunks, new_line_len) =
		      if line_len > 10000
		      then
			  (
			   Log.log Log.Warn ( TF.concat [ "Truncated line from client ",
		    					  formatAddress conn ] );
			   ( chunks, line_len )
			  )
		      else ( chunk :: chunks, line_len + ( SS.size chunk ) )

	      (* val _ = print(concat["readLine rest='",
				      String.toString(SS.string rest), "'\n"]) *)
	      in
		  if SS.isPrefix "\r\n" rest orelse SS.isPrefix "\n" rest
		  then	(* rest contains the beginning of the next line *)
		      let val skip = if SS.sub(rest, 0) = #"\r" then 2 else 1
			  val new_buf = SS.triml skip rest
		      in
			  rdbuf := SS.string new_buf;
			  rdlen := size(!rdbuf);
			  rdoff := 0;
			  return new_chunks
		      end
		  else
		      if SS.isPrefix "\r" rest
		      then
			  (
			   if SS.size rest = 1
			   then
			       (
				(*  There may still be a \n at the beginning of
				 * the next chunk. *)
				rdlen := 0;
				get_line new_chunks true new_line_len
			       )
			   else (* A line terminated by a \r. *)
			       let val new_buf = SS.triml 1 rest
			       in
				   rdbuf := SS.string new_buf;
				   rdlen := size(!rdbuf);
				   rdoff := 0;
				   return new_chunks
			       end
			  )
		      else
			  (
			   (*	This buffer has no line termination. If we've
			    * seen a \r then we have a line terminated by only
			    * a \r.  Otherwise keep accumulating chunks. *)
			   if have_r
			   then return new_chunks
			   else
			       (
				rdlen := 0;
				get_line new_chunks have_r new_line_len
			       )
			  )
	      end

	  fun warn_trunc() =
	      (
	       if !warned
	       then ()
	       else Log.log Log.Warn ( TF.concat [ "Truncated line from client ",
						   formatAddress conn ] );
	       warned := true
	      )

      in
	  if aborted conn
	  then raise Timeout
	  else
	      (
	       if !is_open
	       then get_line [] false 0
	       else NONE
	      )
      end

  val readLine = fn x => NB.executeOn proc (fn () => readLine x)

  (*	The sendVec blocks. *)
  fun write (conn as Conn {socket, is_open, ...}) msg =
      (
       if !is_open
       then let val bytes = Byte.stringToBytes msg
		val len   = size msg
		(*  n is the number of bytes written so far. *)
		fun loop n =
		    (
		     if aborted conn
		     then raise Timeout
		     else let (* val _ = toErr(concat["Connect.write sendVec n=",
		    				      Int.toString n, " len=",
						      Int.toString len, "\n"]) *)
			     val sent = n + ( S.sendVec ( socket, Word8VectorSlice.full bytes ) )
			 in
			     if sent >= len
			     then ()
			     else loop sent
			 end
		    )
	    in
		loop 0
	    end
       else ()
      )

  val write = fn x => fn y => NB.executeOn proc (fn () => write x y)

  fun close (Conn {socket, is_open, ...}) =
      if !is_open
      then
	  (
	   S.close socket;
	   is_open := false
	  )
      else ()

end
