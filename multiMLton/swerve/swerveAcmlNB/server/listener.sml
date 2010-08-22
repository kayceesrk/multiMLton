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

(* $Id: listener.sml,v 1.19 2002/03/10 17:18:25 felix Exp $ *)

(*  This runs the sockets.

@#34567890123456789012345678901234567890123456789012345678901234567890
*)

signature LISTENER =
sig

    (*	This runs the listener in the thread of the caller, which is the
     * main thread. This never returns but it may throw an exception. *)
    val	run:	unit -> unit

end


structure Listener: LISTENER =
struct

    open Common
    open Config

    structure TF = TextFrag
    structure G  = Globals

    structure S = Socket
    structure PF  = Posix.FileSys
    structure PIO = Posix.IO
    structure NB = MLton.PCML.NonBlocking

    (*	These are the requests to the connection manager.
     *	This is our kind of connected socket for TCP connections.
     * INetSock.inet is the specialised type for the internet address family. *)
    type OurConn = S.active INetSock.stream_sock
    type OurAddr = INetSock.sock_addr

    datatype ListenMsg = ConnDied
(*    datatype accept_msg = Accept of (Word8.word ,S.active S.stream) S.sock * Word8.word S.sock_addr *)

    fun run() =
	let val ServerConfig {
		conn_timeout: int option,
		max_clients:  int option,
		listen_host,
		listen_port,
		...
	    } = getServerConfig ()

	    (*  Build an address for listening. *)
	    val listen_addr =
		case listen_host of
		    NONE => INetSock.any listen_port
		  | SOME host =>
		    (
		     (* The configuration is supposed to have validated
		      * the host. *)
		     case NetHostDB.getByName host of
			 NONE       => raise InternalError "invalid host"
		       | SOME entry => INetSock.toAddr(NetHostDB.addr entry, listen_port)
		    )

	    val listener = INetSock.TCP.socket()
	in
	    (*  Doing these fixes the type of the socket as passive. *)
	    Socket.Ctl.setREUSEADDR ( listener, true );
	    Socket.bind ( listener, listen_addr );
	    Socket.listen (listener, !Common.connBackLog);

	    let val pollDesc = case OS.IO.pollDesc ( Socket.ioDesc listener ) of
				   NONE => raise Fail "Unable to get poll_desc from listener socket."
				 | SOME pdesc => pdesc
	    in
		FileIO.setCloseExec pollDesc
	    end;
	    serve listener max_clients conn_timeout
	end
	handle x => (Log.logExn x; raise FatalX)

    (* Here we spawn a thread for each connection. We
     * catch the termination of each thread so that we can count them.
     * If there are too many connections then we just close the new
     * socket giving the client a Connection Refused error.

     * Although we could retain all of the thread ids and use
     * joinEvt() to select on their termination there might be
     * scalability issues in a large server when doing a select()
     * and joinEvt() on many connections.

     * Instead we have each connection tell us when it dies.
     * The connection thread will stay live to the GC although we
     * don't hold its thread_id.

     * Exceptions in here should close the listener socket. *)
    and serve listener max_clients conn_timeout =
	let
        val proc = valOf(NB.createProcessor ())
        val lchan: ListenMsg MLton.PCML.chan = MLton.PCML.channel ()
	    val acceptChan = MLton.PCML.channel ()

	    fun listenLoop () =
		let
           fun foo () = case NB.executeOn proc (fn () => S.acceptNB listener) of
                           NONE => foo ()
                         | SOME e => e
           val sock = foo ()
		in
		    Log.inform Log.Debug (fn () => TF.str "Incoming connection.  Placing it in acceptChan.");
		    MLton.PCML.send ( acceptChan, sock );
		    listenLoop ()
		end
		handle x => ( Log.logExn x; listenLoop () )

	    fun loop num_connects =
		let (* If we have too many then we will refuse the new
		     * connection.  We require each connection thread to tell
		     * us when it dies.

		     * We won't log the connection refusals to avoid the log
		     * overflowing on a DOS attack. *)
          val _ = Debug.debug' ("debug : "^(Int.toString(num_connects)))
		    fun new_connect (conn, conn_addr) =
			(
			 if isSome max_clients andalso num_connects >= (valOf max_clients)
			 then (Socket.close conn;
			       num_connects)
			 else let val pollDesc = case OS.IO.pollDesc (Socket.ioDesc conn) of
						     NONE => raise Fail "Unable to get poll_desc from socket."
						   | SOME pdesc => pdesc
			      in
				  FileIO.setCloseExec ( pollDesc ); (* RPR was Socket.pollDesc *)

				  MLton.PCML.spawn( MyProfile.timeIt "Listener connection"
							      ( connection lchan conn conn_addr conn_timeout ) );

				  num_connects + 1
			      end
			)
			handle x =>
			       (
				Socket.close conn handle _ => ();
				Log.logExn x;
				num_connects
			       )

		    fun msg ConnDied = num_connects - 1

		    val new_num =  if isSome max_clients andalso num_connects + 10 >= (valOf max_clients) then
                            (MLton.PCML.ssync (MLton.PCML.wrap (MLton.PCML.recvEvt lchan, msg)))
                          else MLton.PCML.select [ MLton.PCML.wrap ( MLton.PCML.recvEvt acceptChan, new_connect ),
					       MLton.PCML.wrap ( MLton.PCML.recvEvt lchan, msg ) ]
		in
		    loop new_num
		end
	in
        MLton.PCML.spawn (fn () => NB.executeOn proc listenLoop);
	    loop 0
	end
	handle x =>
	       (
		Socket.close listener;
		Log.logExn x;
		raise FatalX
	       )

    (*	This runs in a thread to handle a connection.

     * The time limit applies both to the client and the handler.
     * If the client doesn't complete sending its request before the
     * time limit then we force the connection closed. Similarly the
     * protocol module must complete its reponse before the time limit.

     * The protocol module is called as a function. It returns when it is
     * time to close the connection or the time limit has been exceeded. *)
    and connection lchan sock sock_addr conn_timeout () =
        let
	      fun run() =
	        let val _ = Log.inform Log.Debug (fn () => TF.str "Setup connection")
            val conn = MyProfile.timeIt "Listener setupConn"
                Connect.setupConn { socket  = sock,
                            sock_addr = sock_addr,
                            timeout = conn_timeout }
	    in
            Log.inform Log.Debug ( fn () => TF.str "Connection Talk" );

            MyProfile.timeIt "Listener talk"
            HTTP_1_0.talk conn;

            Log.inform Log.Debug ( fn () => TF.str "Connection Close" );
            MyProfile.timeIt "Listener close"
            Connect.close conn;

            Log.testInform G.TestConnect Log.Debug
                ( fn () => TF.str "Connection closed" );

            Log.inform Log.Debug ( fn () => TF.str "Listener Release" );

            MyProfile.timeIt "Listener release"
            TmpFile.releasePort ( Connect.getPort conn );

            Log.testInform G.TestConnect Log.Debug
            ( fn () => TF.str "TmpFiles released" )
        end

    in
	Log.inform Log.Info (fn()=>TF.seq [TF.str "New connection from ",
	                            format_addr sock_addr]);

	MyProfile.timeIt "Listener run" run();

	Log.inform Log.Info (fn()=>TF.seq [TF.str "End of connection from ",
				    format_addr sock_addr]);

	MyProfile.timeIt "Listener died"
	    MLton.PCML.send(lchan, ConnDied)
    end
    handle x =>
	let
	    (*	See also Connect.getPort *)
	    val (_, port) = INetSock.fromAddr sock_addr
	in
	    (
		Socket.close sock;
		TmpFile.releasePort port
	    ) handle _ => ();		(* being paranoid *)
	    Log.logExn x;
	    MLton.PCML.send(lchan, ConnDied)
	end


    and format_addr sock_addr =
    let
	val (in_addr, port) = INetSock.fromAddr sock_addr
    in
	TF.concat [NetHostDB.toString in_addr, ":", Int.toString port]
    end

(*------------------------------------------------------------------------------*)


end

