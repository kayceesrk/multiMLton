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

(*  Copyright (c) 2001 Anthony L Shipman *)


structure Main =
struct
    type ServerSock = Socket.active INetSock.stream_sock

    datatype Handler = Handler of {
	    socket: ServerSock,
	    reader: ServerSock -> unit,
	    writer: ServerSock -> unit
	    }

    fun poll (handlers: Handler list) =
    let
	(*  Convert to a list annotated with iodesc. *)
	fun to_iodesc (Handler {socket, reader, writer}) = 
	    (OS.IO.pollToIODesc(Socket.pollDesc socket),
	    	socket, reader, writer)

	val with_iodesc = map to_iodesc handlers

	(*  Generate a list of poll descriptors for reading
	    and writing.
	*)
	fun to_poll (Handler {socket, ...}) = 
		(OS.IO.pollIn o OS.IO.pollOut o Socket.pollDesc)
		socket

	(*  Search for the matching handlers. *)
	fun check_info poll_info =
	let
	    val info_iodesc = OS.IO.pollToIODesc(
				    OS.IO.infoToPollDesc poll_info)
	    val handler = List.find
			  (fn arg => (#1 arg) = info_iodesc)
			  with_iodesc
	in
	    case handler of
	      NONE => raise Fail "polled a non-existent socket!"

	    | SOME (iodesc, socket, reader, writer) =>
	    (
		if OS.IO.isIn  poll_info then reader socket else ();
		if OS.IO.isOut poll_info then writer socket else ()
	    )
	end

	val info_list = OS.IO.poll(map to_poll handlers, NONE)
    in
	app check_info info_list
    end



    fun serve port =
    let
	fun run listener =
	let
	    fun accept() =
	    let
		val (conn, conn_addr) = Socket.accept listener
	    in
		poll [Handler {
			socket = conn,
			reader = reader,
			writer = writer
			}];
		accept()
	    end

	    and writer conn =
	    let
		val msg = "hello world from tcpserver\n"
		val buf = {
			buf = Byte.stringToBytes msg,
			i = 0,
			sz = NONE
			}
	    in
		print "responding to a client\n";
		ignore(Socket.sendVec(conn, buf));
		Socket.close conn
	    end
	    handle x => (Socket.close conn; raise x)

	    and reader conn = ()

	in
	    Socket.Ctl.setREUSEADDR(listener, true);
	    Socket.bind(listener, INetSock.any port);
	    Socket.listen(listener, 9);
	    accept()
	end
	handle x => (Socket.close listener; raise x)
    in
	run (INetSock.TCP.socket())
    end
    handle OS.SysErr (msg, _) => raise Fail (msg ^ "\n")


    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    fun main(arg0, argv) =
    let
    in
	case argv of
	  [port] => 
	    (
		case Int.fromString port of
		  NONE => raise Fail "Invalid port number\n"

		| SOME p => serve p
	    )

	| _ => raise Fail "Usage: pollserver port\n";

        OS.Process.success
    end
    handle
      Fail msg => (toErr msg; OS.Process.failure)

    | x =>
    (
	toErr(concat["Uncaught exception: ", exnMessage x,
	             " from\n"]);
	app (fn s => (print "\t"; print s; print "\n"))
	    (SMLofNJ.exnHistory x);
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("pollserver", main)
end



