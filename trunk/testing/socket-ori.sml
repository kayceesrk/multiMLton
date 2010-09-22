datatype r = SERVER | CLIENT

structure Main =
struct

  open MLton.PCML
  structure NB = (* MLton.PCML.NonBlocking *)
  struct
    fun execute f = f ()
  end


  val DEBUG = true
  fun debug str = if DEBUG then (print str) else ()


  val str = "This is a send on the socket\n"
  val strSize = String.size (str)

  fun read socket : string =
    (Byte.unpackStringVec (Word8VectorSlice.full (Socket.recvVec (socket, 100))))

  fun readNB socket : string option =
    Option.map (Byte.unpackStringVec o Word8VectorSlice.full)
    (Socket.recvVecNB (socket, 100))

  fun writeVector (socket, bytes) : unit =
  let
    val numBytes = Word8VectorSlice.length bytes
    val numSent = Socket.sendVec (socket, bytes)
  in
    if numBytes = numSent then ()
    else
        writeVector (socket, Word8VectorSlice.subslice (bytes, numSent, NONE))
  end

  fun write (socket, s: string): unit =
  let
    val bytes = Byte.stringToBytes s
    val numBytes = Word8Vector.length bytes
    val bytesSlice = Word8VectorSlice.full bytes
    val numSent = Socket.sendVec (socket, bytesSlice)
  in
    if numBytes = numSent then ()
    else
        writeVector (socket, Word8VectorSlice.subslice (bytesSlice, numSent, NONE))
  end

  fun loop x = if x = 0 then () else loop (x-1)
  fun loop2 x = if x = 0 then  print "done\n"  else (loop (90000000); loop2 (x-1))
  fun delay () = loop2 10

  fun main (port, role, numReads, numThreads) =
  let
    val addr  = INetSock.any port
  in
    case role of
        SERVER =>
          let
            val socket = INetSock.TCP.socket ()
            val _ = Socket.bind (socket, addr)
            val _ = Socket.listen (socket, 5)
            val addr = Socket.Ctl.getSockName socket
            val _ = print "\n\n"
            val _ = debug "0\n"
              val (socket, _) = Socket.accept socket
            (* val _ = List.tabulate (numThreads, fn _ => spawn (delay)) *)
              val _ = debug "1\n"
              val _ = print (read socket)
              val _ = debug "2\n"
              val _ = print (case readNB socket of
                                NONE => "NONE\n"
                              | SOME s => s)

            val ch = channel ()
              fun loop x = if x=0 then send (ch, ()) else (NB.execute (fn () => write(socket, str)); loop (x-1))
              (* spawn the io intensive threads *)
              val _ = List.tabulate (numThreads, fn _ => spawn (fn () => loop numReads))
              (* wait for the io intensive threads to complete *)
              val _ = List.tabulate (numThreads, fn _ => recv(ch))
              val _ = Socket.close socket
          in
              ()
          end
      | CLIENT =>
          let
            val _ = print "\n\n"
            val _ = debug "2.5\n"
            val socket' = INetSock.TCP.socket ()
            val _ = Socket.connect (socket', addr)
            val _ = List.tabulate (numThreads, fn _ => spawn (delay))
            val _ = debug "3\n"
            val _ = write (socket', "hello, world\n")
            val _ = debug "4\n"
            val ostrm = TextIO.openOut ("outputFile")
            val ch = channel ()
            fun loop (count) = if !count=0 then (debug ("looping : "^(Int.toString (!count))^"\n"); send (ch, ())) else
                                    (NB.execute (fn () => let
                                                              val strOut = read(socket')
                                                              val strOutSize = String.size strOut
                                                              val _ = count := !count - strOutSize
                                                              val _ = TextIO.output (ostrm, strOut)
                                                              val _ = TextIO.flushOut (ostrm)
                                                              val _ = TextIO.output (ostrm, strOut)
                                                              val _ = TextIO.flushOut (ostrm)
                                                            in
                                                              ()
                                                            end)
                                    ; loop (count))

            (* spawn the io intensive threads *)
            val _ = List.tabulate (numThreads, fn _ => spawn (fn () => loop (ref (strSize*numReads))))
            (* wait for the io intensive threads to complete *)
            val _ = List.tabulate (numThreads, fn _ => recv(ch))

            val _ = debug "5\n"
            val _ = TextIO.closeOut ostrm
            val _ = Socket.close socket'
          in
            ()
          end
  end
end

val (n,role, numReads, numThreads) =
   case CommandLine.arguments () of
      [] => (5567, SERVER, 100, 1)
    | s::role::numReads::numThreads::_ => (case (Int.fromString s, Int.fromString numReads, Int.fromString numThreads) of
                  (NONE, NONE, _) => (5567, if role="parent" then SERVER else CLIENT, 100, 1)
                | (SOME n, NONE, _) => (n, if role="parent" then SERVER else CLIENT, 100, 1)
                | (NONE, SOME n, _) => (5567, if role="parent" then SERVER else CLIENT, n, 1)
                | (SOME n, SOME num, SOME t) => (n, if role="parent" then SERVER else CLIENT, num, t))

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = MLton.RunPCML.doit (fn () => Main.main (n, role, numReads, numThreads), NONE)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
