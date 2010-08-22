datatype r = VANILLA | ORI

structure Main =
struct

  open MLton.PCML
  structure NB =MLton.PCML.NonBlocking

  val DEBUG = true
  fun debug str = if DEBUG then (print str) else ()

  fun loop x = if x = 0 then () else (loop (x-1))
  fun loop2 x = if x = 0 then  print "done\n"  else (loop (90000000); loop2 (x-1))
  fun delay () = loop2 10

  fun main (urlString, role, numReads, numThreads) =
  let
    val url = valOf (Url.fromString (urlString))
    val outputFile = Out.openOut ("downloadedFile")
    fun fetchFile () =
    let
      val _ = debug "1\n"
      val istrm = Http.fetch {head = false, headers = [], post = NONE, proxy = NONE, url = url}
      val _ = debug "2\n"
    in
      In.outputAll (istrm, outputFile)
    end
  in
    case role of
        VANILLA =>
          let
              val _ = List.tabulate (numThreads, fn _ => spawn (delay))
              val ch = channel ()
              fun loop x = if x=0 then send (ch, ()) else (fetchFile (); loop (x-1))
              (* spawn the io intensive threads *)
              val _ = List.tabulate (numThreads, fn _ => spawn (fn () => loop numReads))
              (* wait for the io intensive threads to complete *)
              val _ = List.tabulate (numThreads, fn _ => recv(ch))
          in
              ()
          end
      | ORI =>
          let
              val _ = List.tabulate (numThreads, fn _ => spawn (delay))
              val ch = channel ()
              fun loop x = if x=0 then send (ch, ()) else (NB.execute (fetchFile); loop (x-1))
              (* spawn the io intensive threads *)
              val _ = List.tabulate (numThreads, fn _ => spawn (fn () => loop numReads))
              (* wait for the io intensive threads to complete *)
              val _ = List.tabulate (numThreads, fn _ => recv(ch))
          in
              ()
          end
    end
end

val (urlString, role, numReads, numThreads) =
   case CommandLine.arguments () of
     urlString::role::numReads::numThreads::_ => (urlString, if role="vanilla" then VANILLA else ORI, valOf(Int.fromString numReads), valOf(Int.fromString numThreads))
    | _ => raise Fail "Wrong args"

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = MLton.RunPCML.doit (fn () => Main.main (urlString, role, numReads, numThreads), NONE)
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
