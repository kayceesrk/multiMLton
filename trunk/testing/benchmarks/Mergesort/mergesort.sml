structure Main =
struct
    open MLton
    open Pacml


  fun split(inCh, outCh1, outCh2 : int option chan) = let
    fun loop (NONE, _, _) = (
      send (outCh1, NONE); send (outCh2, NONE))
      | loop  (x, out1, out2) = (
      send (out1, x);
      loop (recv inCh, out2, out1))
  in
    loop (recv inCh, outCh1, outCh2)
  end

  fun merge( inCh1, inCh2, outCh: int option chan) =
  let fun copy(fromCh, toCh) = let
    fun loop v = (
      send  (toCh, v);
      if(isSome v) then loop (recv fromCh) else ())
                               in
                                 loop (recv fromCh)
                               end

  fun merge' (from1, from2) = (
      case (from1, from2)
        of (NONE, NONE) => send (outCh, NONE)
         | (_, NONE) => (send (outCh, from1); copy(inCh1, outCh))
         | (NONE, _) => (send (outCh, from2); copy(inCh2, outCh))
         | (SOME a, SOME b) =>
             if a<b then
               (send (outCh, from1); merge' (recv inCh1, from2))
             else
               (send (outCh, from2); merge' (from1, recv inCh2))
         (* end case *))
  in
    merge' (recv inCh1, recv inCh2)
  end

  fun mergeSort () = (let
	  val ch = channel()
      fun sort() = (
        case (recv ch)
          of NONE => ()
           | v1 => (case (recv ch)
                of NONE => send(ch, v1)
                       | v2 => let
                         val ch1 = mergeSort()
                         val ch2 = mergeSort()
                               in
                                 send (ch1, v1); send(ch2, v2);
                                 split (ch,ch1,ch2);
                                 merge (ch1,ch2,ch)
                               end
                   (* end case *))
             (* end case *);
             send (ch, NONE))
    in
      spawn sort;
      ch
    end)

  fun sort n =
  let
    val sor = mergeSort()
    fun loop' (n) = (
      if n=0 then send(sor,NONE) else (send(sor, SOME(n));
                                        loop'(n-1)))
    fun loop () = case (recv sor) of
                        NONE => shutdown OS.Process.success
                      | SOME x => ( (*print((Int.toString(x))^" "); *)loop())
  in
    loop'(n); print "Getting results\n"; loop()
  end

  fun doit n =
    run (fn()=>
    let
      val _ = sort n
    in
      ()
    end)
    (* n = 10000 *)

end

val n =
   case CommandLine.arguments () of
      [] => 100
    | s::_ => (case Int.fromString s of
                  NONE => 100
                | SOME n => n)

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
