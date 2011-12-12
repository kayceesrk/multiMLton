structure Main =
struct
  open MLton.Pacml

    fun tak(x,y,z)  = let
      val ch = channel()
      fun worker ()  =
          if not (y<x) then
            send (ch, z)
          else
            let
              val ch1 = tak(x-1, y, z)
              val ch2 = tak(y-1, z, x)
              val ch3 = tak(z-1, x, y)
              val ch4 = tak((recv ch1), (recv ch2), (recv ch3))
            in
              send(ch, (recv ch4))
            end
    in
      (* print (concat [Int.toString x, " ", Int.toString y, " ", Int.toString z, "\n"]); *)
      spawn worker;
      ch
    end



    fun doit n =
      run (fn()=>
      let
        val ch = tak(25, 10, 5)
        val x = recv ch
        val _ = print (Int.toString(x)^"\n")
      in
        ()
      end)

    (* n = ANY *)
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
