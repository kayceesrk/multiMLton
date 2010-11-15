structure Main =
struct

  open MLton.Pacml
  fun pause () =
  let
    fun tightLoop2 n =
      if n=0 then ()
      else tightLoop2 (n-1)

    fun tightLoop n = (* tightLoop (1000) ~= 1ms on 1.8Ghz core *)
      if n=0 then ()
      else (tightLoop2 300; tightLoop (n-1))
  in
    tightLoop (100)
  end

  fun foo () =
  let
    val ts = Time.now ()
    val _ = pause ()
    val te = Time.now ()
    val diff = Time.-(te,ts)
    val str = LargeInt.toString (Time.toMilliseconds diff)
    (* val str = Time.toString diff (* This segfaults *) *)
    val _ = print (concat[str, "\n"])
  in
    foo ()
  end


  fun doit () =
    run (fn () =>
           let
             val _ = List.tabulate (100, fn _ => spawnHost (foo))
           in
             ()
           end)
end


val ts = Time.now ()
val _ = Main.doit ()
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
