structure Main =
struct
  open CML

  val ring = 503

  fun thread (i : int) (l : int chan) (r : int chan) =
  let
    val v = recv l
  in
    if v = 0 then
      (print (Int.toString i);
      OS.Process.exit OS.Process.success)
    else
      (send (r, v-1);
      thread i l r)
  end

  fun spawnThreads (i, l) =
  let
    val r = channel ()
    val _ = spawn (fn () => thread i l r)
  in
    r
  end

  fun doit n =
    RunCML.doit
      (fn () =>
        let
          val lst = List.tabulate (ring - 1, fn i => i + 2)
          val initL = channel ()
          val initR = foldl spawnThreads initL lst
          val _ = spawn (fn () => thread 1 initR initL)
          val _ = send (initR, n)
        in
          ()
        end,
        SOME (Time.fromMilliseconds 10))
    end


val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 600
val _ = Main.doit n
