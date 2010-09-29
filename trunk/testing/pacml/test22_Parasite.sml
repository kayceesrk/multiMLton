structure Main =
struct

  open MLton.Pacml

  fun tightLoop2 n =
    if n=0 then ()
    else tightLoop2 (n-1)

  fun tightLoop n =
    if n=0 then ()
    else (tightLoop2 300; tightLoop (n-1))

  (* tightLoop (1000) ~= 1ms *)

  fun doit n m=
  let
    val ch = channel ()
  in
    run
    (fn () =>
    let
      fun loop n =
        if n=0 then ()
        else (spawnParasite (fn () => tightLoop m); loop (n-1))
    in
      loop n
    end)
  end
end

val (n,m) =
   case CommandLine.arguments () of
      [] => raise Fail "Needs 2 arguments"
    | s::[] => raise Fail "Needs 2 arguments"
    | s1::s2::_ => (case (Int.fromString s1, Int.fromString s2) of
                  (SOME n, SOME m) => (n, m)
                | _ => raise Fail "Arguments not valid")

val ts = Time.now ()
val _ = Main.doit n m
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
