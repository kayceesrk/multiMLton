structure Main =
struct
  open MLton
  open Parallel

  fun delay n = case n of
                     0 => ()
                   | _ => delay (n-1)

  fun sum(lst,res) =
    (case List.length(lst) of
         1 => let val _ = (res := (List.nth(lst,0))) in () end
       | _ =>
           (let
             val res1 = ref 0
             val res2 = ref 0
             val l = (List.length lst) div 2
             val lsta = (List.take(lst,l))
             val lstb = (List.drop(lst,l))
             val _ = ForkJoin.fork(fn() => sum(lsta,res1), fn() =>
                     sum(lstb,res2))
             val _ = res := (!res1) + (!res2)
             val _ = delay 500000
           in
             ()
           end))

  fun doit n=
  let
    val res = ref 0
    val lst = List.tabulate(n,fn(x)=>x)
    val _ = sum(lst,res)
  in
    !res
  end
end

(* Get Input *)
val n = (case (TextIO.inputLine(TextIO.stdIn)) of
             NONE => 1
           | SOME(x) => (case Int.fromString(x) of
                              NONE => 1
                            | SOME(y) => y))

val ts = Time.now ()
val v = Main.doit n
val te = Time.now ()
val _ = print(Int.toString(v))
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
