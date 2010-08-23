structure S =
struct
  datatype num = ONE | TWO | ELSE
  fun f n = case n of
                 1 => ONE
               | 2 => TWO
               | _ => ELSE
end


val n =
   case CommandLine.arguments () of
      [] => 100
    | s::_ => (case Int.fromString s of
                  NONE => 100
                | SOME n => n)

val () =
  case S.f(n) of
     S.ONE => print "one"
   | S.TWO => print "two"
   | _ => print "else"
