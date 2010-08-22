structure Select: SELECT =
struct

type 'a select = ('a -> bool)

fun mkSelect f = f

val always = fn _ => true

val never = fn _ => false

fun and_o (s1,s2) = fn x => s1 x andalso s2 x

fun or_o (s1,s2) = fn x => s1 x orelse s2 x

fun andfold [] = always
  | andfold (s::[]) = mkSelect s
  | andfold (s::ss) = List.foldl and_o s ss

fun orfold [] = always
  | orfold (s::[]) = mkSelect s
  | orfold (s::ss) = List.foldl or_o s ss

fun app fs x = fs x

end
