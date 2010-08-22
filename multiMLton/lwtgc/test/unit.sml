val i = ref 0
val l = ref []
val a = fn v => v + (!i)
val b = fn s => (("Hello "^ (Int.toString s)); l := s::(!l); s)
val arr = ref []
val s = MLton.size arr
val _ = print ((Int.toString s)^"\n")

val arr = ref (a::(!arr))
val s = MLton.size arr
val _ = print ((Int.toString s)^"\n")

val arr = ref (b::(!arr))
val s = MLton.size arr
val _ = print ((Int.toString s)^"\n")

val arr = ref ((!arr)@(!arr))
val s = MLton.size arr
val _ = print ((Int.toString s)^"\n")

(* val p = MLton.Pointer.getRawPointer arr
val _ = MLton.Pointer.printPointer p *)
