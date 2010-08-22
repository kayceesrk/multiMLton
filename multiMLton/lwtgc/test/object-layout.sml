val r = ref 0
val foo = (fn y => (r := !r+1; y + (!r)))
val _ = MLton.size foo
val _ = MLton.walk (ref foo)
val v = foo 10
val _ = print (Int.toString v)
