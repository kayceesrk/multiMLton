val a = ref 1
val _ = MLton.move (a)
val _ = print (concat ["The value is ", Int.toString (!a), "\n"])
val _ = MLton.move (a)
val _ = a := !a + 1
val _ = MLton.move (a)
val _ = print (concat ["The value is ", Int.toString (!a), "\n"])
