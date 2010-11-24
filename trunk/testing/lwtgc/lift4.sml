val a = ref 0
val _ = a := 1
val _ = MLton.move (a)
val _ = print (concat ["VALUE : ", Int.toString (!a)])
val _ = a := 6
val _ = print (concat ["VALUE : ", Int.toString (!a)])
val _ = MLton.move (a)
val _ = print (concat ["VALUE : ", Int.toString (!a)])
