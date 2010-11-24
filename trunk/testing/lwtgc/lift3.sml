val a = ref []
val _ = print ("Before first write\n")
val _ = a := 5::(!a)
val _ = print ("Before move\n")
val _ = MLton.move (a)
val _ = print (concat ["VALUE : ", Int.toString (List.nth (!a, 0)), "\n"])
val l = !a
val _ = print (concat ["VALUE from l : ", Int.toString (List.nth (l, 0)), "\n"])
val l = 6::l
val _ = print "Append successful\n"
val _ = a := l
val _ = print "Write Successful\n"
