structure PacmlPrim =
struct
  fun move x = Primitive.MLton.move (ref x)
  fun initRefUpdate () =
    Primitive.Ref.preemptFn := SOME (fn () => print "preemptFn invoked\n")
end
