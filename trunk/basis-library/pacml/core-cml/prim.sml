structure PacmlPrim =
struct
  fun move x = Primitive.MLton.move (ref x)
  fun initRefUpdate f =
    Primitive.Ref.preemptFn := f
  val addToPreemptOnWBA = Primitive.Ref.addToPreemptOnWBA
end
