structure PacmlPrim =
struct
  fun move x = Primitive.MLton.move (ref x)
  fun initRefUpdate f =
    Primitive.Ref.preemptFn := f
end
