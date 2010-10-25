structure PacmlPrim =
struct
  fun move x = Primitive.MLton.move (ref x)
end
