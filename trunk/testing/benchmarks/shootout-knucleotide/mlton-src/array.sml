structure Array =
   struct
      open Array

      fun foldi (a, b, f) = Array.foldli f b a

      fun fold (a, b, f) = Array.foldl f b a

      fun foreach (a, f) = Array.app f a

      fun foreachi (a, f) = appi f a

      fun forall (a, f) = Array.all f a

      val new = array

      fun modify (a, f) = Array.modify f a
   end
