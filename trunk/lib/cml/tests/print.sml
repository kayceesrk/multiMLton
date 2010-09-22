structure TextIO =
   struct
      open TextIO
      fun print s =
         MLton.Thread.atomically
         (fn () => TextIO.print s)

      fun output (strm, s) =
        MLton.Thread.atomically
        (fn() => TextIO.output(strm,s))
   end

val print = TextIO.print
val output = TextIO.output
