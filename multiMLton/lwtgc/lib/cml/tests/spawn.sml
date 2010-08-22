
structure Main =
struct
   open CML
   fun loop n = if n=0 then ()
                else
                  loop (n-1)
  fun print' msg =
  let
    val _ = print (msg^" Before "^Int.toString(0))
    val _ = loop 9000000
  in
    print (msg^" After  "^Int.toString(0))
  end


   fun doit' n = if n=0 then ()
                 else
                    (spawn( fn () => print' ("\nThread"^Int.toString(n)))
                    ; doit' (n-1))
   fun doit n =
     (print "\nIn Main.doit";
     RunCML.doit ( fn () =>
       doit' n, NONE))
end
