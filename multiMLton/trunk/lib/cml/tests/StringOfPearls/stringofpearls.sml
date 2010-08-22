(* String of Pearls
 * ---------------
 * In this microbenchmark, a string of asyncs are created, each of which receive
 * an integer from the previous async and sends that value incremented by 1 to the
 * next async. After the string is created, we put a value on one side and receive
 * the value from the other side. The output is the number of asyncs on the string.
 *
 * KC - 11/9/2009
 *)

open MLton
open PCML.Threadlet

fun thread (inp, out) () =
 aSend (out, aRecv(inp) + 1)

fun createThread (n, ch) =
  if n = 0 then ch else
    let
      val nChan = newAChan ()
     in
       async(thread (ch, nChan)); createThread (n-1, nChan)
    end

fun doit () =
let
  val inp = newAChan ()
  val out = createThread (1000000, inp)
  val _ = aSend (inp, 0)
  val _ = print (Int.toString(aRecv(out)))
in
  ()
end

val _ = RunPCML.doit(doit, NONE)
