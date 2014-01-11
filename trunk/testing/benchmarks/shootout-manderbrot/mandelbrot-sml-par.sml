(* mandelbrot.sml
 *
 *   Mandelbrot (fractal generation) benchmark.
 *     (Loosely based on the C version.)
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 * Modified and ported to MLton by Vesa Karvonen.
 *)

open MLton.Pacml

val (K, L2) = (50, 4.0)

fun out b = TextIO.output1 (TextIO.stdOut, Byte.byteToChar b)

fun repeat 0 f = ()
  | repeat n f = (ignore (f (n-1)); repeat (n-1) f)

fun mandel (h, w, hb, he) =
   let fun p (x, y) =
          let val (Cr, Ci) = (real x*2.0/real w-1.5, real y*2.0/real h-1.0)
              fun lp (r, i, k) =
                  let val (r2, i2) = (r*r, i*i)
                  in r2+i2 <= L2 andalso
                     (k=0 orelse lp (r2-i2+Cr, (r+r)*i+Ci, k-1)) end
          in lp (0.0, 0.0, K) end
       fun xl (x, y, b, n, buf) =
          if x = w then yl (y+1,(Word8.<< (b, n))::buf)
          else let val (buf, b, n) = if n=0w0 then (b::buf, 0w0, 0w8) else (buf, b, n)
               in xl (x+1, y, b+b+(if p (x, y) then 0w1 else 0w0), n-0w1, buf) end
       and yl (y, buf) = if y < he then xl (0, y, 0w0, 0w8, buf) else buf
     val buf = rev (yl (hb,[]))
     (* val _ = app out buf *)
   in buf end

val (n, nt) =
  case CommandLine.arguments () of
     s1::s2::_ => (case (Int.fromString s1,
                         Int.fromString s2) of
                        (SOME n1, SOME n2) => (n1, n2)
                      | _ => (1000, 1))
   | _ => (1000, 1)

fun main (n, nt) =
let
  val numChunks = n div 8
  val resultArray = Array.tabulate (numChunks, fn _ => [])
  val ic : (int * int * int * int) chan = channel ()
  val oc : (int * Word8.word list) chan = channel ()

  fun client () =
  let
    val (h, w, hb, he) = recv ic
    val buf = mandel (h, w, hb, he)
    val _ = aSend (oc, (hb div 8, buf))
  in
    client ()
  end

  fun receiver () =
  let
    val _ = repeat numChunks (fn _ =>
              let val (index, buf) = recv oc
              in Array.update (resultArray, index, buf)
              end)
    val _ = Array.app (fn buf => List.app out buf) resultArray
  in
    OS.Process.exit OS.Process.success
  end


  val _ = repeat nt (fn _ => spawn client)
  val _ = spawn receiver
  val _ = repeat numChunks (fn i => aSend (ic, (n, n, i*8, i*8+8)))
in
  ()
end


val _ = app print ["P4\n", Int.toString n, " ", Int.toString n, "\n"]
val _ = run (fn () => main (n, nt))
(* val _ = run (fn () => ignore (mandel (n,n,0,n))) *)
