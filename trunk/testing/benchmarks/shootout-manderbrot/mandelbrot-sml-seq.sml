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

val (K, L2) = (50, 4.0)

fun out b = TextIO.output1 (TextIO.stdOut, Byte.byteToChar b)

fun mandel (h, w) =
   let fun p (x, y) =
          let val (Cr, Ci) = (real x*2.0/real w-1.5, real y*2.0/real h-1.0)
              fun lp (r, i, k) =
                  let val (r2, i2) = (r*r, i*i)
                  in r2+i2 <= L2 andalso
                     (k=0 orelse lp (r2-i2+Cr, (r+r)*i+Ci, k-1)) end
          in lp (0.0, 0.0, K) end
       fun xl (x, y, b, n) =
          if x = w then (out (Word8.<< (b, n)) ; yl (y+1))
          else let val (b, n) = if n=0w0 then (out b ; (0w0, 0w8)) else (b, n)
               in xl (x+1, y, b+b+(if p (x, y) then 0w1 else 0w0), n-0w1) end
       and yl y = if y < h then xl (0, y, 0w0, 0w8) else ()
   in app print ["P4\n", Int.toString h, " ", Int.toString w, "\n"] ; yl 0 end

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 600

val _ = mandel (n, n)
