(*
 *
 * The Great Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Sebastien Loisel
 * Cleanup by Troestler Christophe
 * Translated to SML by sweeks@sweeks.com
 *)

open MLton.Pacml

fun for (start, stop, f) =
let
  fun loop i =
    if i = stop
    then ()
     else (f i; loop (i + 1))
in
  loop start
end

signature BARRIER =
sig
  type barrier
  val newBarrier : int -> barrier
  val await      : barrier -> unit
end

structure Barrier : BARRIER =
struct
  type barrier = (unit chan * unit chan)

  fun newBarrier count =
  let
    val ic = channel ()
    val oc = channel ()
    fun barrierCore () =
    let
      val _ = for (0, count, fn _ => recv ic)
      val _ = for (0, count, fn _ => aSend (oc, ()))
    in
      barrierCore ()
    end
    val _ = spawnOnProc (barrierCore,0)
  in
    (ic, oc)
  end

  fun await (ic, oc) =
  let
    val _ = aSend (ic, ())
    val _ = recv oc
  in ()
  end
end

fun eval_A (i, j) = 1.0 / Real.fromInt ((i+j)*(i+j+1) div 2+i+1)

fun eval_A_times_u (u, v, offset) =
  for (0, ArraySlice.length v, fn i =>
	  (ArraySlice.update (v, i, 0.0)
	   ; for (0, Array.length u, fn j =>
		     ArraySlice.update (v, i, ArraySlice.sub (v, i) +
                                  eval_A (offset + i, j) * Array.sub (u, j)))))

fun eval_At_times_u (u, v, offset) =
  for (0, ArraySlice.length v, fn i =>
    (ArraySlice.update (v, i, 0.0)
     ; for (0, Array.length u, fn j =>
         ArraySlice.update (v, i, ArraySlice.sub (v, i) +
                                  eval_A (j, offset + i) * Array.sub (u, j)))))

fun eval_AtA_times_u (u, w, v, barrier, offset, length) =
  ( eval_A_times_u (u, ArraySlice.slice (w, offset, SOME length), offset)
  ; Barrier.await barrier
  ; eval_At_times_u (w, ArraySlice.slice (v, offset, SOME length), offset)
  ; Barrier.await barrier)

val (n, nt) =
  case CommandLine.arguments () of
     s1::s2::_ => (case (Int.fromString s1,
                         Int.fromString s2) of
                        (SOME n1, SOME n2) => (n1, n2)
                      | _ => (1, 1))
   | _ => (1, 1)

val u = Array.array (n, 1.0)
val v = Array.array (n, 0.0)
val w = Array.array (n, 0.0)

fun preamble () =
let
  val barrier = Barrier.newBarrier nt
  val chunkSize = (n + nt - 1) div nt

  fun spawnClient tid =
  let
    val startIndex = tid * chunkSize
    val sliceSize = Int.min (chunkSize, n - startIndex)
    fun clientCore () =
      (Barrier.await barrier;
      for (0, 10, fn _ => (eval_AtA_times_u (u, w, v, barrier, startIndex, sliceSize);
                           eval_AtA_times_u (v, w, u, barrier, startIndex, sliceSize))))
  in
    if tid = nt - 1
    then clientCore ()
    else ignore (spawn clientCore)
  end

  val _ = for (0, nt, spawnClient)
in
  ()
end

fun epilogue () =
let
  val vv = ref 0.0
  val vBv = ref 0.0
  val () =
    for (0, n, fn i =>
    (vv := !vv + Array.sub (v, i) * Array.sub (v, i)
    ; vBv := !vBv + Array.sub (u, i) * Array.sub (v, i)))

  val () = print (concat [Real.fmt (StringCvt.FIX (SOME 9))
        (Real.Math.sqrt (!vBv / !vv)),
        "\n"])
in
  OS.Process.exit OS.Process.success
end

val _ = run (epilogue o preamble)
