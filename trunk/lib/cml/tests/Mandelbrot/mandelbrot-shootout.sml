(*   The Computer Language Benchmarks Game
 *   http://shootout.alioth.debian.org/
 *
 *   written by The multiMLton@purdue group
 *)

open MLton.PCML

structure Mandelbrot=
struct

  (* definitions *)
  val word8_zero = 0w8
  val word8_one = Word8.fromInt 1
  val word_one = Word.fromInt 1
  val word_seven = Word.fromInt 7
  val word_eight = Word.fromInt 8

  fun println s = print (s^"\n")


  (* testing parameters *)
  val numberOfWorkers = Aux.numberOfProcessors

  val counter = ref 0

  fun doit' (const_n) =
  let

    (* Benchmark parameters *)
    val size = const_n
    val word_size = Word.fromInt size
    val inverse_N = 2.0 / (real size)
    val width_bytes = (size div 8) + 1

    val output = Array2.array(size, width_bytes, word8_zero)
    val bytes_per_line = Array.array(size, 0)

    fun work () =
    let
      val y = MutexLock.fetchAndAdd (counter, 1)
    in
      if (y >= const_n) then ()
      else
        (* Actual computation follows *)
        let
          val civ = (real y * inverse_N) - 1.0

          fun loop1 (byte_accumulate, bit_num, byte_count, x) =
            if (x=const_n) then (byte_accumulate, bit_num, byte_count)
            else
              let
                val crv = (real x * inverse_N) - 1.5
                fun loop2 (zrv: real, ziv: real, trv: real, tiv: real, i) =
                  if (trv+tiv) > 4.0 orelse (i=0) then i
                  else
                    let
                      val ziv = (zrv * ziv) + (zrv * ziv) + civ
                      val zrv = trv - tiv + crv
                      val trv = zrv * zrv
                      val tiv = ziv * ziv
                    in
                      loop2 (ziv, zrv, trv, tiv, i-1)
                    end
                (* loop2 ends *)
                val i = loop2 (crv,civ,crv*crv,civ*civ, 50)
                val byte_accumulate = Word8.<<(byte_accumulate, word_one)
                val byte_accumulate = if i=0 then (byte_accumulate + word8_one) else byte_accumulate
                val bit_num = bit_num + 1
              in
                if (bit_num) = 8 then
                  (Array2.update(output, y, byte_count, byte_accumulate)
                  ; loop1 (word8_zero, 0, byte_count+1, x+1))
                else
                  loop1 (byte_accumulate, bit_num, byte_count, x+1)
              end
          (* loop1 ends *)
          val (byte_accumulate, bit_num, byte_count) = loop1 (word8_zero, 0, 0, 0) (* start loop1 *)
        in
          if not (bit_num = 0) then
            (Array2.update (output, y, byte_count, Word8.<<(byte_accumulate, (word_eight - Word.andb(word_size, word_seven))))
            ; Array.update (bytes_per_line, y, byte_count + 1)
            ; work ())
          else
            (Array.update (bytes_per_line, y, byte_count)
            ; work ())
        end
        (* computation ends *)
    end

    fun startWorker n =
      if n=0 then ()
      else (spawn (work); startWorker (n-1))

    val _ = startWorker (numberOfWorkers - 1)
    val _ = work ()

    (* output *)
    val stdout = BinIO.openOut ("stdOut")
    fun out b = BinIO.output1 (stdout, b)

    fun loop1 s =
      if s = size then BinIO.closeOut (stdout)
      else
        let
          val maxbpl = Array.sub (bytes_per_line, s)
          fun loop2 bpl =
            if (bpl = maxbpl) then ()
            else
              (out (Array2.sub (output, s, bpl))
              ; loop2 (bpl+1))
          val _ = loop2 0
        in
          loop1 (s+1)
        end

    (* Begin printing *)
    val _ = Vector.app out (Byte.stringToBytes ("P4\n"^(Int.toString size)^" "^(Int.toString size)^"\n"))
    val _ = loop1 0
  in
    ()
  end

  fun doit n = MLton.RunPCML.doit (fn () =>
  let
    val ts = Time.now ()
    val _ = doit' n
    val te = Time.now ()
    val d = Time.-(te, ts)
    val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
    val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
    val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
  in
    ()
  end, NONE)
end

val n =
   case CommandLine.arguments () of
      [] => 200
    | s::_ => (case Int.fromString s of
                  NONE => 200
                | SOME n => n)

val _ = Mandelbrot.doit n
