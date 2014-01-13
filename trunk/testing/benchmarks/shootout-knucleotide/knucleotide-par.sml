(* knucleotide.ml
 *
 * The Great Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Troestler Christophe
 * translated to MLton/SML by sweeks@sweeks.com.
 *)

open MLton.Pacml

fun for (start, stop, inc, f) =
let
  fun loop i =
    if i >= stop
    then ()
     else (f i; loop (i + inc))
in
  loop start
end


structure H = HashTable
(* [counts k dna] fills and return the hashtable [count] of
 * k-nucleotide keys and count values for a particular reading-frame
 * of length [k] of the string [dna].
 *)
fun counts (k, dna, nt) =
let
  val n = size dna + 1 - k
  fun threadCore (ic, oc, start) =
  let
    val count = H.new {equals = op =, hash = String.hash, size = 0x20000}
    val () =
      for (start, n, nt, fn i =>
            Int.inc
            (H.lookupOrInsert (count, String.substring (dna, i, k),
            fn () => ref 0)))
    val prevCount = recv ic
    val () = H.foreach (prevCount, fn (k,v) => Int.add (!v) (H.lookupOrInsert (count, k, fn _ => ref 0)))
  in
    send (oc, count)
  end

  val initIC = channel ()
  val emptyHT = H.new {equals = op =, hash = String.hash, size = 0x1}
  val _ = aSend (initIC, emptyHT)

  fun spawnClients inputChannel tid =
    if tid = nt then inputChannel
    else
      let
        val outputChannel = channel ()
        val _ = spawn (fn () => threadCore (inputChannel, outputChannel, tid))
      in
        spawnClients outputChannel (tid+1)
      end

  val resChan = spawnClients initIC 0
  val result = recv resChan
in
  (n, result)
end

(* [write_frequencies k dna] writes the frequencies for a
 * reading-frame of length [k] sorted by descending frequency and then
 * ascending k-nucleotide key.
 *)
fun compareFreq ((k1:string, f1:real), (k2, f2)) =
   f1 > f2 orelse (Real.== (f1, f2) andalso String.<= (k1, k2))

fun writeFrequencies (k, dna, nt) =
  let
     val (n, cnt) = counts (k, dna, nt)
     val tot = Real.fromInt n
     val frq =
	Array.fromList (H.fold (cnt, [], fn (k, r, l) =>
				(k, 100.0 * Real.fromInt (!r) / tot) :: l))
     val _ = QuickSort.sortArray (frq, compareFreq)
     val () =
	Array.foreach (frq, fn (k, f) =>
		       print (concat [k, " ",
				      Real.fmt (StringCvt.FIX (SOME 3)) f,
				      "\n"]))
  in
     print "\n"
  end

fun writeCount (seq, dna, nt) =
  let
     val (_, cnt) = counts (size seq, dna, nt)
  in
     print (concat [Int.toString (case H.peek (cnt, seq) of
				     NONE => 0
				   | SOME r => !r),
		    "\t", seq, "\n"])
  end

signature BUFFER =
sig
  type t

  val addLine: t * string -> t
  val contents: t -> char array
  val empty: t
end

structure Buffer : BUFFER =
   struct
      datatype t = T of string list

      val empty: t = T []

      fun addLine (T xs, x) = T (x :: xs)

      fun contents (T lines) =
	 let
	    val n = foldl (fn (s, n) => n + size s - 1) 0 lines
	    val a = Array.array (n, #"\000")
	    val _ =
	       foldl (fn (s, i) =>
		      let
			 fun loop (i, j)  =
			    if j < 0
			       then i
			    else (Array.update (a, i, String.sub (s, j))
				  ; loop (i - 1, j - 1))
		      in
			 loop (i, String.size s - 2)
		      end)
	       (n - 1) lines
	 in
	    a
	 end
   end

(* Extract DNA sequence "THREE" from stdin *)
val dnaThree =
  let
     fun line () = TextIO.inputLine TextIO.stdIn
     fun isThree s = String.substring (s, 0, 6) = ">THREE"
     val () = while not (isThree (valOf (line ()))) do ()
     fun loop () =
	let
	   val l = valOf (line ())
	in
	   if #";" = String.sub (l, 0)  (* Skip possible comment *)
	      then loop ()
	   else
	      let
		 fun loop (b, l) =
		    let
		       val b = Buffer.addLine (b, l)
		    in
		       case line () of
			  NONE => b
			| SOME l =>
			     if #"<" = String.sub (l, 0)
				then b
			     else loop (b, l)
		    end
	      in
		 loop (Buffer.empty, l)
	      end
	end
     val a = Buffer.contents (loop ())
  in
     String.toUpper
     (String.tabulate (Array.length a, fn i => Array.sub (a, i)))
  end

val nt = numberOfProcessors ()

fun main () =
let
  val () = writeFrequencies (1, dnaThree, nt)
  val () = writeFrequencies (2, dnaThree, nt)
  val () =
    List.foreach
    (["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"],
      fn k => writeCount (k, dnaThree, nt))
in
  ()
end

val _ = run main
