(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature HASH_TABLE =
   sig
      type ('a, 'b) t

      val fold: ('a, 'b) t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val foreach: ('a, 'b) t * ('a * 'b -> unit) -> unit
      val insert: ('a, 'b) t * 'a * 'b -> unit
      val lookup: ('a, 'b) t * 'a -> 'b
      val lookupOrInsert: ('a, 'b) t * 'a * (unit -> 'b) -> 'b
      val new: {equals: 'a * 'a -> bool,
                hash: 'a -> word,
                size: int} -> ('a, 'b) t
      val peek: ('a, 'b) t * 'a -> 'b option
     val size: ('a, 'b) t -> int
   end

(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure HashTable: HASH_TABLE =
struct

structure Buckets =
   struct
      datatype ('a, 'b) t =
	 Cons of {hash: word,
		  key: 'a,
		  rest: ('a, 'b) t,
		  value: 'b}
       | Nil

      val empty = Nil

      val fold: ('a, 'b) t * 'c * (word * 'a * 'b * 'c -> 'c) -> 'c =
	 fn (bucket, c, f) =>
	 let
	    fun loop (bucket, c) =
	       case bucket of
		  Cons {hash, key, rest, value} =>
		     loop (rest, f (hash, key, value, c))
		| Nil => c
	 in
	    loop (bucket, c)
	 end

      val foreach: ('a, 'b) t * (word * 'a * 'b -> unit) -> unit =
	 fn (bucket, f) =>
	 fold (bucket, (), fn (w, a, b, ()) => f (w, a, b))
   end

datatype ('a, 'b) t =
   T of {buckets: ('a, 'b) Buckets.t array ref,
	 equals: 'a * 'a -> bool,
	 hash: 'a -> word,
	 mask: word ref,
	 numItems: int ref}

fun new {equals, hash, size} =
   let
      val numBuckets = 0w4 * Word.roundUpToPowerOfTwo (Word.fromInt size)
      val mask: word = numBuckets - 0w1
   in
      T {buckets = ref (Array.new (Word.toInt numBuckets, Buckets.empty)),
	 equals = equals,
	 hash = hash,
	 numItems = ref 0,
	 mask = ref mask}
   end

fun size (T {numItems, ...}) = !numItems

fun index (w: word, mask: word): int =
   Word.toInt (Word.andb (w, mask))

fun resize (T {buckets, hash, mask, ...}, size: int, newMask: word): unit =
   let
      val newBuckets = Array.new (size, Buckets.empty)
   in
      Array.foreach (!buckets, fn r =>
		     Buckets.foreach (r, fn (hash, key, value) =>
				      let
					 val j = index (hash, newMask)
				      in
					 Array.update
					 (newBuckets, j,
					  Buckets.Cons
					  {hash = hash,
					   key = key,
					   rest = Array.sub (newBuckets, j),
					   value = value})
				      end))
      ; buckets := newBuckets
      ; mask := newMask
   end

fun maybeGrow (s as T {buckets, mask, numItems, ...}): unit =
   let
      val n = Array.length (!buckets)
   in
      if !numItems * 4 >= n
	 then resize (s,
		      n * 2,
		      (* The new mask depends on growFactor being 2. *)
		      Word.orb (0w1, Word.<< (!mask, 0w1)))
      else ()
   end

fun peekGen (T {buckets = ref buckets, equals, hash, mask, ...}, a, no, yes) =
   let
      val w = hash a
      val j = index (w, !mask)
      val b = Array.sub (buckets, j)
      val rec peek =
	 fn Buckets.Nil => no (w, j, b)
	  | Buckets.Cons {hash, key, rest, value} =>
	       if hash = w andalso equals (key, a)
		  then yes value
	       else peek rest
   in
      peek b
   end

fun peek (t, a) = peekGen (t, a, fn _ => NONE, SOME)

fun lookupOrInsert (table as T {buckets, numItems, ...}, key, makeValue) =
   let
      fun no (hash, j, bucket) =
	 let
	    val value = makeValue ()
	    val _ = Int.inc numItems
	    val _ = Array.update (!buckets, j,
				  Buckets.Cons {hash = hash,
						key = key,
						rest = bucket,
						value = value})
	    val _ = maybeGrow table
	 in
	    value
	 end
      fun yes value = value
   in
      peekGen (table, key, no, yes)
   end

fun insert (t, a, b) =
   ignore (lookupOrInsert (t, a, fn () => b))

fun lookup (t, a) =
   lookupOrInsert (t, a, fn () => raise Fail "HashTable.insert")

fun fold (T {buckets, ...}, c, f) =
   Array.fold (!buckets, c, fn (bucket, c) =>
	       Buckets.fold (bucket, c, fn (_, a, b, c) => f (a, b, c)))

fun foreach (T {buckets, ...}, f) =
   Array.foreach (!buckets, fn b =>
		  Buckets.foreach (b, fn (_, a, b) => f (a, b)))

end
