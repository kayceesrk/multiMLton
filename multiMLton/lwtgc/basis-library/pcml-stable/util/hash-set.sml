(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure HashSet: HASH_SET =
struct

datatype 'a t =
   T of {buckets: 'a list array ref,
         hash: 'a -> word,
         mask: word ref,
         numItems: int ref}

fun 'a newWithBuckets {hash, numBuckets: int}: 'a t =
   let
      val mask: word = Word.- (Word.fromInt numBuckets, 0w1)
   in
      T {buckets = ref (Array.tabulate (numBuckets, fn  _ => [])),
         hash = hash,
         numItems = ref 0,
         mask = ref mask}
   end

val initialSize: int = 64

fun new {hash} = newWithBuckets {hash = hash,
                                 numBuckets = initialSize}

fun newOfSize {hash, size} =
   newWithBuckets {hash = hash,
                   numBuckets = 4 * size}

fun size (T {numItems, ...}) = !numItems

fun index (w: word, mask: word): int =
   Word.toInt (Word.andb (w, mask))

val numPeeks = ref 0
val numLinks = ref 0

fun resize (T {buckets, hash, mask, ...}, size: int, newMask: word): unit =
   let
      val newBuckets = Array.tabulate (size, fn _ => [])
   in (Array.app (fn r =>
                     (List.app (fn a =>
                                   let val j = index (hash a, newMask)
                                   in ignore (Array.update
                                      (newBuckets, j,
                                       a :: Array.sub (newBuckets, j)))
                                   end) r)) (!buckets))
      ; buckets := newBuckets
      ; mask := newMask
   end

fun maybeGrow (s as T {buckets, mask, numItems, ...}): unit =
   let
      val n = Array.length (!buckets)
   in if !numItems * 4 > n
         then resize (s,
                      n * 2,
                      (* The new mask depends on growFactor being 2. *)
                      Word.orb (0w1, Word.<< (!mask, 0w1)))
      else ()
   end

fun listfold (l, b, f) =
         let
                                fun loop (l, b) =
                                                 case l of
                                                                        [] => b
                                                                                        | x :: l => loop (l, f (x, b))
                                                                                                 in loop (l, b)
                                                                                                          end

  fun appendRev (l, l') = listfold (l, l', op ::)
  fun rev l = appendRev (l, [])
  fun remFst (l, f, notFound) =
       let
               fun loop (l, ac) =
                          case l of
                                           [] => notFound ()
                                                     | x :: l =>
                                                                        if f x
                                                                                          then appendRev (ac, l)
                                                                                                         else loop (l, x :: ac)
                                                                                                            in loop (l, [])
                                                                                                               end

  fun removeFirst (l, f) = remFst (l, f, fn () => raise Fail "List.removeFirst")

fun remove (T {buckets, mask, numItems, ...}, w, p) =
   let
      val i = index (w, !mask)
      val b = !buckets
      val _ = Array.update (b, i, removeFirst (Array.sub (b, i), p))
      val _ = numItems := !numItems - 1
   in
      ()
   end

fun peek' (l, f) =
   let
      fun loop l =
         case l of
            [] => NONE
          | x :: l => if f x then SOME x else loop l
   in
      loop l
   end

fun peekGen (T {buckets = ref buckets, mask, ...}, w, p, no, yes) =
   let
      val _ =
         numPeeks := 1 + !numPeeks
         handle Overflow => raise Fail "HashSet: numPeeks overflow"
      val j = index (w, !mask)
      val b = Array.sub (buckets, j)
      fun update () =
         numLinks := !numLinks + 1
         handle Overflow => raise Fail "HashSet: numLinks overflow"
   in case peek' (b, fn a => (update (); p a)) of
      NONE => no (j, b)
    | SOME a => yes a
   end

fun peek (t, w, p) = peekGen (t, w, p, fn _ => NONE, SOME)

(* fun update (T {buckets = ref buckets, equals, hash, mask, ...}, a) =
 *    let
 *       val j = index (hash a, !mask)
 *       val _ =
 *       Array.update (buckets, j,
 *                     a :: (List.remove (Array.sub (buckets, j),
 *                                        fn a' => equals (a, a'))))
 *    in ()
 *    end
 *)

fun insertIfNew (table as T {buckets, numItems, ...}, w, p, f,
                 g: 'a -> unit) =
   let
      fun no (j, b) =
         let val a = f ()
            val _ = numItems := !numItems + 1
            val _ = Array.update (!buckets, j, a :: b)
            val _ = maybeGrow table
         in a
         end
      fun yes x = (g x; x)
   in peekGen (table, w, p, no, yes)
   end


  fun fold' (l, b, f) = Array.foldl f b l

fun fold (T {buckets, ...}, b, f) =
   fold' (!buckets, b, fn (r, b) => listfold (r, b, f))

end
