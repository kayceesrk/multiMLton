(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Layout: LAYOUT =
struct

val detailed = ref false

fun switch {detailed = d,normal = n} x =
   if !detailed then d x else n x

datatype t = T of {length: int,
                   tree: tree}
and tree =
   Empty
  | String of string
  | Sequence of t list
  | Align of {force: bool, rows: t list}
  | Indent of t * int

fun length (T {length, ...}) = length

val empty = T {length = 0, tree = Empty}

fun isEmpty (T {length = 0, ...}) = true
  | isEmpty _ = false

fun str s =
   case s of
      "" => empty
    | _ => T {length = String.size s, tree = String s}

fun fold (l, b, f) = foldl f b l

fun seq ts =
   let val len = fold (ts, 0, fn (t,n) => n + length t)
   in case len of
      0 => empty
    | _ => T {length = len, tree = Sequence ts}
   end

fun toString t =
   let
      fun loop (T {tree, ...}, accum) =
         case tree of
            Empty => accum
          | String s => s :: accum
          | Sequence ts => fold (ts, accum, loop)
          | Align {rows, ...} =>
               (case rows of
                   [] => accum
                 | t :: ts =>
                      fold (ts, loop (t, accum), fn (t, ac) =>
                            loop (t, " " :: ac)))
          | Indent (t, _) => loop (t, accum)
   in
      String.concat (rev (loop (t, [])))
   end

end
