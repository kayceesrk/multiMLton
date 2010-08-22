(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PropertyList (H: HET_CONTAINER):> PROPERTY_LIST =
struct

datatype t = T of H.t list ref

fun new (): t = T (ref [])

val equals = fn (T r, T r') => (r = r')

val numPeeks = ref 0
val numLinks = ref 0
val maxLength: int ref = ref 0

fun 'a newProperty () =
   let
      val {make, pred, peek = peekH} = H.new ()
      fun peek (T hs) =
         let
            fun loop (l, n) =
               let
                  fun update () =
                     ((numLinks := n + !numLinks
                       handle Overflow => raise Fail "PropertyList: numLinks overflow")
                      ; if n > !maxLength
                           then maxLength := n
                        else ())
               in case l of
                  [] => (update (); NONE)
                | e :: l =>
                     case peekH e of
                        r as SOME _ => (update (); r)
                      | NONE => loop (l, n + 1)
               end
            val _ =
               numPeeks := 1 + !numPeeks
               handle Overflow => raise Fail "PropertyList: numPeeks overflow"
         in
            loop (!hs, 0)
         end


      fun fold (l, b, f) =
         let
            fun loop (l, b) =
               case l of
                  [] => b
                | x :: l => loop (l, f (x, b))
         in loop (l, b)
         end
      fun appendRev (l, l') = fold (l, l', op ::)
      fun add (T hs, v: 'a): unit = hs := make v :: !hs
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


      fun remove' (l, f) = remFst (l, f, fn () => l)
      fun remove (T hs) = hs := remove' (!hs, pred)
   in
      {add = add, peek = peek, remove = remove}
   end


end
