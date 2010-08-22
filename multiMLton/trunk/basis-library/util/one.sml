(* Copyright (C) 2006-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure One:
   sig
      type 'a t

      val make: (unit -> 'a) -> 'a t
      val use: 'a t * ('a -> 'b) -> 'b
   end =
   struct
      datatype 'a t = T of {more: unit -> 'a,
                            static: 'a,
                            staticIsInUse: int ref}

      fun make f = T {more = f,
                      static = f (),
                      staticIsInUse = ref 0}

      fun use (T {more, static, staticIsInUse}, f) =
      let
        val free = Primitive.MLton.Parallel.compareAndSwap (staticIsInUse, 0, 1)
        val d = if free then
                    static
                else
                  more ()
      in
        DynamicWind.wind (fn () => f d,
                          fn () => if free then
                                    ignore (Primitive.MLton.Parallel.compareAndSwap (staticIsInUse, 1, 0))
                                   else
                                     ())
      end
   end
