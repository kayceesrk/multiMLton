(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure GetSet =
   struct
      type 'a t = (unit -> 'a) * ('a -> unit)
   end

structure PreThread :> sig type t end = struct type t = Thread.t end
structure Thread :> sig type t end = struct type t = Thread.t end

(**************************************************************************)

structure Bool =
   struct
      open Bool
      fun not b = if b then false else true
   end

structure Controls =
   struct
      val debug = _command_line_const "MLton.debug": bool = false;
      val detectOverflow = _command_line_const "MLton.detectOverflow": bool = true;
      val safe = _command_line_const "MLton.safe": bool = true;
      val bufSize = _command_line_const "TextIO.bufSize": Int32.int = 4096;

      val readBarrier = _command_line_const "MLton.GC.readBarrier": bool = true;
      val wbUsesCleanliness = _command_line_const "MLton.GC.WB.useCleanliness": bool = true;
      val lazySpawn = _command_line_const "MLton.GC.WB.lazySpawn": bool = true;
   end

structure Exn =
   struct
      open Exn

      val name = _prim "Exn_name": exn -> String8.string;

      exception Div
      exception Domain
      exception Fail8 of String8.string
      exception Fail16 of String16.string
      exception Fail32 of String32.string
      exception Overflow
      exception Size
      exception Subscript

      val wrapOverflow: ('a -> 'b) -> ('a -> 'b) =
         fn f => fn a => f a handle PrimOverflow => raise Overflow
   end

structure Order =
   struct
      datatype t = LESS | EQUAL | GREATER
      datatype order = datatype t
   end

structure Option =
   struct
      datatype 'a t = NONE | SOME of 'a
      datatype option = datatype t
   end

structure Lwtgc =
struct
  val addToPreemptOnWBA = _prim "Lwtgc_addToPreemptOnWBA": 'a * Int32.int -> unit;
  val addToMoveOnWBA = _prim "Lwtgc_addToMoveOnWBA": 'a -> unit;
  val addToSpawnOnWBA = _prim "Lwtgc_addToSpawnOnWBA": 'a * Int32.int -> unit;
  val isObjptrInLocalHeap = _prim "Lwtgc_isObjptrInLocalHeap": 'a -> bool;
  val isObjptrInSharedHeap = _prim "Lwtgc_isObjptrInSharedHeap": 'a -> bool;
  val isObjectClosureClean = _prim "Lwtgc_isObjectClosureClean": 'a -> bool;
  val isThreadClosureClean = _prim "Lwtgc_isThreadClosureClean": 'a -> bool;
  val isObjptr = _prim "Lwtgc_isObjptr": 'a -> bool;
  val needPreemption = _prim "Lwtgc_needPreemption": 'a ref * 'a -> bool;
  val move = _prim "MLton_move": 'a * bool * bool -> 'a;
  val move2 = _prim "MLton_move2": 'a * bool * bool -> 'a;
end

structure Ref =
   struct
      open Ref

      exception RefFail

      val ffiPrint = _import "GC_print": Int32.int -> unit;
      val preemptFn = ref (fn () => ())
      val deref = _prim "Ref_deref": 'a ref -> 'a;
      val refAssign = _prim "Ref_assign": 'a ref * 'a * bool -> unit;
      val eq = _prim "MLton_eq": 'a * 'a -> bool;

      fun writeBarrierRef (r, v) =
        (if (Controls.readBarrier) then
          refAssign (r, v, false)
        else
          let
            val preemptFn = deref preemptFn
            val needsMove =
              if (Lwtgc.isObjptr v) andalso
                 (Lwtgc.isObjptrInLocalHeap v) andalso
                 (Lwtgc.isObjptrInSharedHeap r) then
                (if Controls.wbUsesCleanliness andalso
                    Lwtgc.isObjectClosureClean v then
                      true
                else
                  (Lwtgc.addToMoveOnWBA v;
                   preemptFn ();
                   false))
              else false
          in
            refAssign (r, v, needsMove)
          end)

      fun assign (r, v) = writeBarrierRef (r, v)
      fun unsafeAssign (r,v) = refAssign (r, v, false)
   end


structure TopLevel =
   struct
      val getHandler = _prim "TopLevel_getHandler": unit -> (exn -> unit);
      val getSuffix = _prim "TopLevel_getSuffix": unit -> (unit -> unit);
      val setHandler = _prim "TopLevel_setHandler": (exn -> unit) -> unit;
      val setSuffix = _prim "TopLevel_setSuffix": (unit -> unit) -> unit;
   end

end

val not = Primitive.Bool.not

exception Bind = Primitive.Exn.Bind
exception Div = Primitive.Exn.Div
exception Domain = Primitive.Exn.Domain
exception Match = Primitive.Exn.Match
exception Overflow = Primitive.Exn.Overflow
exception Size = Primitive.Exn.Size
exception Subscript = Primitive.Exn.Subscript

datatype option = datatype Primitive.Option.option
datatype order = datatype Primitive.Order.order

infix 4 = <>
val op = = _prim "MLton_equal": ''a * ''a -> bool;
val op <> = fn (x, y) => not (x = y)
