(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Switch (S: SWITCH_STRUCTS): SWITCH =
struct

open S

fun isRedundant {cases: 'a vector,
                 equals: 'a * 'a -> bool}: bool =
   let
      val nCases = Vector.length cases
   in
      0 < nCases
      andalso let
                 fun loop (i: int, prev: 'a): bool =
                    i < nCases
                    andalso let
                               val cur = Vector.sub (cases, i)
                            in
                               equals (cur, prev)
                               orelse loop (i + 1, cur)
                            end
              in
                 loop (1, Vector.sub (cases, 0))
              end
   end

datatype t =
   T of {cases: (WordX.t * Label.t) vector,
         default: Label.t option,
         size: WordSize.t,
         test: Use.t}

fun layout (T {cases, default, test, ...})=
   let
      open Layout
   in
      seq [str "switch ",
           record [("test", Use.layout test),
                   ("default", Option.layout Label.layout default),
                   ("cases",
                    Vector.layout (Layout.tuple2 (WordX.layout, Label.layout))
                    cases)]]
   end

fun isOk (T {cases, default, size = _, test}, {checkUse, labelIsOk}): bool =
   let
      val () = checkUse test
      val ty = Use.ty test

      fun checkCond f s =
      let
        val r = f ()
        val _ = if (not r) then print s else ()
      in
        r
      end

      val res =
        checkCond (fn () => Vector.forall (cases, labelIsOk o #2)) "C1\n"
        andalso
        checkCond (fn () => case default of
                    NONE => true
                  | SOME l => labelIsOk l) "C2\n"
        andalso
        checkCond (fn () => Vector.isSorted (cases, fn ((w, _), (w', _)) =>
                                WordX.le (w, w', {signed = false}))) "C3\n"
        andalso
        checkCond (fn () => not (isRedundant
                    {cases = cases,
                      equals = fn ((w, _), (w', _)) => WordX.equals (w, w')})) "C4\n"
        andalso
        if 0 = Vector.length cases
          then checkCond (fn () => isSome default) "C5\n"
        else
          let
              val casesTy =
                Type.sum (Vector.map (cases, fn (w, _) => Type.ofWordX w))
          in
              checkCond (fn () => Bits.equals (Type.width ty, Type.width casesTy)) "C6\n"
              andalso
              checkCond (fn () => not (Type.isObjptr ty)) "C7\n"
              andalso
              checkCond (fn () => isSome default orelse Type.isSubtype (ty, casesTy)) "C8\n"
          end
   in
     res
   end

fun foldLabelUse (T {cases, default, test, ...}, a: 'a, {label, use}): 'a =
   let
      val a = use (test, a)
      val a = Option.fold (default, a, label)
      val a = Vector.fold (cases, a, fn ((_, l), a) =>
                           label (l, a))
   in
      a
   end

fun foreachLabel (s, f) =
   foldLabelUse (s, (), {label = f o #1,
                         use = fn _ => ()})

end
