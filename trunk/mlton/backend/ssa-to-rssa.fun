(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SsaToRssa (S: SSA_TO_RSSA_STRUCTS): SSA_TO_RSSA =
struct

open S
open Rssa

datatype z = datatype WordSize.prim

structure S = Ssa

local
   open Ssa
in
   structure Base = Base
end

local
   open Runtime
in
   structure GCField = GCField
end

structure Prim =
   struct
      open Prim

      type t = Type.t Prim.t
   end

structure CFunction =
   struct
      open CFunction
      open Type.BuiltInCFunction

      type t = Type.t CFunction.t

      structure CType =
         struct
            open CType
            val gcState = CPointer
            val thread = CPointer (* CHECK; thread (= objptr) would be better? *)
         end

      datatype z = datatype Convention.t
      datatype z = datatype SymbolScope.t
      datatype z = datatype Target.t

      val copyCurrentThread = fn () =>
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new1 CType.gcState, NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_copyCurrentThread",
            writesStackTop = true}

      (* CHECK; thread as objptr *)
      val copyThread = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.thread ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new2 (CPointer, CPointer), SOME CPointer)
                        end,
            readsStackTop = true,
            return = Type.thread (),
            symbolScope = Private,
            target = Direct "GC_copyThread",
            writesStackTop = true}

      val halt = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new2 (CType.gcState, CType.cint ()), NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "MLton_halt",
            writesStackTop = true}

      val ffiGetOpArgsResPtr = fn () =>
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new1 CType.gcState, SOME CType.cpointer),
            readsStackTop = false,
            return = Type.cpointer (),
            symbolScope = Private,
            target = Direct "FFI_getOpArgsResPtr",
            writesStackTop = false}

      fun gcArrayAllocate {return} =
         T {args = Vector.new4 (Type.gcState (),
                                Type.csize (),
                                Type.seqIndex (),
                                Type.objptrHeader ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = true,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new4 (CType.gcState,
                                      CType.csize (),
                                      CType.seqIndex (),
                                      CType.objptrHeader ()),
                         SOME CType.objptr),
            readsStackTop = true,
            return = return,
            symbolScope = Private,
            target = Direct "GC_arrayAllocate",
            writesStackTop = true}

      val returnToC = fn () =>
         T {args = Vector.new0 (),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = true,
            modifiesFrontier = true,
            prototype = (Vector.new0 (), NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "Thread_returnToC",
            writesStackTop = true}

      (* CHECK; thread as objptr *)
      val threadSwitchTo = fn () =>
         T {args = Vector.new3 (Type.gcState (), Type.thread (), Type.csize ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = true,
            mayGC = true,
            maySwitchThreads = true,
            modifiesFrontier = true,
            prototype = (Vector.new3 (CType.gcState,
                                      CType.thread,
                                      CType.csize ()),
                         NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_switchToThread",
            writesStackTop = true}

      val jumpDown = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = true,
            modifiesFrontier = true,
            prototype = (Vector.new2 (CType.gcState,
                                      CType.cint ()),
                         NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_jumpDown",
            writesStackTop = true}

      val prefixAndSwitchTo = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.thread ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = true,
            modifiesFrontier = true,
            prototype = (Vector.new2 (CType.gcState,
                                      CType.thread),
                         NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_prefixAndSwitchTo",
            writesStackTop = true}

      (* CHECK; weak as objptr *)
      fun weakCanGet {arg} =
         T {args = Vector.new2 (Type.gcState (), arg),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME CType.bool),
            readsStackTop = false,
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_weakCanGet",
            writesStackTop = false}

      (* CHECK; weak as objptr *)
      fun weakGet {arg, return} =
         T {args = Vector.new2 (Type.gcState (), arg),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME CType.cpointer),
            readsStackTop = false,
            return = return,
            symbolScope = Private,
            target = Direct "GC_weakGet",
            writesStackTop = false}

      (* CHECK; weak as objptr *)
      fun weakNew {arg, return} =
         T {args = Vector.new3 (Type.gcState (), Type.objptrHeader (), arg),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new3 (CType.gcState,
                                      CType.objptrHeader (),
                                      CType.cpointer),
                         SOME (CType.cpointer)),
            readsStackTop = true,
            return = return,
            symbolScope = Private,
            target = Direct "GC_weakNew",
            writesStackTop = true}

      val worldSave = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.string ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer), NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_saveWorld",
            writesStackTop = true}

      (* CHECK; share with objptr *)
      fun share t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true, (* MLton.share works by tracing an object.
                           * Make sure all the GC invariants are true,
                           * because tracing might encounter the current
                           * stack in the heap.
                           *)
            maySwitchThreads = false,
            modifiesFrontier = true, (* actually, just readsFrontier *)
            prototype = (Vector.new2 (CType.gcState, CType.cpointer), NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_share",
            writesStackTop = true}

      fun addToSpawnOnWBA t =
         T {args = Vector.new3 (Type.gcState (), t, Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new3 (CType.gcState, CType.cpointer, CType.cint ()),
                         NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_addToSpawnOnWBA",
            writesStackTop = true}

      fun addToMoveOnWBA t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer), NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_addToMoveOnWBA",
            writesStackTop = false}

      fun addToPreemptOnWBA t =
         T {args = Vector.new3 (Type.gcState (), t, Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new3 (CType.gcState, CType.cpointer, CType.cint ()),
                         NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_addToPreemptOnWBA",
            writesStackTop = false}

      fun score t =
         T {args = Vector.new3 (Type.gcState (), t, t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new3 (CType.gcState, CType.cpointer, CType.cpointer),
                         NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_score",
            writesStackTop = false}


      fun moveFromWB t =
         T {args = Vector.new4 (Type.gcState (), t, Type.bool, Type.bool),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new4 (CType.gcState, CType.cpointer, CType.bool, CType.bool),
                         SOME CType.cpointer),
            readsStackTop = true,
            return = t,
            symbolScope = Private,
            target = Direct "GC_moveFromWB",
            writesStackTop = true}

      fun move t =
         T {args = Vector.new4 (Type.gcState (), t, Type.bool, Type.bool),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new4 (CType.gcState, CType.cpointer, CType.bool, CType.bool),
                         SOME CType.cpointer),
            readsStackTop = true,
            return = t,
            symbolScope = Private,
            target = Direct "GC_move",
            writesStackTop = true}

      fun isInSharedOrForwarded t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME CType.bool),
            readsStackTop = false,
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_isInSharedOrForwarded",
            writesStackTop = false}

      fun isThreadClosureClean t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME CType.bool),
            readsStackTop = true,
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_isThreadClosureClean",
            writesStackTop = false}

      fun isObjectClosureClean t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME CType.bool),
            readsStackTop = true,
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_isObjectClosureClean",
            writesStackTop = false}


      fun sqAcquireLock () =
         T {args = Vector.new2 (Type.gcState (), Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cint ()), NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_sqAcquireLock",
            writesStackTop = false}

      fun sqReleaseLock () =
         T {args = Vector.new2 (Type.gcState (), Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cint ()), NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_sqReleaseLock",
            writesStackTop = false}

      fun sqCreateQueues () =
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new1 (CType.gcState), NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_sqCreateQueues",
            writesStackTop = false}

      fun sqClean () =
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new1 (CType.gcState), NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_sqClean",
            writesStackTop = false}

      fun sqIsEmpty () =
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new1 (CType.gcState),
                         SOME CType.bool),
            readsStackTop = true,
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_sqIsEmpty",
            writesStackTop = true}


      fun sqEnque t =
         T {args = Vector.new4 (Type.gcState (), t, Type.cint (), Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new4 (CType.gcState,
                                      CType.cpointer,
                                      CType.cint (),
                                      CType.cint ()),
                         NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_sqEnque",
            writesStackTop = false}

      fun sqDeque t =
         T {args = Vector.new2 (Type.gcState (), Type.cint ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState, CType.cint ()),
                         SOME CType.cpointer),
            readsStackTop = false,
            return = t,
            symbolScope = Private,
            target = Direct "GC_sqDeque",
            writesStackTop = false}

      fun parallelInit t =
         T {args = Vector.new0 (),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = true,
            modifiesFrontier = true,
            prototype = (Vector.new0 (), NONE),
            readsStackTop = true,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "Parallel_init",
            writesStackTop = true}

      (* CHECK; size with objptr *)
      fun size t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true, (* MLton.size works by tracing an object.
                           * Make sure all the GC invariants are true,
                           * because tracing might encounter the current
                           * stack in the heap.
                           *)
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME (CType.csize ())),
            readsStackTop = true,
            return = Type.csize (),
            symbolScope = Private,
            target = Direct "GC_size",
            writesStackTop = true}

      fun testSavedClosure () =
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new1 (CType.gcState),
                         SOME CType.bool),
            readsStackTop = false,
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_testSavedClosure",
            writesStackTop = false}

      fun getSavedClosure t =
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new1 (CType.gcState),
                         SOME CType.cpointer),
            readsStackTop = false,
            return = t,
            symbolScope = Private,
            target = Direct "GC_getSavedClosure",
            writesStackTop = false}

      fun setSavedClosure t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState,
                                      CType.cpointer),
                         NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_setSavedClosure",
            writesStackTop = false}

      fun testThreadId () =
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new1 (CType.gcState),
                         SOME CType.bool),
            readsStackTop = false,
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_testThreadId",
            writesStackTop = false}

      fun getThreadId t =
         T {args = Vector.new1 (Type.gcState ()),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new1 (CType.gcState),
                         SOME CType.cpointer),
            readsStackTop = false,
            return = t,
            symbolScope = Private,
            target = Direct "GC_getThreadId",
            writesStackTop = false}

      fun setThreadId t =
         T {args = Vector.new2 (Type.gcState (), t),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = false,
            prototype = (Vector.new2 (CType.gcState,
                                      CType.cpointer),
                         NONE),
            readsStackTop = false,
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_setThreadId",
            writesStackTop = false}



   end

structure Name =
   struct
      open Prim.Name

      type t = Type.t t

      structure CType =
         struct
            open CType
            val intInf = Objptr
            val string = Objptr
         end

      fun cFunctionRaise (n: t): CFunction.t =
         let
            datatype z = datatype CFunction.Convention.t
            datatype z = datatype CFunction.SymbolScope.t
            datatype z = datatype CFunction.Target.t
            val name = toString n
            val real = Type.real
            val word = Type.word
            val vanilla = CFunction.vanilla
            fun wordCType (s, sg) = CType.word (s, sg)
            fun realCType s = CType.real s
            fun coerce (t1, ct1, t2, ct2) =
               vanilla {args = Vector.new1 t1,
                        name = name,
                        prototype = (Vector.new1 ct1, SOME ct2),
                        return = t2}
            fun amAllocationProfiling () =
               Control.ProfileAlloc = !Control.profile
            val intInfBinary = fn () =>
               CFunction.T {args = Vector.new3 (Type.intInf (), Type.intInf (),
                                                Type.csize ()),
                            bytesNeeded = SOME 2,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = (Vector.new3 (CType.intInf, CType.intInf,
                                                      CType.csize ()),
                                         SOME CType.intInf),
                            readsStackTop = amAllocationProfiling (),
                            return = Type.intInf (),
                            symbolScope = Private,
                            target = Direct name,
                            writesStackTop = false}
            val intInfShift = fn () =>
               CFunction.T {args = Vector.new3 (Type.intInf (),
                                                Type.shiftArg,
                                                Type.csize ()),
                            bytesNeeded = SOME 2,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = (Vector.new3 (CType.intInf,
                                                      CType.shiftArg,
                                                      CType.csize ()),
                                         SOME CType.intInf),
                            readsStackTop = amAllocationProfiling (),
                            return = Type.intInf (),
                            symbolScope = Private,
                            target = Direct name,
                            writesStackTop = false}
            val intInfToString = fn () =>
               (* CHECK; cint would be better? *)
               CFunction.T {args = Vector.new3 (Type.intInf (),
                                                Type.word WordSize.word32,
                                                Type.csize ()),
                            bytesNeeded = SOME 2,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = (Vector.new3 (CType.intInf,
                                                      CType.Int32,
                                                      CType.csize ()),
                                         SOME CType.string),
                            readsStackTop = amAllocationProfiling (),
                            return = Type.string (),
                            symbolScope = Private,
                            target = Direct name,
                            writesStackTop = false}
            val intInfUnary = fn () =>
               CFunction.T {args = Vector.new2 (Type.intInf (), Type.csize ()),
                            bytesNeeded = SOME 1,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = (Vector.new2 (CType.intInf, CType.csize ()),
                                         SOME CType.intInf),
                            readsStackTop = amAllocationProfiling (),
                            return = Type.intInf (),
                            symbolScope = Private,
                            target = Direct name,
                            writesStackTop = false}
            local
               fun make n s =
                  let
                     val t = real s
                     val ct = CType.real s
                  in
                     vanilla {args = Vector.new (n, t),
                              name = name,
                              prototype = (Vector.new (n, ct), SOME ct),
                              return = t}
                  end
            in
               val realBinary = make 2
               val realTernary = make 3
               val realUnary = make 1
            end
            fun realCompare s =
               let
                  val t = real s
               in
                  vanilla {args = Vector.new2 (t, t),
                           name = name,
                           prototype = let
                                          val t = CType.real s
                                       in
                                          (Vector.new2 (t, t), SOME CType.bool)
                                       end,
                           return = Type.bool}
               end
            local
               fun make n (s, sg) =
                  let
                     val t = word s
                     val ct = CType.word (s, sg)
                  in
                     vanilla {args = Vector.new (n, t),
                              name = name,
                              prototype = (Vector.new (n, ct), SOME ct),
                              return = t}
                  end
               fun makeOverflows n (s, sg) =
                  let
                     val t = word s
                     val ct = CType.word (s, sg)
                  in
                     vanilla {args = Vector.new (n, t),
                              name = name ^ "Overflows",
                              prototype = (Vector.new (n, ct), SOME CType.bool),
                              return = Type.bool}
                  end
            in
               val wordBinary = make 2
               val wordBinaryOverflows = makeOverflows 2
               val wordUnary = make 1
               val wordUnaryOverflows = makeOverflows 1
            end
            fun wordCompare (s, sg) =
               let
                  val t = word s
                  val ct = CType.word (s, sg)
               in
                  vanilla {args = Vector.new2 (t, t),
                           name = name,
                           prototype = (Vector.new2 (ct, ct), SOME CType.bool),
                           return = Type.bool}
               end
            fun wordShift (s, sg) =
               let
                  val t = word s
                  val ct = CType.word (s, sg)
               in
                  vanilla {args = Vector.new2 (t, Type.shiftArg),
                           name = name,
                           prototype = (Vector.new2 (ct, CType.shiftArg), SOME ct),
                           return = t}
               end
         in
            case n of
               IntInf_add => intInfBinary ()
             | IntInf_andb => intInfBinary ()
             | IntInf_arshift => intInfShift ()
             | IntInf_compare =>
                  (* CHECK; cint would be better? *)
                  vanilla {args = Vector.new2 (Type.intInf (), Type.intInf ()),
                           name = name,
                           prototype = (Vector.new2 (CType.intInf, CType.intInf),
                                        SOME CType.Int32),
                           return = Type.word WordSize.word32}
             | IntInf_equal =>
                  vanilla {args = Vector.new2 (Type.intInf (), Type.intInf ()),
                           name = name,
                           prototype = (Vector.new2 (CType.intInf, CType.intInf),
                                        SOME CType.bool),
                           return = Type.bool}
             | IntInf_gcd => intInfBinary ()
             | IntInf_lshift => intInfShift ()
             | IntInf_mul => intInfBinary ()
             | IntInf_neg => intInfUnary ()
             | IntInf_notb => intInfUnary ()
             | IntInf_orb => intInfBinary ()
             | IntInf_quot => intInfBinary ()
             | IntInf_rem => intInfBinary ()
             | IntInf_sub => intInfBinary ()
             | IntInf_toString => intInfToString ()
             | IntInf_xorb => intInfBinary ()
             | MLton_bug => CFunction.bug ()
             | Real_Math_acos s => realUnary s
             | Real_Math_asin s => realUnary s
             | Real_Math_atan s => realUnary s
             | Real_Math_atan2 s => realBinary s
             | Real_Math_cos s => realUnary s
             | Real_Math_exp s => realUnary s
             | Real_Math_ln s => realUnary s
             | Real_Math_log10 s => realUnary s
             | Real_Math_sin s => realUnary s
             | Real_Math_sqrt s => realUnary s
             | Real_Math_tan s => realUnary s
             | Real_abs s => realUnary s
             | Real_add s => realBinary s
             | Real_castToWord (s1, s2) =>
                  coerce (real s1, realCType s1,
                          word s2, wordCType (s2, {signed = false}))
             | Real_div s => realBinary s
             | Real_equal s => realCompare s
             | Real_ldexp s =>
                  let
                     val t = real s
                     val ct = CType.real s
                  in
                     vanilla {args = Vector.new2 (t, Type.cint ()),
                              name = name,
                              prototype = (Vector.new2 (ct, CType.cint ()),
                                           SOME ct),
                              return = t}
                  end
             | Real_le s => realCompare s
             | Real_lt s => realCompare s
             | Real_mul s => realBinary s
             | Real_muladd s => realTernary s
             | Real_mulsub s => realTernary s
             | Real_neg s => realUnary s
             | Real_qequal s => realCompare s
             | Real_rndToReal (s1, s2) =>
                  coerce (real s1, realCType s1, real s2, realCType s2)
             | Real_rndToWord (s1, s2, sg) =>
                  coerce (real s1, realCType s1,
                          word s2, wordCType (s2, sg))
             | Real_round s => realUnary s
             | Real_sub s => realBinary s
             | Thread_returnToC => CFunction.returnToC ()
             | Word_add s => wordBinary (s, {signed = false})
             | Word_addCheck (s, sg) => wordBinaryOverflows (s, sg)
             | Word_andb s => wordBinary (s, {signed = false})
             | Word_castToReal (s1, s2) =>
                  coerce (word s1, wordCType (s1, {signed = false}),
                          real s2, realCType s2)
             | Word_equal s => wordCompare (s, {signed = false})
             | Word_extdToWord (s1, s2, sg) =>
                  coerce (word s1, wordCType (s1, sg),
                          word s2, wordCType (s2, {signed = false}))
             | Word_lshift s => wordShift (s, {signed = false})
             | Word_lt z => wordCompare z
             | Word_mul z => wordBinary z
             | Word_mulCheck (s, sg) => wordBinaryOverflows (s, sg)
             | Word_neg s => wordUnary (s, {signed = true})
             | Word_negCheck s => wordUnaryOverflows (s, {signed = true})
             | Word_notb s => wordUnary (s, {signed = false})
             | Word_orb s => wordBinary (s, {signed = false})
             | Word_quot z => wordBinary z
             | Word_rem z => wordBinary z
             | Word_rndToReal (s1, s2, sg) =>
                  coerce (word s1, wordCType (s1, sg),
                          real s2, realCType s2)
             | Word_xorb s => wordBinary (s, {signed = false})
             | Word_rol s => wordShift (s, {signed = false})
             | Word_ror s => wordShift (s, {signed = false})
             | Word_rshift z => wordShift z
             | Word_sub s => wordBinary (s, {signed = false})
             | Word_subCheck (s, sg) => wordBinaryOverflows (s, sg)
             | _ => Error.bug "SsaToRssa.Name.cFunctionRaise"
         end

      fun cFunction n = SOME (cFunctionRaise n) handle _ => NONE
   end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure PackedRepresentation = PackedRepresentation (structure Rssa = Rssa
                                                       structure Ssa = Ssa)

structure Type =
   struct
      open Type

      fun scale (ty: t): Scale.t =
         case Scale.fromBytes (bytes ty) of
            NONE => Error.bug "SsaToRssa.Type.scale"
          | SOME s => s
   end

val cardSizeLog2 : IntInf.t = 8 (* must agree with CARD_SIZE_LOG2 in gc.c *)


fun convertConst (c: Const.t): Const.t =
   let
      datatype z = datatype Const.t
   in
      case c of
         Word w => Word (WordX.resize (w, WordSize.roundUpToPrim (WordX.size w)))
       | _ => c
   end

local
  open ObjectType
  open Type
in
  fun hasTransitiveIdentity objectTypes ty i =
    let
      val visited = ref []

      fun objptrTyconHasTransitiveIdentity ty =
        (mainLoop
          (Vector.sub (objectTypes, ObjptrTycon.index ty))
          (ObjptrTycon.index ty))

      and typeHasTransitiveIdentity (typ : Type.t) =
        (case deObjptrVector typ of
              SOME v =>
              (Vector.fold (v, false, (fn (e, res) => (res orelse (objptrTyconHasTransitiveIdentity e)))))
            | NONE =>
              (case deSeq typ of
                  SOME v =>
                    (Vector.fold (v, false, (fn (e, res) => (res orelse (typeHasTransitiveIdentity e)))))
                | NONE => false))

      and mainLoop ty i =
        if List.contains (!visited, i, Int.equals) then
          false
        else
          (visited := (i::(!visited));
            case ty of
                Array {elt, hasIdentity, hasIdentityTransitive} =>
                  (if hasIdentity then
                    (hasIdentityTransitive := true;
                    true)
                  else
                    let
                      val res = typeHasTransitiveIdentity elt
                      val _ = hasIdentityTransitive := res
                    in
                      res
                    end)
              | Normal {hasIdentity, ty, hasIdentityTransitive, ...} =>
                  (if hasIdentity then
                    (hasIdentityTransitive := true;
                    true)
                  else
                    let
                      val res = typeHasTransitiveIdentity ty
                      val _ = hasIdentityTransitive := res
                    in
                      res
                    end)
              | _ => false)
    in
      mainLoop ty i
    end

  fun isUnbounded objectTypes ty i =
    let
      val visited = ref []

      fun objptrTyconIsUnbounded ty =
        (mainLoop
          (Vector.sub (objectTypes, ObjptrTycon.index ty))
          (ObjptrTycon.index ty))

      and typeIsUnbounded (typ : Type.t) =
        (case deObjptrVector typ of
              SOME v =>
              (Vector.fold (v, false, (fn (e, res) => (res orelse (objptrTyconIsUnbounded e)))))
            | NONE =>
              (case deSeq typ of
                  SOME v =>
                    (Vector.fold (v, false, (fn (e, res) => (res orelse (typeIsUnbounded e)))))
                | NONE => false))

      and mainLoop ty i =
        if List.contains (!visited, i, Int.equals) then
          true
        else
          (visited := (i::(!visited));
            case ty of
                Array {elt, hasIdentity, ...} => true
              | Normal {hasIdentity, ty, isUnbounded, ...} =>
                  let
                    val res = typeIsUnbounded ty
                    val _ = isUnbounded := res
                  in
                    res
                  end
              | _ => false)
    in
      mainLoop ty i
    end

end

fun convert (program as S.Program.T {functions, globals, main, ...},
             {codegenImplementsPrim: Rssa.Type.t Rssa.Prim.t -> bool}): Rssa.Program.t =
   let
      val isGlobalFn = S.Statement.getIsGlobalFunction (globals)
      val {diagnostic, genCase, object, objectTypes, select, toRtype, update} =
         PackedRepresentation.compute program
      val objectTypes = Vector.concat [ObjectType.basic (), objectTypes]
      val () =
         Vector.foreachi
         (objectTypes, fn (i, (opt, _)) => ObjptrTycon.setIndex (opt, i))
      val objectTypes = Vector.map (objectTypes, #2)
      val () = Vector.foreachi (objectTypes, fn (i, ty) =>
                                let
                                  val _ = hasTransitiveIdentity objectTypes ty i
                                  val _ = isUnbounded objectTypes ty i
                                in
                                  ()
                                end)

      val () = diagnostic ()
      val {get = varInfo: Var.t -> {ty: S.Type.t},
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist,
                              Property.initRaise ("varInfo", Var.layout))
      val setVarInfo =
         Trace.trace2 ("SsaToRssa.setVarInfo",
                       Var.layout, S.Type.layout o #ty, Unit.layout)
         setVarInfo
      val varType = #ty o varInfo
      fun varOp (x: Var.t): Operand.t =
         Var {var = x, ty = valOf (toRtype (varType x))}
      val varOp =
         Trace.trace ("SsaToRssa.varOp", Var.layout, Operand.layout) varOp
      fun varOps xs = Vector.map (xs, varOp)
      val extraBlocks = ref []
      fun newBlock {args, kind,
                    statements: Statement.t vector,
                    transfer: Transfer.t}: Label.t =
         let
            val l = Label.newNoname ()
            val _ = List.push (extraBlocks,
                               Block.T {args = args,
                                        kind = kind,
                                        label = l,
                                        statements = statements,
                                        transfer = transfer})
         in
            l
         end
      val {get = labelInfo: (Label.t ->
                             {args: (Var.t * S.Type.t) vector,
                              cont: (Handler.t * Label.t) list ref,
                              handler: Label.t option ref}),
           set = setLabelInfo, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("label info", Label.layout))
      fun translateCase ({test: Var.t,
                          cases: S.Cases.t,
                          default: Label.t option})
         : Statement.t list * Transfer.t =
         case cases of
            S.Cases.Con cases =>
               (case (Vector.length cases, default) of
                   (0, NONE) => ([], Transfer.bug ())
                 | _ =>
                      (case S.Type.dest (varType test) of
                          S.Type.Datatype tycon =>
                             let
                                val test = fn () => varOp test
                                val cases =
                                   Vector.map
                                   (cases, fn (con, dst) =>
                                    {con = con,
                                     dst = dst,
                                     dstHasArg =
                                     Vector.fold
                                     (#args (labelInfo dst), false, fn ((_,ty),b) =>
                                      b orelse isSome (toRtype ty))})
                                val (ss, t, blocks) =
                                   genCase {cases = cases,
                                            default = default,
                                            test = test,
                                            tycon = tycon}
                                val () =
                                   extraBlocks := blocks @ !extraBlocks
                             in
                                (ss, t)
                             end
                        | _ => Error.bug "SsaToRssa.translateCase: strange type"))
          | S.Cases.Word (s, cs) =>
               ([],
                Switch
                (Switch.T
                 {cases = (QuickSort.sortVector
                           (cs, fn ((w, _), (w', _)) =>
                            WordX.le (w, w', {signed = false}))),
                  default = default,
                  size = s,
                  test = varOp test}))
      fun eta (l: Label.t, kind: Kind.t): Label.t =
         let
            val {args, ...} = labelInfo l
            val args = Vector.keepAllMap (args, fn (x, t) =>
                                          Option.map (toRtype t, fn t =>
                                                      (Var.new x, t)))
            val l' = Label.new l
            val _ =
               List.push
               (extraBlocks,
                Block.T {args = args,
                         kind = kind,
                         label = l',
                         statements = Vector.new0 (),
                         transfer = (Transfer.Goto
                                     {dst = l,
                                      args = Vector.map (args, fn (var, ty) =>
                                                         Var {var = var,
                                                              ty = ty})})})
         in
            l'
         end
      fun labelHandler (l: Label.t): Label.t =
         let
            val {handler, ...} = labelInfo l
         in
            case !handler of
               NONE =>
                  let
                     val l' = eta (l, Kind.Handler)
                     val _ = handler := SOME l'
                  in
                     l'
                  end
             | SOME l => l
         end
      fun labelCont (l: Label.t, h: Handler.t): Label.t =
         let
            val {cont, ...} = labelInfo l
            datatype z = datatype Handler.t
         in
            case List.peek (!cont, fn (h', _) => Handler.equals (h, h')) of
               SOME (_, l) => l
             | NONE =>
                  let
                     val l' = eta (l, Kind.Cont {handler = h})
                     val _ = List.push (cont, (h, l'))
                  in
                     l'
                  end
         end
      val labelCont =
         Trace.trace2 ("SsaToRssa.labelCont",
                       Label.layout, Handler.layout, Label.layout)
         labelCont
      fun vos (xs: Var.t vector) =
         Vector.keepAllMap (xs, fn x =>
                            Option.map (toRtype (varType x), fn _ =>
                                        varOp x))
      fun bogus (t: Type.t): Operand.t =
         case Type.deReal t of
            NONE => Operand.cast (Operand.word (Type.bogusWord t), t)
          | SOME s => Operand.Const (Const.real (RealX.zero s))
      val handlesSignals =
         S.Program.hasPrim
         (program, fn p =>
          case Prim.name p of
             Prim.Name.MLton_installSignalHandler => true
           | _ => false)
      fun translateFormals v =
         Vector.keepAllMap (v, fn (x, t) =>
                            Option.map (toRtype t, fn t => (x, t)))
      fun translatePrim p =
         Prim.map (p, fn t =>
                   case toRtype t of
                      NONE => Type.unit
                    | SOME t => t)
      fun translateTransfer (t: S.Transfer.t): (Statement.t list *
                                                Transfer.t) =
         case t of
            S.Transfer.Arith {args, overflow, prim, success, ty} =>
               let
                  val prim = translatePrim prim
                  val ty = valOf (toRtype ty)
                  val res = Var.newNoname ()
                  val noOverflow =
                     newBlock
                     {args = Vector.new0 (),
                      kind = Kind.Jump,
                      statements = Vector.new0 (),
                      transfer = (Transfer.Goto
                                  {dst = success,
                                   args = (Vector.new1
                                           (Var {var = res, ty = ty}))})}
               in
                  if codegenImplementsPrim prim
                     then ([],
                           Transfer.Arith {dst = res,
                                           args = vos args,
                                           overflow = overflow,
                                           prim = prim,
                                           success = noOverflow,
                                           ty = ty})
                     else
                        let
                           datatype z = datatype Prim.Name.t
                           fun doOperCheckCF (operCheck) =
                              let
                                 val operCheckCF =
                                    case Name.cFunction operCheck of
                                       NONE =>
                                          Error.bug
                                          (concat ["SsaToRssa.translateTransfer: ",
                                                   "unimplemented arith:",
                                                   Name.toString operCheck])
                                     | SOME operCheckCF => operCheckCF
                                 val afterOperCheck =
                                    let
                                       val checkRes = Var.newNoname ()
                                    in
                                       newBlock
                                       {args = Vector.new1 (checkRes, Type.bool),
                                        kind = Kind.CReturn {func = operCheckCF},
                                        statements = Vector.new0 (),
                                        transfer = (Transfer.ifBool
                                                    (Var {var = checkRes,
                                                          ty = Type.bool},
                                                     {falsee = noOverflow,
                                                      truee = overflow}))}
                                    end
                              in
                                 Transfer.CCall
                                 {args = vos args,
                                  func = operCheckCF,
                                  return = SOME afterOperCheck}
                              end
                           fun doOperCF (oper, operCheck) =
                              let
                                 val operCF =
                                    case Name.cFunction oper of
                                       NONE =>
                                          Error.bug
                                          (concat ["SsaToRssa.translateTransfer: ",
                                                   "unimplemented arith:",
                                                   Name.toString oper])
                                     | SOME operCF => operCF
                                 val afterOper =
                                    newBlock
                                    {args = Vector.new1 (res, ty),
                                     kind = Kind.CReturn {func = operCF},
                                     statements = Vector.new0 (),
                                     transfer = doOperCheckCF operCheck}
                              in
                                 Transfer.CCall
                                 {args = vos args,
                                  func = operCF,
                                  return = SOME afterOper}
                              end
                           fun doPrim prim =
                              [Statement.PrimApp
                               {dst = SOME (res, ty),
                                prim = prim,
                                args = vos args}]
                           fun doit (prim, operCheck) =
                              if codegenImplementsPrim prim
                                 then (doPrim prim, doOperCheckCF operCheck)
                              else ([], doOperCF (Prim.name prim, operCheck))
                        in
                           case Prim.name prim of
                              Word_addCheck (s, sg) =>
                                 doit (Prim.wordAdd s,
                                       Word_addCheck (s, sg))
                            | Word_mulCheck (s, sg) =>
                                 doit (Prim.wordMul (s, sg),
                                       Word_mulCheck (s, sg))
                            | Word_negCheck s =>
                                 doit (Prim.wordNeg s,
                                       Word_negCheck s)
                            | Word_subCheck (s, sg) =>
                                 doit (Prim.wordSub s,
                                       Word_subCheck (s, sg))
                            | _ => Error.bug (concat ["SsaToRssa.translateTransfer: ",
                                                      "strange arith:",
                                                      Name.toString (Prim.name prim)])
                        end
               end
          | S.Transfer.Bug => ([], Transfer.bug ())
          | S.Transfer.Call {func, args, return} =>
               let
                  datatype z = datatype S.Return.t
                  val return =
                     case return of
                        Dead => Return.Dead
                      | NonTail {cont, handler} =>
                           let
                              datatype z = datatype S.Handler.t
                              val handler =
                                 case handler of
                                    Caller => Handler.Caller
                                  | Dead => Handler.Dead
                                  | Handle l => Handler.Handle (labelHandler l)
                           in
                              Return.NonTail {cont = labelCont (cont, handler),
                                              handler = handler}
                           end
                      | Tail => Return.Tail
               in
                  ([], Transfer.Call {func = func,
                                      args = vos args,
                                      return = return})
               end
          | S.Transfer.Case r => translateCase r
          | S.Transfer.Goto {dst, args} =>
               ([], Transfer.Goto {dst = dst, args = vos args})
          | S.Transfer.Raise xs => ([], Transfer.Raise (vos xs))
          | S.Transfer.Return xs => ([], Transfer.Return (vos xs))
          | S.Transfer.Runtime {args, prim, return} =>
               let
                  datatype z = datatype Prim.Name.t
               in
                  case Prim.name prim of
                     MLton_halt =>
                           ([],
                            Transfer.CCall
                            {args = Vector.concat [Vector.new1 GCState,
                                                   vos args],
                             func = CFunction.halt (),
                             return = NONE})
                   | Thread_copyCurrent =>
                        let
                           val func = CFunction.copyCurrentThread ()
                           val l =
                              newBlock {args = Vector.new0 (),
                                        kind = Kind.CReturn {func = func},
                                        statements = Vector.new0 (),
                                        transfer = (Goto {args = Vector.new0 (),
                                                          dst = return})}
                        in
                           ([],
                            Transfer.CCall
                            {args = Vector.concat [Vector.new1 GCState,
                                                   vos args],
                             func = func,
                             return = SOME l})
                        end
                   | _ => Error.bug (concat
                                     ["SsaToRssa.translateTransfer: ",
                                      "strange Runtime prim: ",
                                      Prim.toString prim])
               end
      fun translateStatementsTransfer (statements, ss, transfer) =
         let
            fun loop (i, ss, t): Statement.t vector * Transfer.t =
               if i < 0
                  then (Vector.fromList ss, t)
               else
                  let
                     fun none () = loop (i - 1, ss, t)
                     fun add s = loop (i - 1, s :: ss, t)
                     fun adds ss' = loop (i - 1, ss' @ ss, t)
                     val s = Vector.sub (statements, i)
                     fun split (args, kind,
                                ss: Statement.t list,
                                make: Label.t -> Statement.t list * Transfer.t) =
                        let
                           val l = newBlock {args = args,
                                             kind = kind,
                                             statements = Vector.fromList ss,
                                             transfer = t}
                           val (ss, t) = make l
                        in
                           loop (i - 1, ss, t)
                        end

                    fun addressInSharedHeap (addr) =
                    let
                      val sz = WordSize.objptr ()
                      val indexTy = Type.word sz
                      val c1 = Var.newNoname ()
                      val c2 = Var.newNoname ()
                      val c3 = Var.newNoname ()
                      val c4 = Var.newNoname ()
                      val cond = Var.newNoname ()
                      val stmts =
                        [PrimApp {args = Vector.new2 (Operand.cast (Operand.Runtime GCField.SharedHeapStart, indexTy),
                                                      Operand.cast (addr, indexTy)),
                                  dst = SOME (c1, Type.bool),
                                  prim = Prim.wordLt (sz, {signed = false})},
                        PrimApp {args = Vector.new2 (Operand.cast (addr, indexTy),
                                                      Operand.cast (Operand.Runtime GCField.SharedHeapEnd, indexTy)),
                                  dst = SOME (c2, Type.bool),
                                  prim = Prim.wordLt (sz, {signed = false})},
                        PrimApp {args = Vector.new2 (Operand.cast (addr, indexTy),
                                                      Operand.cast (Operand.Runtime GCField.SharedHeapStart, indexTy)),
                                  dst = SOME (c3, Type.bool),
                                  prim = Prim.wordEqual sz},
                        PrimApp {args = Vector.new2 (Operand.Var {var = c1, ty = Type.bool},
                                                      Operand.Var {var = c2, ty = Type.bool}),
                                  dst = SOME (c4, Type.bool),
                                  prim = Prim.wordAndb (WordSize.bool)},
                        PrimApp {args = Vector.new2 (Operand.Var {var = c3, ty = Type.bool},
                                                      Operand.Var {var = c4, ty = Type.bool}),
                                  dst = SOME (cond, Type.bool),
                                  prim = Prim.wordOrb (WordSize.bool)}]
                    in
                      (stmts, cond)
                    end

                    fun addressInLocalHeap (addr) =
                    let
                      val sz = WordSize.objptr ()
                      val indexTy = Type.word sz
                      val c1 = Var.newNoname ()
                      val c2 = Var.newNoname ()
                      val c3 = Var.newNoname ()
                      val c4 = Var.newNoname ()
                      val cond = Var.newNoname ()
                      val stmts =
                        [PrimApp {args = Vector.new2 (Operand.cast (Operand.Runtime GCField.LocalHeapStart, indexTy),
                                                      Operand.cast (addr, indexTy)),
                                  dst = SOME (c1, Type.bool),
                                  prim = Prim.wordLt (sz, {signed = false})},
                        PrimApp {args = Vector.new2 (Operand.cast (addr, indexTy),
                                                      Operand.cast (Operand.Runtime GCField.LimitPlusSlop, indexTy)),
                                  dst = SOME (c2, Type.bool),
                                  prim = Prim.wordLt (sz, {signed = false})},
                        PrimApp {args = Vector.new2 (Operand.cast (addr, indexTy),
                                                      Operand.cast (Operand.Runtime GCField.LocalHeapStart, indexTy)),
                                  dst = SOME (c3, Type.bool),
                                  prim = Prim.wordEqual sz},
                        PrimApp {args = Vector.new2 (Operand.Var {var = c1, ty = Type.bool},
                                                      Operand.Var {var = c2, ty = Type.bool}),
                                  dst = SOME (c4, Type.bool),
                                  prim = Prim.wordAndb (WordSize.bool)},
                        PrimApp {args = Vector.new2 (Operand.Var {var = c3, ty = Type.bool},
                                                      Operand.Var {var = c4, ty = Type.bool}),
                                  dst = SOME (cond, Type.bool),
                                  prim = Prim.wordOrb (WordSize.bool)}]
                    in
                      (stmts, cond)
                    end


                  in
                     case s of
                        S.Statement.Profile e => add (Statement.Profile e)
                      | S.Statement.Update {base, offset, value, needsMove} =>
                          let
                            fun updateCardRB (lhsAddr: Operand.t, rhsAddr, newRhsAddr: Var.t,
                                            continue: Label.t, returnTy, baseTy):
                                           (Statement.t list * Transfer.t) =
                              let
                                  val index = Var.newNoname ()
                                  (* CHECK; WordSize.objptr or WordSize.cpointer? *)
                                  val sz = WordSize.objptr ()
                                  val indexTy = Type.word sz
                                  val cardElemSize = WordSize.fromBits Bits.inByte
                                  val cardMarkStmts =
                                    [PrimApp {args = (Vector.new2
                                                      (Operand.cast (lhsAddr, Type.bits (WordSize.bits sz)),
                                                      Operand.word
                                                      (WordX.fromIntInf (cardSizeLog2, WordSize.shiftArg)))),
                                              dst = SOME (index, indexTy),
                                              prim = Prim.wordRshift (sz, {signed = false})},
                                    Move {dst = (ArrayOffset
                                                  {base = Runtime GCField.CardMapAbsolute,
                                                  index = Var {ty = indexTy, var = index},
                                                  offset = Bytes.zero,
                                                  scale = Scale.One,
                                                  ty = Type.word cardElemSize}),
                                          src = Operand.word (WordX.one cardElemSize)}]

                                val (stmts2, cond2) = addressInLocalHeap (rhsAddr)

                                val newRhsAddrOp =
                                  Operand.Var {var = newRhsAddr, ty = returnTy}

                                val doesNotScoreBlock =
                                  newBlock
                                  {args = Vector.new1 (newRhsAddr, returnTy),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.Goto {args = Vector.new0 (),
                                                   dst = continue}}

                                val origContinue =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                   Goto {args = Vector.new1 (rhsAddr),
                                         dst = doesNotScoreBlock}}


                                val cReturnVar = Var.newNoname ()
                                val cReturnOp = Operand.Var {var = cReturnVar, ty = returnTy}

                                val returnFromHandler =
                                  newBlock
                                  {args = Vector.new1 (cReturnVar, returnTy),
                                   kind = Kind.CReturn {func = CFunction.move returnTy},
                                   statements = Vector.new0 (),
                                   transfer =
                                   Goto {args = Vector.new1 (cReturnOp),
                                         dst = doesNotScoreBlock}}

                                val moveBlock =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.CCall
                                    {args = Vector.new4 (GCState,
                                                         rhsAddr,
                                                         Operand.bool false,
                                                         Operand.bool false),
                                    func = CFunction.move returnTy,
                                    return = SOME returnFromHandler}}
                                val maybeMoveBlock =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.ifBool
                                    (Operand.Var {var = cond2, ty = Type.bool},
                                     {truee = moveBlock,
                                      falsee = origContinue})}

                                val cardMarkBlock =
                                  newBlock
                                  {args = Vector.new0 (),
                                  kind = Kind.Jump,
                                  statements = Vector.fromList cardMarkStmts,
                                  transfer =
                                  Goto {args = Vector.new0 (),
                                        dst = origContinue}}
                                val maybeCardMarkBlock =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.ifBool
                                    (Operand.Var {var = cond2, ty = Type.bool},
                                     {truee = cardMarkBlock,
                                      falsee = origContinue})}

                                val cReturnVar2 = Var.newNoname ()
                                val cReturnOp2 = Operand.Var {var = cReturnVar2, ty = Type.bool}

                                val returnFromHandler2 =
                                  newBlock
                                  {args = Vector.new1 (cReturnVar2, Type.bool),
                                   kind = Kind.CReturn {func = CFunction.isInSharedOrForwarded baseTy},
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.ifBool
                                    (cReturnOp2,
                                      {truee = maybeMoveBlock,
                                      falsee = if (!Control.markCards) then maybeCardMarkBlock else origContinue})}

                                val isInSharedOrForwardedBlock =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.CCall
                                    {args = Vector.new2 (GCState,
                                                         lhsAddr),
                                    func = CFunction.isInSharedOrForwarded baseTy,
                                    return = SOME returnFromHandler2}}

                              in
                                (stmts2, Transfer.Goto {args = Vector.new0 (), dst = isInSharedOrForwardedBlock})
                              end

                            fun updateCardWB (lhsAddr: Operand.t, rhsAddr, newRhsAddr: Var.t,
                                            continue: Label.t, returnTy, baseTy):
                                           (Statement.t list * Transfer.t) =
                              let
                                  val index = Var.newNoname ()
                                  (* CHECK; WordSize.objptr or WordSize.cpointer? *)
                                  val sz = WordSize.objptr ()
                                  val indexTy = Type.word sz
                                  val cardElemSize = WordSize.fromBits Bits.inByte
                                  val cardMarkStmts =
                                    [PrimApp {args = (Vector.new2
                                                      (Operand.cast (lhsAddr, Type.bits (WordSize.bits sz)),
                                                      Operand.word
                                                      (WordX.fromIntInf (cardSizeLog2, WordSize.shiftArg)))),
                                              dst = SOME (index, indexTy),
                                              prim = Prim.wordRshift (sz, {signed = false})},
                                    Move {dst = (ArrayOffset
                                                  {base = Runtime GCField.CardMapAbsolute,
                                                  index = Var {ty = indexTy, var = index},
                                                  offset = Bytes.zero,
                                                  scale = Scale.One,
                                                  ty = Type.word cardElemSize}),
                                          src = Operand.word (WordX.one cardElemSize)}]

                                val (stmts2, cond2) = addressInLocalHeap (rhsAddr)
                                val (stmts3, cond3) = addressInSharedHeap (lhsAddr)

                                val newRhsAddrOp =
                                  Operand.Var {var = newRhsAddr, ty = returnTy}

                                val doesNotScoreBlock =
                                  newBlock
                                  {args = Vector.new1 (newRhsAddr, returnTy),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.Goto {args = Vector.new0 (),
                                                   dst = continue}}

                                val origContinue =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                   Goto {args = Vector.new1 (rhsAddr),
                                         dst = doesNotScoreBlock}}


                                val cReturnVar2 = Var.newNoname ()
                                val cReturnOp2 = Operand.Var {var = cReturnVar2, ty = returnTy}

                                val returnFromHandler2 =
                                  newBlock
                                  {args = Vector.new1 (cReturnVar2, returnTy),
                                   kind = Kind.CReturn {func = CFunction.moveFromWB returnTy},
                                   statements = Vector.new0 (),
                                   transfer =
                                   Goto {args = Vector.new1 (cReturnOp2),
                                         dst = doesNotScoreBlock}}

                                val moveBlock2 =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.CCall
                                    {args = Vector.new4 (GCState,
                                                         rhsAddr,
                                                         Operand.bool false,
                                                         Operand.bool false),
                                    func = CFunction.moveFromWB returnTy,
                                    return = SOME returnFromHandler2}}

                                val cReturnVar1 = Var.newNoname ()
                                val cReturnOp1 = Operand.Var {var = cReturnVar1, ty = returnTy}

                                val returnFromHandler1 =
                                  newBlock
                                  {args = Vector.new1 (cReturnVar1, returnTy),
                                   kind = Kind.CReturn {func = CFunction.moveFromWB returnTy},
                                   statements = Vector.new0 (),
                                   transfer =
                                   Goto {args = Vector.new1 (cReturnOp1),
                                         dst = doesNotScoreBlock}}

                                val moveBlock1 =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.new0 (),
                                   transfer =
                                    Transfer.CCall
                                    {args = Vector.new4 (GCState,
                                                         rhsAddr,
                                                         Operand.bool false,
                                                         Operand.bool false),
                                    func = CFunction.moveFromWB returnTy,
                                    return = SOME returnFromHandler1}}

                                val maybeMoveBlock =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.fromList (stmts2),
                                   transfer =
                                    Transfer.ifBool
                                    (Operand.Var {var = cond2, ty = Type.bool},
                                     {truee = moveBlock2,
                                      falsee = origContinue})}

                                val cardMarkBlock =
                                  newBlock
                                  {args = Vector.new0 (),
                                  kind = Kind.Jump,
                                  statements = Vector.fromList cardMarkStmts,
                                  transfer =
                                  Goto {args = Vector.new0 (),
                                        dst = origContinue}}

                                val checkLHSAddr =
                                  newBlock
                                  {args = Vector.new0 (),
                                   kind = Kind.Jump,
                                   statements = Vector.fromList (stmts3),
                                   transfer =
                                    Transfer.ifBool
                                    (Operand.Var {var = cond3, ty = Type.bool},
                                      {truee = maybeMoveBlock,
                                      falsee = if (!Control.markCards) then cardMarkBlock else origContinue})}

                              in
                                ([], Transfer.ifBool (Operand.Var {var=needsMove, ty = Type.bool}, {truee=moveBlock1, falsee=checkLHSAddr}))
                              end
                          in
                           (case toRtype (varType value) of
                               NONE => none ()
                             | SOME ty =>
                                  let
                                     val baseOp = Base.map (base, varOp)
                                     val valueOp = varOp value
                                     val newValueVar = Var.newNoname ()
                                     val newValueOp = Operand.Var {var = newValueVar, ty = ty}
                                     val baseTy = Option.valOf (toRtype (varType (Base.object base)))
                                     val isGlobal = isGlobalFn value

                                     val ss' =
                                        update
                                        {base = baseOp,
                                         baseTy = varType (Base.object base),
                                         offset = offset,
                                         value = newValueOp}
                                    val ss'' =
                                        update
                                        {base = baseOp,
                                         baseTy = varType (Base.object base),
                                         offset = offset,
                                         value = valueOp}

                                    val updateCard = if (!Control.readBarrier) then
                                                       updateCardRB
                                                     else
                                                       updateCardWB

                                  in
                                    if (Type.isObjptr ty andalso (not isGlobal)) then
                                       split (Vector.new0 (), Kind.Jump, ss' @ ss,
                                              fn l => updateCard (Base.object baseOp, valueOp, newValueVar, l, ty, baseTy))
                                    else
                                      adds ss''
                                  end)
                          end
                      | S.Statement.Bind {exp, ty, var} =>
                  let
                     fun maybeMove (f: Type.t -> Operand.t) =
                        case toRtype ty of
                           NONE => none ()
                         | SOME ty =>
                              add (Bind {dst = (valOf var, ty),
                                         isMutable = false,
                                         src = f ty})
                     fun move (src: Operand.t) = maybeMove (fn _ => src)
                     fun maybeBindStmt (stmts, oper) =
                        case toRtype ty of
                           NONE => none ()
                         | SOME ty => adds (stmts @
                                             [Bind {dst = (valOf var, ty),
                                                    isMutable = false,
                                                    src = oper}])
                     fun isObjptrInLocalHeap (addr) =
                      let
                        val (stmts, cond) = addressInLocalHeap (addr)
                      in
                        maybeBindStmt (stmts, Operand.Var {var = cond, ty = Type.bool})
                      end
                     fun isObjptrInSharedHeap (addr) =
                      let
                        val (stmts, cond) = addressInSharedHeap (addr)
                      in
                        maybeBindStmt (stmts, Operand.Var {var = cond, ty = Type.bool})
                      end
                    fun needPreemption (refAddr, valAddr) =
                      let
                        val (stmts1, cond1) = addressInSharedHeap (refAddr)
                        val (stmts2, cond2) = addressInLocalHeap (valAddr)
                        val cond = Var.newNoname ()
                        val stmt =
                          [PrimApp {args = Vector.new2 (Operand.Var {var = cond1, ty = Type.bool},
                                                        Operand.Var {var = cond2, ty = Type.bool}),
                                    dst = SOME (cond, Type.bool),
                                    prim = Prim.wordAndb (WordSize.bool)}]
                      in
                        maybeBindStmt (stmts1@stmts2@stmt, Operand.Var {var = cond, ty = Type.bool})
                      end
                  in
                     case exp of
                        S.Exp.Const c => move (Const (convertConst c))
                      | S.Exp.Inject {variant, ...} =>
                           if isSome (toRtype ty)
                              then move (varOp variant)
                           else none ()
                      | S.Exp.Object {args, con} =>
                           (case toRtype ty of
                               NONE => none ()
                             | SOME dstTy =>
                                  adds (object {args = args,
                                                con = con,
                                                dst = (valOf var, dstTy),
                                                objectTy = ty,
                                                oper = varOp}))
                      | S.Exp.PrimApp {args, prim} =>
                           let
                              val prim = translatePrim prim
                              fun arg i = Vector.sub (args, i)
                              fun a i = varOp (arg i)
                              fun cast () =
                                 move (Operand.cast (a 0, valOf (toRtype ty)))
                              fun ifIsWeakPointer (ty: S.Type.t, yes, no) =
                                 case S.Type.dest ty of
                                    S.Type.Weak ty =>
                                       (case toRtype ty of
                                           NONE => no ()
                                         | SOME t =>
                                              if Type.isObjptr t
                                                 then yes t
                                              else no ())
                                  | _ => Error.bug "SsaToRssa.ifIsWeakPointer"
                              fun arrayOrVectorLength () =
                                 move (Offset
                                       {base = a 0,
                                        offset = Runtime.arrayLengthOffset (),
                                        ty = Type.seqIndex ()})
                              fun subWord s =
                                 let
                                    val ty = Type.word s
                                 in
                                    move (ArrayOffset {base = a 0,
                                                       index = a 1,
                                                       offset = Bytes.zero,
                                                       scale = Type.scale ty,
                                                       ty = ty})
                                 end
                              fun dst () =
                                 case var of
                                    SOME x =>
                                       Option.map (toRtype (varType x), fn t =>
                                                   (x, t))
                                  | NONE => NONE
                              fun primApp prim =
                                 add (PrimApp {dst = dst (),
                                               prim = prim,
                                               args = varOps args})
                              datatype z = datatype Prim.Name.t
                              fun bumpAtomicState n =
                                 let
                                    val atomicState = Runtime GCField.AtomicState
                                    val res = Var.newNoname ()
                                    val resTy = Operand.ty atomicState
                                 in
                                    [Statement.PrimApp
                                     {args = (Vector.new2
                                              (atomicState,
                                               (Operand.word
                                                (WordX.fromIntInf
                                                 (IntInf.fromInt n,
                                                  WordSize.word32))))),
                                      dst = SOME (res, resTy),
                                      prim = Prim.wordAdd WordSize.word32},
                                     Statement.Move
                                     {dst = atomicState,
                                      src = Var {ty = resTy, var = res}}]
                                 end
                              fun setAtomicState n =
                              let
                                val atomicState = Runtime GCField.AtomicState
                              in
                                add (Statement.Move
                                      {dst = atomicState,
                                      src = n})
                              end
                              fun ccall {args: Operand.t vector,
                                         func: CFunction.t} =
                                 let
                                    val formals =
                                       case dst () of
                                          NONE => Vector.new0 ()
                                        | SOME (x, t) => Vector.new1 (x, t)
                                 in
                                    split
                                    (formals, Kind.CReturn {func = func}, ss,
                                     fn l =>
                                     ([],
                                      Transfer.CCall {args = args,
                                                      func = func,
                                                      return = SOME l}))
                                 end
                              fun simpleCCall (f: CFunction.t) =
                                 ccall {args = vos args,
                                        func = f}
                              fun simpleCCallWithGCState (f: CFunction.t) =
                                 ccall {args = Vector.concat
                                               [Vector.new1 GCState,
                                                vos args],
                                        func = f}
                              fun array (numElts: Operand.t) =
                                 let
                                    val result = valOf (toRtype ty)
                                    val opt =
                                       case Type.deObjptr result of
                                          NONE => Error.bug "SsaToRssa.array"
                                        | SOME opt => ObjptrTycon opt
                                    val args =
                                       Vector.new4 (GCState,
                                                    EnsuresBytesFree,
                                                    numElts,
                                                    opt)
                                    val func =
                                       CFunction.gcArrayAllocate
                                       {return = result}
                                 in
                                    ccall {args = args, func = func}
                                 end
                     fun cpointerGet () =
                        maybeMove (fn ty =>
                                   ArrayOffset {base = a 0,
                                                index = a 1,
                                                offset = Bytes.zero,
                                                scale = Type.scale ty,
                                                ty = ty})
                     fun cpointerSet () =
                        let
                           val src = a 2
                           val ty = Operand.ty src
                        in
                           add (Move {dst = ArrayOffset {base = a 0,
                                                         index = a 1,
                                                         offset = Bytes.zero,
                                                         scale = Type.scale ty,
                                                         ty = ty},
                                      src = a 2})
                        end
                     fun codegenOrC (p: Prim.t) =
                        let
                           val n = Prim.name p
                        in
                           if codegenImplementsPrim p
                              then primApp p
                           else (case Name.cFunction n of
                                    NONE =>
                                       Error.bug (concat ["SsaToRssa.codegenOrC: ",
                                                          "unimplemented prim:",
                                                          Name.toString n])
                                  | SOME f => simpleCCall f)
                        end
                     datatype z = datatype Prim.Name.t
                           in
                              case Prim.name prim of
                                 Array_array => array (a 0)
                               | Array_length => arrayOrVectorLength ()
                               | Array_toVector =>
                                    let
                                       val array = a 0
                                       val vecTy = valOf (toRtype ty)
                                       val opt =
                                          case Type.deObjptr vecTy of
                                             NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_toVector"
                                           | SOME opt => opt
                                    in
                                       loop
                                       (i - 1,
                                        Move
                                        {dst = (Offset
                                                {base = array,
                                                 offset = Runtime.headerOffset (),
                                                 ty = Type.objptrHeader ()}),
                                         src = ObjptrTycon opt}
                                        :: Bind {dst = (valOf var, vecTy),
                                                 isMutable = false,
                                                 src = Operand.cast (array, vecTy)}
                                        :: ss,
                                        t)
                                    end
                               | CPointer_getCPointer => cpointerGet ()
                               | CPointer_getObjptr => cpointerGet ()
                               | CPointer_getReal _ => cpointerGet ()
                               | CPointer_getWord _ => cpointerGet ()
                               | CPointer_setCPointer => cpointerSet ()
                               | CPointer_setObjptr => cpointerSet ()
                               | CPointer_setReal _ => cpointerSet ()
                               | CPointer_setWord _ => cpointerSet ()
                               | FFI f => simpleCCall f
                               (* PERF spoons this not a very efficient way of
                                getting this value (since we know its offset *)
                               | FFI_getOpArgsResPtr =>
                                    simpleCCallWithGCState
                                    (CFunction.ffiGetOpArgsResPtr ())
                               | GC_collect =>
                                    ccall
                                    {args = (Vector.new5
                                             (GCState,
                                              Operand.zero (WordSize.csize ()),
                                              Operand.bool true,
                                              File,
                                              Line)),
                                     func = (CFunction.gc
                                             {maySwitchThreads = handlesSignals})}
                               | IntInf_toVector => cast ()
                               | IntInf_toWord => cast ()
                               | MLton_bogus =>
                                    (case toRtype ty of
                                        NONE => none ()
                                      | SOME t => move (bogus t))
                               | MLton_eq =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool true)
                                      | SOME t =>
                                           codegenOrC
                                           (Prim.wordEqual
                                            (WordSize.fromBits (Type.width t))))
                               | MLton_installSignalHandler => none ()
                               | MLton_parInit =>
                                   simpleCCall (CFunction.parallelInit ())
                               | SQ_acquireLock =>
                                   simpleCCallWithGCState (CFunction.sqAcquireLock ())
                               | SQ_releaseLock =>
                                   simpleCCallWithGCState (CFunction.sqReleaseLock ())
                               | SQ_createQueues =>
                                   simpleCCallWithGCState (CFunction.sqCreateQueues ())
                               | SQ_clean =>
                                   simpleCCallWithGCState (CFunction.sqClean ())
                               | SQ_isEmpty =>
                                   simpleCCallWithGCState (CFunction.sqIsEmpty ())
                               | SQ_enque =>
                                  (case toRtype (varType (arg 0)) of
                                      NONE => Error.bug "SQ_enque saw unit"
                                    | SOME t =>
                                          if not (Type.isObjptr t)
                                            then Error.bug "SQ_enque saw non-objptr"
                                          else
                                            simpleCCallWithGCState
                                            (CFunction.sqEnque (Operand.ty (a 0))))
                               | SQ_deque =>
                                   (case toRtype ty of
                                        NONE => none ()
                                      | SOME t => simpleCCallWithGCState (CFunction.sqDeque t))
                               | Thread_testSavedClosure =>
                                   simpleCCallWithGCState (CFunction.testSavedClosure ())
                               | Thread_getSavedClosure =>
                                   (case toRtype ty of
                                        NONE => Error.bug "Thread_getSavedClosure saw unit"
                                      | SOME t => simpleCCallWithGCState (CFunction.getSavedClosure t))
                               | Thread_setSavedClosure =>
                                  (case toRtype (varType (arg 0)) of
                                      NONE => Error.bug "Thread_setSavedClosure found unit"
                                    | SOME t =>
                                          if not (Type.isObjptr t)
                                            then Error.bug "Thread_setSavedClosure saw non-objptr"
                                          else
                                            simpleCCallWithGCState
                                            (CFunction.setSavedClosure (Operand.ty (a 0))))
                               | ThreadId_testThreadId =>
                                   simpleCCallWithGCState (CFunction.testThreadId ())
                               | ThreadId_getThreadId =>
                                   (case toRtype ty of
                                        NONE => Error.bug "ThreadId_getThreadId saw unit"
                                      | SOME t => simpleCCallWithGCState (CFunction.getThreadId t))
                               | ThreadId_setThreadId =>
                                  (case toRtype (varType (arg 0)) of
                                      NONE => Error.bug "ThreadId_setThreadId found unit"
                                    | SOME t =>
                                          if not (Type.isObjptr t)
                                            then Error.bug "ThreadId_setThreadId saw non-objptr"
                                          else
                                            simpleCCallWithGCState
                                            (CFunction.setThreadId (Operand.ty (a 0))))
                               | SQ_makeObject => cast ()
                               | MLton_share =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => none ()
                                      | SOME t =>
                                           if not (Type.isObjptr t)
                                              then none ()
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.share (Operand.ty (a 0))))
                               | MLton_move2 =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => none ()
                                      | SOME t =>
                                           if not (Type.isObjptr t) then
                                             move (a 0)
                                           else
                                             ccall {args = Vector.concat
                                                            [Vector.new1 GCState,
                                                             vos args],
                                                    func = CFunction.moveFromWB (Operand.ty (a 0))})
                               | MLton_move =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => none ()
                                      | SOME t =>
                                           if not (Type.isObjptr t) then
                                             move (a 0)
                                           else
                                             ccall {args = Vector.concat
                                                            [Vector.new1 GCState,
                                                             vos args],
                                                    func = CFunction.move (Operand.ty (a 0))})
                               | Lwtgc_addToSpawnOnWBA =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => none ()
                                      | SOME t =>
                                           if not (Type.isObjptr t)
                                              then none ()
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.addToSpawnOnWBA (Operand.ty (a 0))))
                               | Lwtgc_addToMoveOnWBA =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => none ()
                                      | SOME t =>
                                           if not (Type.isObjptr t)
                                              then none ()
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.addToMoveOnWBA (Operand.ty (a 0))))
                               | Lwtgc_addToPreemptOnWBA =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => Error.bug "Lwtgc_addToPreemptOnWBA saw unit"
                                      | SOME t =>
                                           if not (Type.isObjptr t)
                                              then Error.bug "Lwtgc_addToPreemptOnWBA saw non-objptr"
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.addToPreemptOnWBA (Operand.ty (a 0))))
                               | Lwtgc_needPreemption =>
                                    (case toRtype (varType (arg 1)) of
                                        NONE => move (Operand.bool false)
                                      | SOME t =>
                                           if not (Type.isObjptr t) then
                                             move (Operand.bool false)
                                           else needPreemption (varOp (arg 0), varOp (arg 1)))
                               | Lwtgc_isObjptrInLocalHeap =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool false)
                                      | SOME t =>
                                           if not (Type.isObjptr t) then
                                             move (Operand.bool false)
                                           else isObjptrInLocalHeap (varOp (arg 0)))
                               | Lwtgc_isObjptrInSharedHeap =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool false)
                                      | SOME t =>
                                           if not (Type.isObjptr t) then
                                             move (Operand.bool false)
                                           else isObjptrInSharedHeap (varOp (arg 0)))
                               | Lwtgc_isThreadClosureClean =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool false)
                                      | SOME t =>
                                           if not (Type.isObjptr t) then
                                             move (Operand.bool false)
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.isThreadClosureClean (Operand.ty (a 0))))
                               | Lwtgc_isObjectClosureClean =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool false)
                                      | SOME t =>
                                           if not (Type.isObjptr t) then
                                             move (Operand.bool false)
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.isObjectClosureClean (Operand.ty (a 0))))
                               | Lwtgc_isObjptr =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool false)
                                      | SOME t =>
                                          let
                                          in
                                           if not (Type.isObjptr t) then
                                             move (Operand.bool false)
                                           else move (Operand.bool true)
                                          end)
                               | MLton_size =>
                                    simpleCCallWithGCState
                                    (CFunction.size (Operand.ty (a 0)))
                               | MLton_touch =>
                                   let
                                     val a = arg 0
                                   in
                                     if isSome (toRtype (varType a)) then
                                       add (PrimApp {args = Vector.new1 (varOp a),
                                                     dst = NONE,
                                                     prim = prim})

                                     else
                                       none ()
                                   end
                               | Thread_atomicBegin =>
                                    (* gcState.atomicState++;
                                     * if (gcState.signalsInfo.signalIsPending)
                                     *   gcState.limit = gcState.limitPlusSlop - LIMIT_SLOP;
                                     *)
                                    split
                                    (Vector.new0 (), Kind.Jump, ss,
                                     fn continue =>
                                     let
                                        datatype z = datatype GCField.t
                                        val tmp = Var.newNoname ()
                                        val size = WordSize.cpointer ()
                                        val ty = Type.cpointer ()
                                        val statements =
                                           Vector.new2
                                           (Statement.PrimApp
                                            {args = (Vector.new2
                                                     (Runtime LimitPlusSlop,
                                                      Operand.word
                                                      (WordX.fromIntInf
                                                       (IntInf.fromInt
                                                        (Bytes.toInt Runtime.limitSlop),
                                                        size)))),
                                             dst = SOME (tmp, ty),
                                             prim = Prim.cpointerSub},
                                            Statement.Move
                                            {dst = Runtime Limit,
                                             src = Var {ty = ty, var = tmp}})
                                        val signalIsPending =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.Jump,
                                            statements = statements,
                                            transfer = (Transfer.Goto
                                                        {args = Vector.new0 (),
                                                         dst = continue})}
                                     in
                                        (bumpAtomicState 1,
                                         if handlesSignals
                                            then
                                               Transfer.ifBool
                                               (Runtime SignalIsPending,
                                                {falsee = continue,
                                                 truee = signalIsPending})
                                         else
                                            Transfer.Goto {args = Vector.new0 (),
                                                           dst = continue})
                                     end)
                               | Thread_atomicEnd =>
                                    (* gcState.atomicState--;
                                     * if (gcState.signalsInfo.signalIsPending
                                     *     and 0 == gcState.atomicState)
                                     *   gc;
                                     *)
                                    split
                                    (Vector.new0 (), Kind.Jump, ss,
                                     fn continue =>
                                     let
                                        datatype z = datatype GCField.t
                                        val func =
                                           CFunction.gc {maySwitchThreads = true}
                                        val returnFromHandler =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.CReturn {func = func},
                                            statements = Vector.new0 (),
                                            transfer =
                                            Goto {args = Vector.new0 (),
                                                  dst = continue}}
                                        val args =
                                           Vector.new5
                                           (GCState,
                                            Operand.zero (WordSize.csize ()),
                                            Operand.bool false,
                                            File,
                                            Line)
                                        val switchToHandler =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.Jump,
                                            statements = Vector.new0 (),
                                            transfer =
                                            Transfer.CCall
                                            {args = args,
                                             func = func,
                                             return = SOME returnFromHandler}}
                                        val testAtomicState =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.Jump,
                                            statements = Vector.new0 (),
                                            transfer =
                                            Transfer.ifZero
                                            (Runtime AtomicState,
                                             {falsee = continue,
                                              truee = switchToHandler})}
                                     in
                                        (bumpAtomicState ~1,
                                         if handlesSignals
                                            then
                                               Transfer.ifBool
                                               (Runtime SignalIsPending,
                                                {falsee = continue,
                                                 truee = testAtomicState})
                                         else
                                            Transfer.Goto {args = Vector.new0 (),
                                                           dst = continue})
                                     end)
                               | Thread_getAtomicState =>
                                    move (Runtime GCField.AtomicState)
                               | Thread_setAtomicState =>
                                   setAtomicState (varOp (arg 0))
                               | Thread_copy =>
                                    simpleCCallWithGCState
                                    (CFunction.copyThread ())
                               | Thread_switchTo =>
                                    ccall {args = (Vector.new3
                                                   (GCState,
                                                    a 0,
                                                    EnsuresBytesFree)),
                                           func = CFunction.threadSwitchTo ()}
                               | Threadlet_jumpDown =>
                                    simpleCCallWithGCState
                                    (CFunction.jumpDown ())
                               | Threadlet_prefixAndSwitchTo =>
                                    simpleCCallWithGCState
                                    (CFunction.prefixAndSwitchTo ())
                               | Vector_length => arrayOrVectorLength ()
                               | Weak_canGet =>
                                    ifIsWeakPointer
                                    (varType (arg 0),
                                     fn _ =>
                                     simpleCCallWithGCState
                                     (CFunction.weakCanGet
                                      {arg = Operand.ty (a 0)}),
                                     fn () => move (Operand.bool false))
                               | Weak_get =>
                                    ifIsWeakPointer
                                    (varType (arg 0),
                                     fn t =>
                                     simpleCCallWithGCState
                                     (CFunction.weakGet
                                      {arg = Operand.ty (a 0),
                                       return = t}),
                                     none)
                               | Weak_new =>
                                    ifIsWeakPointer
                                    (ty,
                                     fn t =>
                                     let
                                        val result = valOf (toRtype ty)
                                        val header =
                                           ObjptrTycon
                                           (case Type.deObjptr result of
                                               NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Weak_new"
                                             | SOME opt => opt)
                                        val func =
                                           CFunction.weakNew {arg = t,
                                                              return = result}
                                     in
                                        ccall {args = (Vector.concat
                                                       [Vector.new2
                                                        (GCState, header),
                                                        vos args]),
                                               func = func}
                                     end,
                                     none)
                               | Word_equal s =>
                                    codegenOrC (Prim.wordEqual
                                               (WordSize.roundUpToPrim s))
                               | Word_toIntInf => cast ()
                               | Word_extdToWord (s1, s2, {signed}) =>
                                    if WordSize.equals (s1, s2)
                                       then move (a 0)
                                    else
                                       let
                                          val signed =
                                             signed
                                             andalso Bits.< (WordSize.bits s1,
                                                             WordSize.bits s2)
                                          val s1 = WordSize.roundUpToPrim s1
                                          val s2 = WordSize.roundUpToPrim s2
                                       in
                                          if WordSize.equals (s1, s2)
                                             then cast ()
                                          else
                                             codegenOrC
                                             (Prim.wordExtdToWord
                                              (s1, s2, {signed = signed}))
                                       end
                               | WordVector_toIntInf => move (a 0)
                               | Word8Array_subWord s => subWord s
                               | Word8Array_updateWord s =>
                                       let
                                          val ty = Type.word s
                                       in
                                          add (Move {dst = (ArrayOffset
                                                            {base = a 0,
                                                             index = a 1,
                                                             offset = Bytes.zero,
                                                             scale = Type.scale ty,
                                                             ty = ty}),
                                                     src = a 2})
                                       end
                               | Word8Vector_subWord s => subWord s
                               | World_save =>
                                    simpleCCallWithGCState
                                    (CFunction.worldSave ())
                               | _ => codegenOrC prim
                           end
                      | S.Exp.Select {base, offset} =>
                           (case var of
                               NONE => none ()
                             | SOME var =>
                                  (case toRtype ty of
                                      NONE => none ()
                                    | SOME ty =>
                                         adds
                                         (select
                                          {base = Base.map (base, varOp),
                                           baseTy = varType (Base.object base),
                                           dst = (var, ty),
                                           offset = offset})))
                      | S.Exp.Var y =>
                           (case toRtype ty of
                               NONE => none ()
                             | SOME _ => move (varOp y))
                  end
                  end
         in
            loop (Vector.length statements - 1, ss, transfer)
         end
      fun translateBlock (S.Block.T {label, args, statements, transfer}) =
         let
            val (ss, t) = translateTransfer transfer
            val (ss, t) = translateStatementsTransfer (statements, ss, t)
         in
            Block.T {args = translateFormals args,
                     kind = Kind.Jump,
                     label = label,
                     statements = ss,
                     transfer = t}
         end
      fun translateFunction (f: S.Function.t): Function.t =
         let
            val _ =
               S.Function.foreachVar (f, fn (x, t) => setVarInfo (x, {ty = t}))
            val {args, blocks, name, raises, returns, start, ...} =
               S.Function.dest f
            val _ =
               Vector.foreach
               (blocks, fn S.Block.T {label, args, ...} =>
                setLabelInfo (label, {args = args,
                                      cont = ref [],
                                      handler = ref NONE}))
            val blocks = Vector.map (blocks, translateBlock)
            val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
            val _ = extraBlocks := []
            fun transTypes (ts : S.Type.t vector option)
               : Type.t vector option =
               Option.map (ts, fn ts => Vector.keepAllMap (ts, toRtype))
         in
            Function.new {args = translateFormals args,
                          blocks = blocks,
                          name = name,
                          raises = transTypes raises,
                          returns = transTypes returns,
                          start = start}
         end
      val main =
          let
             val start = Label.newNoname ()
             val bug = Label.newNoname ()
          in
             translateFunction
             (S.Function.profile
              (S.Function.new
               {args = Vector.new0 (),
                blocks = (Vector.new2
                          (S.Block.T
                           {label = start,
                            args = Vector.new0 (),
                            statements = globals,
                            transfer = (S.Transfer.Call
                                        {args = Vector.new0 (),
                                         func = main,
                                         return =
                                         S.Return.NonTail
                                         {cont = bug,
                                          handler = S.Handler.Dead}})},
                           S.Block.T
                           {label = bug,
                            args = Vector.new0 (),
                            statements = Vector.new0 (),
                            transfer = S.Transfer.Bug})),
                mayInline = false, (* doesn't matter *)
                name = Func.newNoname (),
                raises = NONE,
                returns = NONE,
                start = start},
               S.SourceInfo.main))
          end
      val functions = List.revMap (functions, translateFunction)
      val p = Program.T {functions = functions,
                         handlesSignals = handlesSignals,
                         main = main,
                         objectTypes = objectTypes}
      val _ = Program.clear p
   in
      p
   end

end
