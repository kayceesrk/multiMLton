(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Runtime (S: RUNTIME_STRUCTS): RUNTIME =
struct

open S

structure GCField =
   struct
      datatype t =
         AtomicState
       | CardMapAbsolute
       | CurrentThread
       | CurSourceSeqsIndex
       | ExnStack
       | FFIOpArgsResPtr
       | Frontier
       | GlobalObjptrNonRoot
       | Limit
       | LimitPlusSlop
       | LocalHeapStart
       | MaxFrameSize
       | ProcId
       | ReturnToC
       | SharedHeapStart
       | SharedHeapEnd
       | SignalIsPending
       | StackBottom
       | StackLimit
       | StackTop
       | SessionStart

      val atomicStateOffset: Bytes.t ref = ref Bytes.zero
      val cardMapAbsoluteOffset: Bytes.t ref = ref Bytes.zero
      val currentThreadOffset: Bytes.t ref = ref Bytes.zero
      val curSourceSeqsIndexOffset: Bytes.t ref = ref Bytes.zero
      val exnStackOffset: Bytes.t ref = ref Bytes.zero
      val ffiOpArgsResPtrOffset: Bytes.t ref = ref Bytes.zero
      val frontierOffset: Bytes.t ref = ref Bytes.zero
      val globalObjptrNonRootOffset: Bytes.t ref = ref Bytes.zero
      val limitOffset: Bytes.t ref = ref Bytes.zero
      val limitPlusSlopOffset: Bytes.t ref = ref Bytes.zero
      val localHeapStartOffset: Bytes.t ref = ref Bytes.zero
      val maxFrameSizeOffset: Bytes.t ref = ref Bytes.zero
      val procIdOffset: Bytes.t ref = ref Bytes.zero
      val returnToCOffset: Bytes.t ref = ref Bytes.zero
      val sharedHeapStartOffset: Bytes.t ref = ref Bytes.zero
      val sharedHeapEndOffset: Bytes.t ref = ref Bytes.zero
      val signalIsPendingOffset: Bytes.t ref = ref Bytes.zero
      val stackBottomOffset: Bytes.t ref = ref Bytes.zero
      val stackLimitOffset: Bytes.t ref = ref Bytes.zero
      val stackTopOffset: Bytes.t ref = ref Bytes.zero
      val sessionStartOffset: Bytes.t ref = ref Bytes.zero

      fun setOffsets {atomicState, cardMapAbsolute, currentThread, curSourceSeqsIndex,
                      exnStack, ffiOpArgsResPtr, frontier, globalObjptrNonRoot, limit,
                      limitPlusSlop, localHeapStart, maxFrameSize, procId, returnToC,
                      sharedHeapStart, sharedHeapEnd, signalIsPending, stackBottom,
                      stackLimit, stackTop, sessionStart} =
         (atomicStateOffset := atomicState
          ; cardMapAbsoluteOffset := cardMapAbsolute
          ; currentThreadOffset := currentThread
          ; curSourceSeqsIndexOffset := curSourceSeqsIndex
          ; exnStackOffset := exnStack
          ; ffiOpArgsResPtrOffset := ffiOpArgsResPtr
          ; frontierOffset := frontier
          ; globalObjptrNonRootOffset := globalObjptrNonRoot
          ; limitOffset := limit
          ; limitPlusSlopOffset := limitPlusSlop
          ; localHeapStartOffset := localHeapStart
          ; maxFrameSizeOffset := maxFrameSize
          ; procIdOffset := procId
          ; returnToCOffset := returnToC
          ; sharedHeapStartOffset := sharedHeapStart
          ; sharedHeapEndOffset := sharedHeapEnd
          ; signalIsPendingOffset := signalIsPending
          ; stackBottomOffset := stackBottom
          ; stackLimitOffset := stackLimit
          ; stackTopOffset := stackTop
          ; sessionStartOffset := sessionStart)

      val offset =
         fn AtomicState => !atomicStateOffset
          | CardMapAbsolute => !cardMapAbsoluteOffset
          | CurrentThread => !currentThreadOffset
          | CurSourceSeqsIndex => !curSourceSeqsIndexOffset
          | ExnStack => !exnStackOffset
          | FFIOpArgsResPtr => !ffiOpArgsResPtrOffset
          | Frontier => !frontierOffset
          | GlobalObjptrNonRoot => !globalObjptrNonRootOffset
          | Limit => !limitOffset
          | LimitPlusSlop => !limitPlusSlopOffset
          | LocalHeapStart => !localHeapStartOffset
          | MaxFrameSize => !maxFrameSizeOffset
          | ProcId => !procIdOffset
          | ReturnToC => !returnToCOffset
          | SharedHeapStart => !sharedHeapStartOffset
          | SharedHeapEnd => !sharedHeapEndOffset
          | SignalIsPending => !signalIsPendingOffset
          | StackBottom => !stackBottomOffset
          | StackLimit => !stackLimitOffset
          | StackTop => !stackTopOffset
          | SessionStart => !sessionStartOffset

      val atomicStateSize: Bytes.t ref = ref Bytes.zero
      val cardMapAbsoluteSize: Bytes.t ref = ref Bytes.zero
      val currentThreadSize: Bytes.t ref = ref Bytes.zero
      val curSourceSeqsIndexSize: Bytes.t ref = ref Bytes.zero
      val exnStackSize: Bytes.t ref = ref Bytes.zero
      val ffiOpArgsResPtrSize: Bytes.t ref = ref Bytes.zero
      val frontierSize: Bytes.t ref = ref Bytes.zero
      val globalObjptrNonRootSize: Bytes.t ref = ref Bytes.zero
      val limitSize: Bytes.t ref = ref Bytes.zero
      val limitPlusSlopSize: Bytes.t ref = ref Bytes.zero
      val localHeapStartSize: Bytes.t ref = ref Bytes.zero
      val maxFrameSizeSize: Bytes.t ref = ref Bytes.zero
      val procIdSize: Bytes.t ref = ref Bytes.zero
      val returnToCSize: Bytes.t ref = ref Bytes.zero
      val sharedHeapStartSize: Bytes.t ref = ref Bytes.zero
      val sharedHeapEndSize: Bytes.t ref = ref Bytes.zero
      val signalIsPendingSize: Bytes.t ref = ref Bytes.zero
      val stackBottomSize: Bytes.t ref = ref Bytes.zero
      val stackLimitSize: Bytes.t ref = ref Bytes.zero
      val stackTopSize: Bytes.t ref = ref Bytes.zero
      val sessionStartSize: Bytes.t ref = ref Bytes.zero

      fun setSizes {atomicState, cardMapAbsolute, currentThread, curSourceSeqsIndex,
                    exnStack, ffiOpArgsResPtr, frontier, globalObjptrNonRoot, limit,
                    limitPlusSlop, localHeapStart, maxFrameSize,  procId, returnToC,
                    sharedHeapStart, sharedHeapEnd, signalIsPending, stackBottom,
                    stackLimit, stackTop, sessionStart} =
         (atomicStateSize := atomicState
          ; cardMapAbsoluteSize := cardMapAbsolute
          ; currentThreadSize := currentThread
          ; curSourceSeqsIndexSize := curSourceSeqsIndex
          ; exnStackSize := exnStack
          ; ffiOpArgsResPtrSize := ffiOpArgsResPtr
          ; frontierSize := frontier
          ; globalObjptrNonRootSize := globalObjptrNonRoot
          ; limitSize := limit
          ; limitPlusSlopSize := limitPlusSlop
          ; localHeapStartSize := localHeapStart
          ; maxFrameSizeSize := maxFrameSize
          ; procIdSize := procId
          ; returnToCSize := returnToC
          ; sharedHeapStartSize := sharedHeapStart
          ; sharedHeapEndSize := sharedHeapEnd
          ; signalIsPendingSize := signalIsPending
          ; stackBottomSize := stackBottom
          ; stackLimitSize := stackLimit
          ; stackTopSize := stackTop
          ; sessionStartSize := sessionStart)

      val size =
         fn AtomicState => !atomicStateSize
          | CardMapAbsolute => !cardMapAbsoluteSize
          | CurrentThread => !currentThreadSize
          | CurSourceSeqsIndex => !curSourceSeqsIndexSize
          | ExnStack => !exnStackSize
          | FFIOpArgsResPtr => !ffiOpArgsResPtrSize
          | Frontier => !frontierSize
          | GlobalObjptrNonRoot => !globalObjptrNonRootSize
          | Limit => !limitSize
          | LimitPlusSlop => !limitPlusSlopSize
          | LocalHeapStart => !localHeapStartSize
          | MaxFrameSize => !maxFrameSizeSize
          | ProcId => !procIdSize
          | ReturnToC => !returnToCSize
          | SharedHeapStart => !sharedHeapStartSize
          | SharedHeapEnd => !sharedHeapEndSize
          | SignalIsPending => !signalIsPendingSize
          | StackBottom => !stackBottomSize
          | StackLimit => !stackLimitSize
          | StackTop => !stackTopSize
          | SessionStart => !sessionStartSize

      val toString =
         fn AtomicState => "AtomicState"
          | CardMapAbsolute => "CardMapAbsolute"
          | CurrentThread => "CurrentThread"
          | CurSourceSeqsIndex => "CurSourceSeqsIndex"
          | ExnStack => "ExnStack"
          | FFIOpArgsResPtr => "FFIOpArgsResPtr"
          | Frontier => "Frontier"
          | GlobalObjptrNonRoot => "GlobalObjptrNonRoot"
          | Limit => "Limit"
          | LimitPlusSlop => "LimitPlusSlop"
          | LocalHeapStart => "LocalHeapStart"
          | MaxFrameSize => "MaxFrameSize"
          | ProcId => "ProcIdSize"
          | ReturnToC => "ReturnToC"
          | SharedHeapStart => "SharedHeapStart"
          | SharedHeapEnd => "SharedHeapEnd"
          | SignalIsPending => "SignalIsPending"
          | StackBottom => "StackBottom"
          | StackLimit => "StackLimit"
          | StackTop => "StackTop"
          | SessionStart => "SessionStart"

      val layout = Layout.str o toString
   end

structure RObjectType =
   struct
      datatype t =
         Array of {hasIdentity: bool,
                   hasIdentityTransitive: bool,
                   bytesNonObjptrs: Bytes.t,
                   numObjptrs: int}
       | Normal of {hasIdentity: bool,
                    hasIdentityTransitive: bool,
                    isUnbounded: bool,
                    bytesNonObjptrs: Bytes.t,
                    numObjptrs: int}
       | Stack
       | Weak of {gone: bool}
       | HeaderOnly
       | Fill

      fun layout (t: t): Layout.t =
         let
            open Layout
         in
            case t of
               Array {hasIdentity, bytesNonObjptrs, numObjptrs,
                      hasIdentityTransitive} =>
                  seq [str "Array ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("hasIdentityTransitive", Bool.layout hasIdentityTransitive),
                               ("bytesNonObjptrs", Bytes.layout bytesNonObjptrs),
                               ("numObjptrs", Int.layout numObjptrs)]]
             | Normal {hasIdentity, bytesNonObjptrs, numObjptrs,
                       hasIdentityTransitive, isUnbounded} =>
                  seq [str "Normal ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("hasIdentityTransitive", Bool.layout hasIdentityTransitive),
                               ("isUnbounded", Bool.layout isUnbounded),
                               ("bytesNonObjptrs", Bytes.layout bytesNonObjptrs),
                               ("numObjptrs", Int.layout numObjptrs)]]
             | Stack => str "Stack"
             | Weak {gone} =>
                  seq [str "Weak",
                       record [("gone", Bool.layout gone)]]
             | HeaderOnly => str "HeaderOnly"
             | Fill => str "Fill"
         end
      val _ = layout (* quell unused warning *)
   end

(* see gc/object.h *)
local
   val maxTypeIndex = Int.pow (2, 18)
in
   (* see gc/object.c:buildHeaderFromTypeIndex *)
   fun typeIndexToHeader typeIndex =
      (Assert.assert ("Runtime.header", fn () =>
                      0 <= typeIndex
                      andalso typeIndex < maxTypeIndex)
       ; Word.orb (0w1, Word.<< (Word.fromInt typeIndex, 0w1)))

   fun headerToTypeIndex w = Word.toInt (Word.>> (w, 0w1))
end

(* see gc/object.h *)
val objptrSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.objptr)

(* see gc/object.h *)
val headerSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.header)
val headerOffset : unit -> Bytes.t =
   Promise.lazy (Bytes.~ o headerSize)

val lwtgcMask = Word.toIntInf 0wxFFF1FFFF
val virginMaskLower = Word.toIntInf 0wx00020000
val pointerMask = Word.toIntInf 0wx00000003


val virginMask = Word.toIntInf 0wx00060000
val virginMaskInvert = Word.toIntInf 0wxfff9ffff
val virginShift = Word.toIntInf 0wx11

(* see gc/array.h *)
val arrayLengthSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.seqIndex)
val arrayLengthOffset : unit -> Bytes.t =
   Promise.lazy (fn () => Bytes.~ (Bytes.+ (headerSize (),
                                            arrayLengthSize ())))

val cpointerSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.cpointer)
val labelSize = cpointerSize

(* See platform.c. *)
val allocTooLarge = Bytes.fromWord 0wxFFFFFFFC

(* See gc/heap.h. *)
val limitSlop = Bytes.fromInt 512

(* See gc/frame.h. *)
val maxFrameSize = Bytes.fromInt (Int.pow (2, 16))

end
