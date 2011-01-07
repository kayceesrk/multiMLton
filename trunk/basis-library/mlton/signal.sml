(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonSignal: MLTON_SIGNAL_EXTRA =
struct

open Posix.Signal
structure Prim = PrimitiveFFI.Posix.Signal
structure PrimWorld = Primitive.MLton.World
structure Error = PosixError
structure SysCall = Error.SysCall
val restart = SysCall.restartFlag

type t = signal

type how = C_Int.t

fun raiseInval () =
   let
      open PosixError
   in
      raiseSys inval
   end

val validSignals =
   Array.tabulate
   (C_Int.toInt Prim.NSIG, fn i =>
    SysCall.syscallErr
    ({clear = false, restart = false, errVal = C_Int.fromInt ~1}, fn () =>
     {return = Prim.sigismember (repFromInt i),
      post = fn _ => true,
      handlers = [(Error.inval, fn () => false)]}))

structure Mask =
   struct
      datatype t =
         AllBut of signal list
       | Some of signal list

      val allBut = AllBut
      val some = Some

      val all = allBut []
      val none = some []

      fun read () =
         Some
         (Array.foldri
          (fn (i, b, sigs) =>
           if b
              then let
                      val s = fromInt i
                      val s' = repFromInt i
                      val res =
                         SysCall.simpleResult
                         (fn () => Prim.sigismember s')
                   in
                      if res = C_Int.fromInt 1
                         then s::sigs
                         else sigs
                   end
              else sigs)
          []
          validSignals)

      fun write m =
         case m of
            AllBut signals =>
               (SysCall.simple Prim.sigfillset
                ; List.app (fn s => SysCall.simple
                                    (fn () => Prim.sigdelset (toRep s)))
                           signals)
          | Some signals =>
               (SysCall.simple Prim.sigemptyset
                ; List.app (fn s => SysCall.simple
                                    (fn () => Prim.sigaddset (toRep s)))
                           signals)

      local
         fun make (how: how) (m: t) =
            (write m; SysCall.simpleRestart (fn () => Prim.pthread_sigmask how))
      in
         val block = make Prim.SIG_BLOCK
         val unblock = make Prim.SIG_UNBLOCK
         val setBlocked = make Prim.SIG_SETMASK
         fun getBlocked () = (make Prim.SIG_BLOCK none; read ())
      end

      local
         fun member (sigs, s) = List.exists (fn s' => s = s') sigs
      in
         fun isMember (mask, s) =
            if Array.sub (validSignals, toInt s)
               then case mask of
                       AllBut sigs => not (member (sigs, s))
                     | Some sigs => member (sigs, s)
               else raiseInval ()
      end
   end

structure Handler =
   struct
      datatype t =
         Default
       | Handler of MLtonThread.Runnable.t -> MLtonThread.Runnable.t
       | Ignore
       | InvalidSignal
   end

datatype handler = datatype Handler.t

local
   val numProcessors = PacmlFFI.numberOfProcessors
   val procNum = PacmlFFI.processorNumber

   (* XXX KC : per-thread state *)
   val r = Array.tabulate(numProcessors, fn _ => ref C_Int.zero)
in
   fun initHandler (s: signal): Handler.t =
      SysCall.syscallErr
      ({clear = false, restart = false, errVal = C_Int.fromInt ~1}, fn () =>
       {return = Prim.isDefault (toRep s, Array.unsafeSub(r,procNum())),
        post = fn _ => if !(Array.unsafeSub(r,procNum())) <> C_Int.zero then Default else Ignore,
        handlers = [(Error.inval, fn () => InvalidSignal)]})

    val gcHandlers = Array.tabulate(numProcessors, fn _ => Ignore)
end

val (getHandler, setHandler, handlers) =
   let
      val localhandlers = fn () => Array.tabulate (C_Int.toInt Prim.NSIG, initHandler o fromInt)
      (* A localHandlers array for each processor *)
      val handlersArr = Array.tabulate
      (PacmlFFI.numberOfProcessors, fn _ => localhandlers ())
     fun init h =
         Cleaner.addNew
         (Cleaner.atLoadWorld, fn () =>
          Array.modifyi (initHandler o fromInt o #1) h)

     val numProc = PacmlFFI.numberOfProcessors
     (* KC Used for CML *)
     fun setHandlerForAll h s =
     let
       val _ = Array.tabulate(numProc, fn p =>
                     Array.update(Array.unsafeSub(handlersArr,p), toInt
                     s, h))
     in
       ()
     end

     (* Initialize localHandlers *)
     val _ = Array.app init handlersArr

     val procNum = PacmlFFI.processorNumber
   in
      (fn s: t => Array.sub (Array.unsafeSub(handlersArr, procNum()), toInt s),
       fn (s: t, h) => if Primitive.MLton.Profile.isOn andalso s = prof
                          then raiseInval ()
                       else
                         if (PrimWorld.getIsPCML ()) andalso (s = alrm) andalso
                         (case h of
                            Handler _ => true
                          | _ => false)
                         then
                           setHandlerForAll h s
                         else
                          Array.update (Array.unsafeSub (handlersArr,procNum ()), toInt s, h),
       fn () => Array.unsafeSub(handlersArr, procNum()))
   end

fun handled () =
   Mask.some
   (Array.foldri
    (fn (s, h, sigs) =>
     case h of
        Handler _ => (fromInt s)::sigs
      | _ => sigs) [] (handlers()) )

structure Handler =
   struct
      open Handler

      val default = Default
      val ignore = Ignore

      val isDefault = fn Default => true | _ => false
      val isIgnore = fn Ignore => true | _ => false

      val handler =
         (* This let is used so that Thread.setHandler is only used if
          * Handler.handler is used.  This prevents threads from being part
          * of every program.
          *)
         let
            (* As far as C is concerned, there is only one signal handler.
             * As soon as possible after a C signal is received, this signal
             * handler walks over the array of all SML handlers, and invokes any
             * one for which a C signal has been received.
             *
             * Any exceptions raised by a signal handler will be caught by
             * the topLevelHandler, which is installed in thread.sml.
             *)
            val _ =
               PosixError.SysCall.blocker :=
               (fn () => let
                            val m = Mask.getBlocked ()
                            val () = Mask.block (handled ())
                         in
                            fn () => Mask.setBlocked m
                         end)

            val () =
               MLtonThread.setSignalHandler
               (fn t =>
                let
                   val mask = Mask.getBlocked ()
                   val () = Mask.block (handled ())
                   val proc = PacmlFFI.processorNumber()
                   val fs =
                      case Array.unsafeSub(gcHandlers,proc) of
                         Handler f => if Prim.isPendingGC () <> C_Int.zero
                                         then [f]
                                         else []
                       | _ => []
                   val fs =
                      Array.foldri
                      (fn (s, h, fs) =>
                       case h of
                          Handler f =>
                             if Prim.isPending (repFromInt s) <> C_Int.zero
                                then f::fs
                                else fs
                        | _ => fs) fs (handlers())
                   val () = Prim.resetPending ()
                   val () = Mask.setBlocked mask
                in
                   List.foldl (fn (f, t) => f t) t fs
                end)
         in
            Handler
         end

      fun simple (f: unit -> unit) = handler (fn t => (f (); t))
   end

val setHandler = fn (s, h) =>
   case (getHandler s, h) of
      (InvalidSignal, _) => raiseInval ()
    | (_, InvalidSignal) => raiseInval ()
    | (Default, Default) => ()
    (* KC don't call into c for CML & SIGALRM *)
    | (_, Default) =>
         (setHandler (s, Default)
         (* Prevent this thread from handling C alrm signal if CML since
          * we have installed a separate signal handler therad.
          * XXX KC : pthread_sigmask is set in c-main.h. So Is this redundant??
          *)
         ; if (PrimWorld.getIsPCML ()) andalso s = alrm then
                ()
           else
                SysCall.simpleRestart (fn () => Prim.default (toRep s)))
    (* XXX KC modified because the request may be for another processor*)
    | (Handler _, Handler _) =>
         (setHandler (s, h)
          ; if (PrimWorld.getIsPCML ()) andalso s = alrm then
                ()
           else
                SysCall.simpleRestart (fn () => Prim.handlee (toRep s)))
    | (_, Handler _) =>
         (setHandler (s, h)
          ; if (PrimWorld.getIsPCML ()) andalso s = alrm then
                ()
           else
               SysCall.simpleRestart (fn () => Prim.handlee (toRep s)))
    | (Ignore, Ignore) => ()
    | (_, Ignore) =>
         (setHandler (s, Ignore)
          ;if (PrimWorld.getIsPCML ()) andalso s = alrm then
                ()
           else
                SysCall.simpleRestart (fn () => Prim.ignore (toRep s)))

fun suspend m =
   (Mask.write m
    ; Prim.sigsuspend ()
    ; MLtonThread.switchToSignalHandler ())

fun handleGC f =
   (Prim.handleGC ()
    ; Array.update (gcHandlers,PacmlFFI.processorNumber (),
    Handler.simple f))

end
