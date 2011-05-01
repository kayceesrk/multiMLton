(* critical.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure Critical : CRITICAL =
   struct
      structure Thread = MLtonThread
      structure AtomicState = MLtonThread.AtomicState
      structure Signal = MLtonSignal
      structure Itimer = MLtonItimer

      local datatype z = datatype Thread.AtomicState.t
      in
         fun atomicMsg () =
            case Thread.getAtomicState () of
               AtomicState.NonAtomic => "[NonAtomic]"
             | AtomicState.Atomic n => concat ["[ Atomic ", Int.toString n, "]"]
         fun getAtomicState () =
           case Thread.getAtomicState () of
                AtomicState.NonAtomic => 0
              | AtomicState.Atomic n => n
         val setAtomicState = Thread.setAtomicState
      end

      fun atomicBegin () = Thread.atomicBegin ()
      fun atomicEnd () = Thread.atomicEnd ()
      fun doAtomic (f: unit -> 'a) =
        let
          val () = atomicBegin ()
          val res = f ()
          val () = atomicEnd ()
        in
          res
        end

      val mask = Signal.Mask.some [Itimer.signal Itimer.Real]
      fun maskBegin () = Signal.Mask.block mask
      fun maskEnd () = Signal.Mask.unblock mask
      fun doMasked (f: unit -> unit) = (maskBegin (); f (); maskEnd ())
   end
