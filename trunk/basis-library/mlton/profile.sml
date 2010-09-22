(* Copyright (C) 2003-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonProfile: MLTON_PROFILE =
struct

structure P = Primitive.MLton.Profile

val procCount = PacmlFFI.numberOfProcessors
val procNum = PacmlFFI.processorNumber

val gcState = Primitive.MLton.GCState.gcState

val isOn = P.isOn

structure Data =
   struct
      datatype t = T of {isCurrent: bool ref,
                         isFreed: bool ref,
                         raw: P.Data.t}

      val all: t list array = Array.tabulate (procCount, fn _ => [])

      local
         fun make f (T r) = f r
      in
         val isFreed = make #isFreed
         val raw = make #raw
      end

      fun equals (d, d') =
         isFreed d = isFreed d'

      fun free (d as T {isCurrent, isFreed, raw, ...}) =
         if not isOn
            then ()
         else
            if !isFreed
               then raise Fail "free of freed profile data"
            else if !isCurrent
                    then raise Fail "free of current profile data"
                 else
                   let
                     val myAll = Array.sub (all, procNum())
                     val myNewAll = (List.foldl (fn (d', ac) =>
                                        if equals (d, d')
                                           then ac
                                        else d' :: ac) [] (myAll))
                   in
                     (Array.update (all, procNum(), myNewAll)
                      ; P.Data.free (gcState, raw)
                      ; isFreed := true)
                   end

      fun make (raw: P.Data.t): t =
         T {isCurrent = ref false,
            isFreed = ref false,
            raw = raw}

      fun malloc (): t =
         let
            val array =
               if isOn
                  then P.Data.malloc gcState
               else P.Data.dummy
            val d = make array
            val _ = Array.update (all, procNum(), d::Array.sub (all, procNum()))
         in
            d
         end

      fun write (T {isFreed, raw, isCurrent}, file) =
         if not isOn then
            ()
         else if !isFreed then
            raise Fail "write of freed profile data"
         else
            P.Data.write (gcState, raw,
                          Primitive.NullString8.fromString
                          (String.nullTerm file))
   end

val currentArray: Data.t array = Array.tabulate (procCount, fn _ => (Data.make P.Data.dummy))

fun current () = Array.sub (currentArray, procNum())

fun setCurrent (d as Data.T {isCurrent, isFreed, raw, ...}) =
   if not isOn
      then ()
   else
      if !isFreed
         then raise Fail "setCurrent of freed profile data"
      else
         let
            val Data.T {isCurrent = ic, ...} = current ()
            val _ = ic := false
            val _ = isCurrent := true
            val _ = Array.update (currentArray, procNum(), d)
            val _ = P.setCurrent (gcState, raw)
         in
            ()
         end

fun withData (d: Data.t, f: unit -> 'a): 'a =
   let
      val old = current ()
      val _ = setCurrent d
   in
      DynamicWind.wind (f, fn () => setCurrent old)
   end

fun init () = setCurrent (Data.make (P.getCurrent gcState))

val _ =
   if not isOn
      then ()
   else
      let
         val _ =
            Cleaner.addNew
            (Cleaner.atExit, fn () => P.done gcState)
         val _ =
            Cleaner.addNew
            (Cleaner.atLoadWorld, fn () =>
             ((* In a new world, all of the old profiling data is invalid. *)
              Array.update (Data.all, procNum(), [])
              ; init ()))
      in
         init ()
      end

end
