(* imp-queue.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure LockFreeQueue : LOCK_FREE_QUEUE =
   struct
      type 'a = RepTypes.'a
      datatype t = T of {front: 'a list ref ref, back: 'a list ref ref}

    val cas = _import "Parallel_compareAndSwap": 'a list ref ref * 'a list ref * 'a list ref -> bool;

    fun normalize (T {front, back}) =
      let
        fun grabBack () =
        let
          val oldTail = !back
          val res = !oldTail
          val newTail = ref []
        in
          if cas (back, oldTail, newTail) then
            res
          else
            grabBack ()
        end

        val oldTail = grabBack ()
        val revOldTail = List.rev oldTail

        fun appFront () =
        let
          val oldHead = !front
          val newHead = ref (!oldHead @ revOldTail)
        in
          if cas (front, oldHead, newHead) then
            ()
          else
            appFront ()
        end
      in
        ()
      end

      fun deque (q as T {front, back}) =
      let
        val _ = Assert.assertAtomic' ("LockFreeQueue.deque", NONE)
        fun bar () =
          let
            val oldTail = !back
          in
            case !oldTail of
                [] => NONE
              | _ => (normalize (q); foo ())
          end

        and foo () =
          let
            val oldHead = !front
          in
            case !oldHead of
                [] => bar ()
              | x::rest => let
                              val newHead = ref rest
                            in
                              if cas (front, oldHead, newHead) then
                                  SOME (x)
                              else
                                foo ()
                            end
        end
      in
        foo ()
      end


      fun unsafeEmpty (T {front, back}) =
         (Assert.assertAtomic' ("LockFreeQueue.empty", NONE)
          ; case !(!front) of
               [] => (case !(!back) of
                         [] => true
                       | _ => false)
             | _ => false)

      fun enque (T {back, ...}, x) =
      let
        val _ = Assert.assertAtomic' ("LockFreeQueue.enque", NONE)
        fun foo () =
        let
          val oldTail = !back
          val newTail = ref (x::(!oldTail))
        in
          if cas (back, oldTail, newTail) then
            ()
          else
            foo ()
        end
      in
        foo ()
      end


      fun undeque (T {front, ...}, x) =
      let
        val _ = Assert.assertAtomic' ("LockFreeQueue.enque", NONE)
        fun foo () =
        let
          val oldHead = !front
          val newHead = ref (x::(!oldHead))
        in
          if cas (front, oldHead, newHead) then
            ()
          else
            foo ()
        end
      in
        foo ()
      end

      fun new () = T {front = ref (ref []), back = ref (ref [])}

      fun unsafeReset (T {front, back}) =
         (Assert.assertAtomic' ("LockFreeQueue.reset", NONE)
          ; front := ref []
          ; back := ref [])

   end
