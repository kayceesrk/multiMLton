(* mailbox.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* mailbox.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Asynchronous channels (called mailboxes).
 *)

structure Mailbox : MAILBOX_EXTRA =
   struct

      structure Assert = LocalAssert(val assert = false)
      structure Debug = LocalDebug(val debug = false)

      open Critical
      structure TID = ThreadID
      structure PT= ProtoThread

      fun debug msg = Debug.sayDebug ([atomicMsg, TID.tidMsg], msg)
      fun debug' msg = debug (fn () => msg()^"."^(PT.getThreadTypeString()) ^" : "^Int.toString(PacmlFFI.processorNumber()))

      structure E = Event
      structure C = Channel

      datatype 'a mbox = MB of ('a C.chan)

      fun mailbox () = MB (C.channel())

      fun sameMailbox (MB (c1), MB (c2)) = C.sameChannel(c1, c2)

      fun send (MB (c), x) =
      let
        val () = Assert.assertNonAtomic' "Mailbox.send(1)"
        val () = debug' (fn () => "Mailbox.send(1)")
        val () = C.aSend (c, x)
        val () = Assert.assertNonAtomic' "Mailbox.send(2)"
        val () = debug' (fn () => "Mailbox.send(2)")
      in
        ()
      end

      fun recv (MB (c)) =
      let
        val () = Assert.assertNonAtomic' "Mailbox.recv(1)"
        val () = debug' (fn () => "Mailbox.recv(1)")
        val v = C.recv (c)
        val () = Assert.assertNonAtomic' "Mailbox.recv(2)"
        val () = debug' (fn () => "Mailbox.recv(2)")
      in
        v
      end

      fun recvEvt (MB (c)) =
        let
          val () = debug' (fn () => "Mailbox.recvEvt(1)")
          val e = C.recvEvt(c)
          val () = debug' (fn () => "Mailbox.recvEvt(2)")
        in
          e
        end

      fun recvPoll (MB (c)) =
        let
          val () = debug' (fn () => "Mailbox.recvPoll(1)")
          val v = C.recvPoll (c)
          val () = debug' (fn () => "Mailbox.recvPoll(2)")
        in
          v
        end

  end
