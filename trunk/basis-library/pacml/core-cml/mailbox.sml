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

      structure E = Event
      structure C = Channel

      datatype 'a mbox = MB of ('a C.chan)

      fun mailbox () = MB (C.channel())

      fun sameMailbox (MB (c1), MB (c2)) = C.sameChannel(c1, c2)

      fun send (MB (c), x) = E.aSync(C.aSendEvt(c, x))
     
      fun recv (MB (c)) = C.recv(c)

      fun recvEvt (MB (c)) = C.recvEvt(c)
       
      fun recvPoll (MB (c)) = C.recvPoll(c)
    
  end
