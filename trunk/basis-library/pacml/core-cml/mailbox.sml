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
      
      datatype 'a mbox = MB of 'a chan

      fun mailbox () = MB (channel())

      fun sameMailbox (MB (c1), MB (c2) = sameChannle(c1, c2)

      fun send (MB (c), x) = aSync(aSendEvt(c1, x))
     
      fun recv (MB (c)) = recv(c)
       
      fun recvPoll (MB (c)) = recvPoll(c)
    
  end
