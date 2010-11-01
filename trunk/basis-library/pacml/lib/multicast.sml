(* multicast.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* multicast.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Asynchronous multicast (one-to-many) channels.  This implementation
 * is based on a condition variable implementation of multicast channels.
 * See Chapter 5 of "Concurrent Programming in ML" for details.
 *)

structure Multicast : MULTICAST =
   struct

      type 'a event = 'a Event.sevt

      datatype 'a request =
         Message of 'a
       | NewPort of 'a Channel.chan
      
      datatype 'a port =
         Port of 'a Channel.chan 
     
      datatype 'a mchan =
         MChan of ('a request Channel.chan * 'a Channel.chan list ref)

    fun mChannel () =
       let
          val reqCh = Channel.channel()
          and channels = ref []
          fun server () =
             case (Channel.recv reqCh) of
                NewPort chan => (channels := (chan::(!channels)); server()) 
                | (Message m) =>
                   let val chanList = !channels
                       fun sender(channels) =
                         case channels
                           of x::xs => (Channel.aSend(x, m); sender(xs))
                            | [] => ()
                       val _ = sender(chanList)
                   in server ()
                   end
          val _ = Thread.spawn (fn () => server ())
       in
          MChan(reqCh, channels)
       end

    fun multicast (MChan(ch, _), m) = Channel.send (ch, Message m)

    fun port (MChan(reqCh, channels)) =
      let val ch = Channel.channel()
          val _ =  Channel.send(reqCh, NewPort(ch))
      in Port(ch)
      end
    fun recv (Port(ch)) = Channel.recv ch
    fun recvEvt (Port(ch)) = Channel.recvEvt ch
   end

