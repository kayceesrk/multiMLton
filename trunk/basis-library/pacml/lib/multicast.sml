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

      structure SV = SyncVar

      type 'a event = 'a Event.sevt

      datatype 'a request =
         Message of 'a
       | NewPort
      datatype 'a mc_state = MCState of ('a * 'a mc_state SV.ivar)
      datatype 'a port =
         Port of (('a * 'a mc_state SV.ivar) Channel.chan * 'a mc_state SV.ivar SV.mvar)
      datatype 'a mchan =
         MChan of ('a request Channel.chan * 'a port Channel.chan)

    fun mkPort cv =
       let
          val outCh = Channel.channel()
          val stateVar = SV.mVarInit cv
          fun tee cv =
             let
                val (MCState(v, nextCV)) = SV.iGet cv
             in
                Channel.send (outCh, (v, nextCV))
                ; tee nextCV
             end
          val _ = Thread.spawn (fn () => tee cv)
       in
          Port(outCh, stateVar)
       end

    fun mChannel () =
       let
          val reqCh = Channel.channel()
          and replyCh = Channel.channel()
          fun server cv =
             case (Channel.recv reqCh) of
                NewPort =>
                   (Channel.send (replyCh, mkPort cv)
                    ; server cv)
              | (Message m) =>
                   let
                      val nextCV = SV.iVar()
                   in
                      SV.iPut (cv, MCState(m, nextCV))
                      ; server nextCV
                   end
          val _ = Thread.spawn (fn () => server (SV.iVar()))
       in
          MChan(reqCh, replyCh)
       end

    fun multicast (MChan(ch, _), m) = Channel.send (ch, Message m)

    fun port (MChan(reqCh, replyCh)) =
       (Channel.send (reqCh, NewPort)
        ; Channel.recv replyCh)

    fun copy (Port(_, stateV)) = mkPort(SV.mGet stateV)

    fun recvMsg stateV (v, nextCV) =
       let val _ = SV.mSwap (stateV, nextCV)
       in v
       end

    fun recv (Port(ch, stateV)) = recvMsg stateV (Channel.recv ch)
    fun recvEvt (Port(ch, stateV)) = Event.wrap(Channel.recvEvt ch, recvMsg stateV)
   end

