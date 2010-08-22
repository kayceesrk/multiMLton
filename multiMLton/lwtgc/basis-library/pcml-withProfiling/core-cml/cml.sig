(* cml.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* cml-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The interface to the core CML features.
 *)

signature MLTON_PCML =
  sig
     include VERSION
     include EVENT_TYPE
     include THREAD
     include EVENT
     include CHANNEL
     include TIME_OUT
     structure MutexLock : MUTEX_LOCK
     structure SyncVar : SYNC_VAR
     structure Mailbox : MAILBOX
     structure SimpleRPC : SIMPLE_RPC
     structure Multicast : MULTICAST
     structure Threadlet : THREADLET
     structure Aux : AUX
  end
