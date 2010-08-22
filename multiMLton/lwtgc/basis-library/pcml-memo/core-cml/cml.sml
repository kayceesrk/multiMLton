(* cml.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* cml.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure MLtonPCML : MLTON_PCML =
   struct
      open Version
      open Thread
      open EventType
      open Event
      open Channel
      open TimeOut
      structure MutexLock : MUTEX_LOCK = MutexLock
      structure SyncVar : SYNC_VAR = SyncVar
      structure Mailbox : MAILBOX = Mailbox
      structure SimpleRPC : SIMPLE_RPC = SimpleRPC
      structure Multicast : MULTICAST = Multicast
      structure Threadlet : THREADLET = Threadlet
      structure Stable : STABLE = Stable
      structure Aux : AUX = Aux
   end
