(* rep-types.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* rep-types.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * These are the concrete representations of the various CML types.
 * These types are abstract (or not even visible) outside this library.
 *)

structure RepTypes =
   struct
      structure L = Lock

      datatype queue_prio = PRI | SEC
      datatype thread_type = PARASITE | HOST

      (** condition variables --- see cvar.sml and events.sml *)
      datatype cvar = CVAR of (cvar_state ref * L.cmlLock)
      and cvar_state =
         CVAR_unset of {transId : int ref, (* XXX KC - Me wants macros *)
                        cleanUp : unit -> unit,
                        thread : rdy_thread,
                        procNum : int} list
       | CVAR_set of int

      (** thread IDs --- see thread-id.sml and threads.sml **)
      and thread_id =
         TID of {
                 (* an unique ID *)
                 id : int,
                 (* true, if there is a pending alert on this thread *)
                 alert : bool ref,
                 (* set this whenever this thread does some concurrency operation. *)
                 done_comm : bool ref,
                 (* root-level exception handler hook *)
                 exnHandler : (exn -> unit) ref,
                 (* holds thread-local properties *)
                 props : exn list ref,
                 (* the cvar that becomes set when the thread dies *)
                 dead : cvar,
                 (* Whether we are host thread or parasite *)
                 threadType : thread_type ref,
                 (* Pointer to the threadlet sitting below us *)
                 (* It is an offset from the bottom of the stack *)
                 parasiteBottom : int ref,
                 (* Whether to preempt a parasite *)
                 preemptParasite : bool ref
                 }

      (** threads --- see scheduler.sml and threads.sml **)
      and 'a thread = THRD of thread_id * 'a MLtonThread.t
      and rdy_thread = RTHRD of thread_id * MLtonThread.Runnable.t

      (** events --- see events.sml **)
      datatype sync_status = DOIT_FAIL | DOIT_SUCCESS
      datatype 'a status =
         ENABLED of {prio : int, doitFn : ((sync_status thread option) -> 'a option)}
       | BLOCKED of {transId : int ref,
                     cleanUp : unit -> unit,
                     next : unit -> rdy_thread,
                     parentThread : (unit -> rdy_thread) option} -> 'a
      type 'a base = unit -> 'a status
      datatype 'a sevt =
         BEVT of 'a base list
       | CHOOSE of 'a sevt list
       | GUARD of unit -> 'a sevt

       datatype ('a,'b) aevt =
            PAIR of ('a sevt * 'b sevt)

       datatype ('a,'b) cevt =
            SEVT of 'a sevt
          | AEVT of ('a,'b) aevt

       datatype placement = CUR_PROC | ANY_PROC
   end
