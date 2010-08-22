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
      (** transaction IDs -- see trans-id.sml *)
      datatype thread_state = ASYNC | MAIN
      datatype trans_id = TXID of (
          {txst : trans_id_state ref,
          cas : (trans_id_state ref * trans_id_state * trans_id_state) -> trans_id_state})
      and trans_id_state =
         WAITING
       | CLAIMED
       | SYNCHED

     type timeoutFunc = (unit -> unit) ref -> trans_id -> unit -> unit
     type constraintFunc = timeoutFunc -> unit
     type effectFunc = unit -> unit

      (** condition variables --- see cvar.sml and events.sml *)
      datatype cvar = CVAR of (cvar_state ref * L.cmlLock)
      and cvar_state =
         CVAR_unset of {transId : trans_id,
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
                 (* Whether we are Main thread or Async *)
                 state : thread_state ref,
                 (* Pointer to the threadlet sitting below us *)
                 (* It is an offset from the bottom of the stack *)
                 next : int ref
                 (** memoization **),
                 taking_memo : bool ref,
                 memo_cache  : (unit -> unit) ref list ref
                 }

      (** threads --- see scheduler.sml and threads.sml **)
      and 'a thread = THRD of thread_id * 'a MLtonThread.t
      and rdy_thread = RTHRD of thread_id * MLtonThread.Runnable.t

      (** events --- see events.sml **)
      datatype 'a status =
         ENABLED of {prio : int, doitFn : (unit -> 'a option)}
       | BLOCKED of {transId : trans_id,
                     cleanUp : unit -> unit,
                     next : unit -> rdy_thread} -> 'a
      type 'a base = unit -> 'a status
      datatype 'a event =
         BEVT of 'a base list
       | CHOOSE of 'a event list
       | GUARD of unit -> 'a event
       | WNACK of unit event -> 'a event
     datatype 'a sref =
        SREF of {history : (int * 'a) list ref,
                id      : int}
   end
