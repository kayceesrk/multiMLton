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



      type proc_num = int

      exception DOIT_FAIL

      datatype queue_prio = PRI | SEC | ANY
      datatype thread_type = PARASITE | HOST

      type parasite = Primitive.MLton.Parasite.parasite
      type primHost = Primitive.MLton.Thread.thread

      (** condition variables --- see cvar.sml and events.sml *)
      datatype cvar = CVAR of (cvar_state ref)
      and cvar_state =
         CVAR_unset of {transId : int ref, (* KC - Me wants macros *)
                        cleanUp : unit -> unit,
                        thread : rdy_thread,
                        procNum : int} list
       | CVAR_set of int

      and parasite_state =
        PSTATE of {
                 (* Whether we are host thread or parasite *)
                 threadType : thread_type,
                 (* Pointer to the threadlet sitting below us *)
                 (* It is an offset from the bottom of the stack *)
                 parasiteBottom : (int * int), (* First int is offset, second is tidNum *)
                 (* This parameter is used to penalize a thread for
                 *  compute intensive parasites*)
                 numPenaltySpawns : int,
                 (* This integer is used to obtain locks. This id is unique across
                  * hosts and parasites*)
                 lockId : int
                 }

      (** thread IDs --- see thread-id.sml and threads.sml **)
      and thread_id =
         TID of {(* an unique ID *)
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
                 (* parasitic state *)
                 pstate : parasite_state ref,
                 (* Whether to preempt a parasite *)
                 preemptParasite : bool ref,
                 (* Processor Id to which the thread belongs to *)
                 processorId : int}

      (* Need to be prepared with a value to run -- -1 *)
      and 'a thread = H_THRD of (thread_id * 'a MLtonThread.t)
                    | P_THRD of (int (*lockId*) * parasite * ((unit -> 'a) -> unit))

      and runnable_host = RHOST of (thread_id * MLtonThread.Runnable.t)

     and cmlLock = LOCK of {state: int ref,
                            lockId: int ref,
                            count: int ref,
                            que: rdy_thread CirQueue.t}

      (* Ready to run -- 0 *)
      and rdy_thread = H_RTHRD of runnable_host
                    |  P_RTHRD of (int (* lockId *) * parasite)

      (** events --- see events.sml **)
      datatype 'a status =
         ENABLED of {prio : int, doitFn : unit -> 'a}
       | BLOCKED of ((int ref) -> 'a)

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

       datatype prefix_kind = PREFIX_REGULAR | PREFIX_SPECIAL
   end
