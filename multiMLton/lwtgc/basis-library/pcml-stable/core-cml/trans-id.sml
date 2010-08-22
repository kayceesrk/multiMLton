(* trans-id.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* ???
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure TransID : TRANS_ID =
   struct
      structure Assert = LocalAssert(val assert = false)

      structure R = RepTypes
      structure L = Lock
      structure T = ThreadID


      (* Transaction IDs are used to mark blocked threads in the various waiting
       * queues.  They are "cancelled" when some other event is selected.
       *)
      datatype trans_id = datatype R.trans_id
      datatype trans_id_state = datatype R.trans_id_state

     fun tidNum () = T.tidToInt (Basic.getCurThreadId ())
      (* create a new transaction ID. *)
      (* KC TODO : Implement CAS on numbers. This implementation might be
      * inefficient *)
      fun mkTxId () =
      let
        val lock = L.initCmlLock ()
        val v = ref WAITING
        fun cas (v,t1,t2) =
          let
            val _ = MLtonThread.atomicBegin ()
            val _ = L.getCmlLock lock (tidNum ())
            val res = !v
            val _ = if (!v) = t1 then
                       (v := t2)
                    else ()
            val _ = L.releaseCmlLock lock (tidNum ())
            val _ = MLtonThread.atomicEnd ()
          in
            res
          end
      in
        TXID({txst = v, cas = cas})
      end


      (* create a transaction flag (ID and cleanUp). *)
      fun mkFlg () =
         let
            val txid as TXID {txst, cas} = mkTxId ()
         in
            txid
         end

      (* XXX KC : unsafe *)
      (** Used by stabilizers to clear partial communication on channels **)
      fun cancel (TXID {txst, cas}) = txst := SYNCHED
      (*
      fun toString (TXID {txst,cas}) =
         case !txst of
            WAITING => "WAITING"
          | SYNCHED => "SYNCHED"
      *)
   end
