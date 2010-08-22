(* Contains the DEBUG flag and debug printing functions *)
structure DebugHelper =
  struct
    val DEBUG = false

    exception Error of string
    fun error msg =
      let val _ = print "CMLGraph.ERROR: '"
          val _ = print msg
          val _ = print "' raising.\n"
      in raise Error msg
      end

    fun valOf s =
      case s
        of SOME v => v
         | NONE   => error "Option.valOf expected SOME, got NONE"

    fun debug(fx) =
      if DEBUG
      then print(fx())
      else ()

  end

(**
 * Contains functions for manipulating nodes:
 * newId/newRefId     : creates fresh node/ref id's
 * nodeEnv/setNodeEnv : get/sets the continuation, id, stableStack, and tid for the node
 * node2...           : quick getters
 * getTxnId           : each node can wait on at most 1 txnId (used in channel communication)
 * dfs                : startNode, node->acc, acc
 **)

 structure ListN =
 struct
      fun foreach (l,f) = List.app f l
      fun isEmpty l =
        case l of
            [] => true
          | _ :: _ => false


        fun fold' (l, b, f) =
         let
            fun loop (l, b) =
               case l of
                  [] => b
                | x :: l => loop (l, f (x, b))
         in loop (l, b)
         end

        fun push' (r, x) = r := x :: !r


 end
structure NodeHelper =
  struct

    structure Graph = GCAbleGraph
    structure Node = Graph.Node
    structure Edge = Graph.Edge
    structure Tid = ThreadID


     fun tidNum () = Tid.tidToInt (Basic.getCurThreadId ())
    (* Counter for assigning unique node Id's *)
    val nodeCount = ref 0
    val nodeCountLock = Lock.initCmlLock ()
    val refCount  = ref 0
    val refCountLock = Lock.initCmlLock ()

    local
      open DebugHelper
    in

      (* Creates a fresh ID for nodes *)
      fun newId() =
        let
            val _ = Lock.getCmlLock nodeCountLock (tidNum ())
            val id = !nodeCount
            val () = nodeCount := id + 1
            val _ = Lock.releaseCmlLock nodeCountLock (tidNum ())
        in id
        end

     (********************************************************************************
       *  each node has a continuation and the thread id of the thread it belongs to  *
       *  a numerical identifier as well as a stack of nodes representing the nesting *
       *  levels of stable sections. The first node of each thread (spawn), does not  *
       *  contain a continuation, also channel nodes do not belong to a thread.  In   *
       *  each of these cases the appropriate fields will be set to NONE              *
       ********************************************************************************)
      val {get = nodeEnv: unit Node.t -> {continuation:  (unit -> unit) ref option,
                                          nodeId: int,
                                          stableStack: unit Node.t list,
                                          tid: Tid.thread_id option},
           set = setNodeEnv, ...} =
                  Property.getSet
                   (Node.plist, Property.initRaise ("Cmlgraph.nodeEnv", Node.layout))
      val node2Cont = #continuation o nodeEnv  (* NONE when node is a spawn node *)
      val node2Id = #nodeId o nodeEnv
      val node2Tid = #tid o nodeEnv   (* NONE when node just got zapped by stabilize *)
      val node2Stack = #stableStack o nodeEnv

      (**
       * Stores the optional refStack.
       * When the property is requested, we store the
       * last refId in the property and a list of id*(unit->unit)
       * ref undo functions.
       **)
      fun newRefId() =
        let
          val _ = Lock.getCmlLock refCountLock (tidNum ())
          val id = !refCount
          val () = refCount := id + 1
          val _ = Lock.releaseCmlLock refCountLock (tidNum ())
        in id
        end
      (* Used by TidHelper *)
      val {get = node2RefStack: unit Node.t -> (int * (unit -> unit)) list ref, ... } =
                  Property.getSet
                   (Node.plist, Property.initFun (fn _ => ref []))

      (* Used only by enterSS and exitSS *)
      fun setNodeStack(node, newStack) =
        setNodeEnv (node, {continuation = node2Cont node,
                           nodeId       = node2Id node,
                           stableStack  = newStack,
                           tid          = node2Tid node})

      (**
       * A node can wait on at most 1 txnid.
       * getTxnId : node -> trans option
       *)
      val {get = getTxnId : unit Node.t -> (TransID.trans_id option),
           set = setTxnId, ...} =
                   Property.getSet
                   (Node.plist, Property.initFun (fn _ => NONE (*default*)))

      fun nodeToString(n) =
        let val idStr = (Int.toString (node2Id n)) handle _ => "no_id?"
            val tidStr = (case node2Tid n
                            of NONE => "NO_TID"
                             | SOME tid => Tid.tidToString tid) handle _ => "no_tid?"
            val contStr = (case node2Cont n
                             of NONE => "."
                              | SOME _ => "c") handle _ => "?"
            val txnStr = (case getTxnId n
                            of NONE => "."
                             | SOME _ => "p") handle _ => "?"
        in "[" ^ idStr ^ ":" ^ tidStr ^ ":" ^ contStr ^ txnStr ^ "]"
        end
      fun addEdge(r as {from, to}) =
        let val _ = debug(fn()=>"Graph:addEdge: " ^ nodeToString from ^ "->" ^ nodeToString to ^ "\n")
        in Graph.addEdge(r, Basic.tidNum ())
        end

      (**
       * Simple DFS. Uses Property to mark visited nodes
       **)
      fun dfs(start, doStuff, initAcc) =
        let val {get = nodeInfo: unit Node.t -> bool ref,
                 destroy, ...} =
                Property.destGetSet (Node.plist,
                  Property.initFun (fn _ => ref false))
            fun dfs'(n, acc) =
              let val hasBeenVisited = (nodeInfo n)
              in if  !hasBeenVisited
                 then acc
                 else let val () = hasBeenVisited := true
                          val () = debug(fn() => "API - DFS - visiting a node " ^ nodeToString n ^ "\n")
                          val newAcc = doStuff(n, acc)
                          val adjs = Node.successors n
                          val newAcc2 = ListN.fold'(map Edge.to(adjs), newAcc, dfs')
                      in newAcc2
                      end
              end
            val ret = dfs'(start, initAcc)
            val _ = destroy ()
        in ret
        end (* end let *)

    end (* end local *)
  end (* end struct *)

(**
 * Contains functions for manipulating data tied to a tid:
 *
 * A compStack contains pairs of node and fn to run.
 *
 * tid2... : quick getters
 * tidPush : changes current node for the tid
 **)
structure TidHelper =
  struct

    local
      open DebugHelper
      open RepTypes (* for the TID datatype *)

      (* Used to add a new compensation/ref-undo to a node.
       * Each node(stable section) can have many compensations/ref-undo's.
       * "stack" is either a compensation stack or a ref stack
       * "newEntry" is the new compensation/ref-undo that will be added
       *)
      fun add(stack, n, newEntry) =
        ListN.push' (stack, (n, SOME newEntry))

    in

      fun tid2Current  (TID{currentNode, ...}) =
        case !currentNode
          of NONE => (* Initial thread was not spawned and does not have a currentNode yet *)
                     let val n = GCAbleGraph.Node.new()
                         val () = currentNode := SOME n
                     in n
                     end
           | SOME n => n

      fun tid2CompStack(TID{compStack, ...})   = !compStack

      fun tidSetNode(TID{currentNode, ...}, newCurNode) =
        currentNode := SOME newCurNode
      (* Replaces the current compStack. Used only in stabilize *)
      fun tidSetCompStack(TID{compStack, ...}, newCompStack) =
        compStack := newCompStack
      (* changes current node for the tid *)
      fun tidPush(tid as TID{compStack, ...}, newCurNode) =
        let val newComp = (newCurNode, NONE)
            val () = tidSetNode(tid, newCurNode)
            val () = ListN.push'(compStack, newComp)
        in ()
        end
      fun tidAddComp(tid as TID{compStack, ...}, newComp) =
        add(compStack, tid2Current tid, newComp)
      fun tidAddRef(tid, newRef) =
        let val refStack = NodeHelper.node2RefStack (tid2Current tid)
        in ListN.push'(refStack, (NodeHelper.newRefId(), newRef))
        end
    end (* end local *)
  end (* end struct *)


(* The main structure *)
structure StableGraph: STABLE_GRAPH  =
  struct

    structure Thread = MLtonThread

    local
      open DebugHelper
      open TidHelper
      open NodeHelper

      (* Used in stabilize for temporary storage *)
      val DEFAULT_HASH_TABLE_SIZE = 5000
      (* NECESSARY to mark the first thread because it has no parent *)
      (**TODO: since we don't have channel nodes, can't we just use id=0? **)
      val firstSpawn = ref true



    in

      (******************************************************
       *  This function creates a new thread node for tid   *
       *  A mapping is added to currentThreads for the tid  *
       *  The initial thread (id = 0) has no parent thread  *
       *  Called by scheduler.new                           *
       ******************************************************)
      fun spawnThread(tidParent, tid) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: spawning thread: ")
            val _ = debug(fn() => Tid.tidToString(tid))
            val _ = debug(fn() => " by thread: ")
            val _ = debug(fn() => Tid.tidToString(tidParent))
            val _ = debug(fn() => "\n")
            val isFirst = !firstSpawn (* (0 = !count) *)
            val threadStartNode = Graph.Node.new()
            val _ = setNodeEnv(threadStartNode, {continuation = NONE,
                                                 nodeId = newId(),
                                                 stableStack = [],
                                                 tid = SOME tid})
            val _ = tidPush(tid, threadStartNode)
            val _ = if isFirst
                    then firstSpawn := false (* We found the root case *)
                    else ignore(addEdge({from = tid2Current(tidParent),
                                               to   = threadStartNode}))
        in Thread.atomicEnd()
        end
      (* Same as previous except makes a bidirectional edge
       * Used internally by CML to ensure selective
       * communication is rolled back correctly.
       *)
      fun spawnThread2(tidParent, tid) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: spawning thread2: ")
            val _ = debug(fn() => Tid.tidToString(tid))
            val _ = debug(fn() => " by thread: ")
            val _ = debug(fn() => Tid.tidToString(tidParent))
            val _ = debug(fn() => "\n")
            val isFirst = !firstSpawn (* (0 = !count) *)
            val threadStartNode = Graph.Node.new()
            val _ = setNodeEnv(threadStartNode, {continuation = NONE,
                                                 nodeId = newId(),
                                                 stableStack = [],
                                                 tid = SOME tid})
            val _ = tidPush(tid, threadStartNode)
            val _ = if isFirst
                    then firstSpawn := false (* We found the root case *)
                    else (ignore(addEdge({from = tid2Current(tidParent),
                                               to   = threadStartNode}))
                               ;ignore (addEdge({to = tid2Current(tidParent),
                                               from   = threadStartNode})))
        in Thread.atomicEnd()
        end

      (*****************************************************
       * Called BEFORE enterSS.                            *
       *                                                   *
       * Update the currentThreads with a new node.        *
       * Add a link to this node from the old currNode     *
       * Copy info from the previous node without changing *
       * because we are not entering/exiting a stable sec. *
       *****************************************************)
      fun schedThread(savedCont, tid) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: scheduling thread: ")
            val _ = debug(fn() => Tid.tidToString(tid))
            val _ = debug(fn() => "\n")

            val currentNode = tid2Current(tid)
            (* Either: we create a new current node and make an edge
             *         from current to newNode (COMMON CASE)
             *     OR: stabilize was called and the currentNode is "hollow"
             *         and we just need to refill it
             *)
            fun refill(n) = setNodeEnv(n, {continuation = SOME savedCont,
                                           nodeId = newId(),
                                           stableStack = node2Stack currentNode,
                                           tid  = SOME tid})
            (*TODO check if we can ever get the "hollow" case?? *)
            val node = case node2Tid currentNode
                         of SOME _ => (* normal case *)
                             let val n = Graph.Node.new()
                                 val _ = refill(n)
                                 val _ = addEdge(
                                           {from = currentNode,
                                            to   = n})
                             in n
                             end
                          | NONE   => (* "hollow" node *) (refill(currentNode); currentNode)

            val () = tidPush(tid, node)
        in Thread.atomicEnd()
        end
      (* Same as previous except makes a bidirectional edge
       * Used internally by CML to ensure selective
       * communication is rolled back correctly.
       *)
      fun schedThread2(savedCont, tid) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: scheduling thread2: ")
            val _ = debug(fn() => Tid.tidToString(tid))
            val _ = debug(fn() => "\n")

            val currentNode = tid2Current(tid)
            (* Either: we create a new current node and make an edge
             *         from current to newNode (COMMON CASE)
             *     OR: stabilize was called and the currentNode is "hollow"
             *         and we just need to refill it
             *)
            fun refill(n) = setNodeEnv(n, {continuation = SOME savedCont,
                                           nodeId = newId(),
                                           stableStack = node2Stack currentNode,
                                           tid  = SOME tid})
            (*TODO check if we can ever get the "hollow" case?? *)
            val node = case node2Tid currentNode
                         of SOME _ => (* normal case *)
                             let val n = Graph.Node.new()
                                 val _ = refill(n)
                                 val _ = addEdge(
                                           {from = currentNode,
                                            to   = n})
                                 val _ = addEdge(
                                           {from = n,
                                            to   = currentNode})
                             in n
                             end
                          | NONE   => (* "hollow" node *) (refill(currentNode); currentNode)

            val () = tidPush(tid, node)
        in Thread.atomicEnd()
        end

      (*
       * Occurs when a send/receive is not yet paired.
       * Store the txnId so we can clear the message
       * from the channel if stabilize is called before
       * a completeCom occurs
       *)
      fun partialCom(tid, txid) =
        let val _ = Thread.atomicBegin()
            val node = tid2Current(tid)
            val _ = debug(fn() => "API: partial com " ^ nodeToString node ^ "\n")
            val _ = setTxnId(node, SOME txid)
        in Thread.atomicEnd()
        end

      (****************************************************
       * Remove the partial communication from tid        *
       * to channel.                                      *
       * Add a bidirectional edge to mark the             *
       * interdependence between the sender and receiver. *
       * If one unrolls, so must the other.               *
       * "suspendTid" : the tid that engaged in partial   *
       *                communication                     *
       * "matchedTid" : the other end of the comm         *
       ****************************************************)
      fun completeCom(suspendTid, matchedTid) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: complete com:")
            val _ = debug(fn() => Tid.tidToString suspendTid)
            val _ = debug(fn() => " - ")
            val _ = debug(fn() => Tid.tidToString matchedTid)
            val _ = debug(fn() => "\n")


            val suspendNode = tid2Current(suspendTid)
            val matchedNode = tid2Current(matchedTid)
            val _ = addEdge({from = suspendNode,
                             to   = matchedNode})
            val _ = addEdge({from = matchedNode,
                             to   = suspendNode})
            (* remove that the node was in partial com *)
            (* if stabilize is called, both tids must unroll now *)
            val _ = setTxnId (suspendNode, NONE)
        in Thread.atomicEnd()
        end

      (*
       * Uses node put on by schedThread (see CML.sml)
       * - Push node onto stable stack
       *   (no node creation unless entering nested stable section)
       * - Set this node as current
       *)
      fun enterSS(tid, newComp: unit -> unit, nodeRef) =
        (* set up compensation here *)
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: entering stable scope for thread:")
            val _ = debug(fn() => Tid.tidToString(tid))
            val _ = debug(fn() => "\n")

            val currentNode = tid2Current(tid)
            (* Used by enterSS to combine multiple compensations and addCompensation *)
            (* Used to add reference undo to an existing stable section *)
            (** The size of newCompStack is the number of nodes for a thread.
             * !!NOT!! the current nesting depth of stable sections because
             * We could have entered and exited and need to still keep the compens around.
             **)
            val () = tidAddComp(tid, newComp)

            (* Used to stabilize arbitrary stable sections *)
            (*TODO: why don't we just return this without an option? *)
            val _ = nodeRef := SOME currentNode

            (* Push the currentNode onto the stable stack  ExitSS will pop it off *)
            val _ = setNodeStack(currentNode, (currentNode :: (node2Stack currentNode)))

        in Thread.atomicEnd()
        end


      fun addReference(tid, refUndo) =
        let val _ = Thread.atomicBegin()
            val () = debug(fn() => "API: add Reference\n")
            val _ = tidAddRef(tid, refUndo)
            val _ = Thread.atomicEnd()
        in ()
        end

      fun addSyncVarWrite(writeTid, varUndo) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: sync var write\n")
            (* Add undo code to the graph. Just like a reference. *)
            val _ = tidAddRef(writeTid, varUndo)
            val _ = Thread.atomicEnd()
        in ()
        end

      fun addSyncVarCom(readTid, writeTid) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: complete sync var com\n")
            (* Add dependency edge from the writer to reader. *)
            (* If the writer is restored, readers must also. *)
            val readNode = tid2Current(readTid)
            val writeNode = tid2Current(writeTid)
            val _ = addEdge({from = writeNode,
                             to   = readNode})
            val _ = Thread.atomicEnd()
        in ()
        end


      (*
       * - pop top node off to this thread's stack
       * - If nested stable section, add a back edge to top of stack
       *)
      fun exitSS(tid) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: exit stable scope\n")
            val curNode = tid2Current(tid)
            val newStack = case (node2Stack curNode)
                             of (_::xs) => xs
                              | []      => error "Mismatched ExitSS (stack was empty)"
            val _ = setNodeStack(curNode, newStack)
            val _ = case newStack
                      of parentNode::_ => ignore
                                  (debug(fn() => "API: exitSS adding back edge\n");
                                   addEdge({from = curNode,
                                            to   = parentNode}))
                       | [] => (debug(fn() => "API: exitSS NOT adding back edge\n"))

        in Thread.atomicEnd()
        end

      (* Used by channel.sml to decide when to grab continuations *)
      fun inStableSection tid =
        let val n = tid2Current(tid)
            val stack = node2Stack n
        in not (ListN.isEmpty stack)
        end

      (* Does 3 things:
       * (1) Partition the graph (find what must be disposed and what must be restored)
       * (2) CML clean up (create new threads with continuations, clear channels)
       * (3) Graph cleanup (remove dangling edges)
       *)
      fun stabilizeGraph(curTid, stableFrom) =
        let val _ = Thread.atomicBegin()
            val _ = debug(fn() => "API: stabilizing with regard to thread:")
            val _ = debug(fn() => Tid.tidToString(curTid))
            val _ = debug(fn() => "\n")

            val counterBefore = !nodeCount (*USED ONLY IN DEBUGGING *)
            val startTime = Time.now()
            val _ = debug(fn() => "STABILIZE START: " ^ Time.toString startTime ^ "\n")

            (* (1) Find the subgraph of "tainted" nodes (those stabilize will affect):
             * - The set of nodes that will be removed because of stabilize.
             *   These are everything reachable from the stabilize point
             * - Clear all relevant partial communications.
             * - Create a killSet of threads that must be removed
             *   to undo the stabilizePoint's changes.
             * - Create a restoreTbl that contains restore points for
             *   threads modified, but not created to undo stabilizePoints changes.
             *
             * The subgraph will be partitioned into the restoreTbl and the killSet.
             *)
            val startNode =
                  case stableFrom
                    of SOME node => node
                     | NONE => tid2Current(curTid)

            (* partitionTbl: (tid * node opt)
             * - Value = NONE: nodes that must be killed
             * - Value = SOME: nodes that aren't killed off and have continuations
             * Once dfs is done we create 2 lists, tids2Kill and threads2Restore.
             *)
            val partitionTbl = HashTable.new (DEFAULT_HASH_TABLE_SIZE, Tid.hashTid, Tid.sameTid)
            fun someAndSmaller(SOME new, SOME old) = node2Id new < node2Id old
              | someAndSmaller(SOME _  , NONE    ) = false (* If tid is already in killset, then don't restore*)
              | someAndSmaller(NONE    , _       ) = true  (* If tid will be    in killset, then don't restore*)

            val refUndoList = ref []

            (**
             * Find dependent (tainted) nodes to remove from the graph
             * Work on the Graph:
             * - Clear all channels with partial communication
             * - Create a set of threads to kill (indirectly spawned by this thread's effects)
             * - Create a set of threads to restore (just communicated with this thread)
             * - Create a list of all references to restore
             **)
            fun nodeWork(n, ()(*acc*)) =
              let (* If a partial com, pull out the channel txnId and clear it *)
                  val () =
                    case getTxnId n
                      of NONE => ()
                       | SOME txnId =>
                          let val () = TransID.cancel txnId
                              val () = setTxnId (n, NONE)
                          in debug(fn()=> "chan cleared:" ^ nodeToString n ^ "\n")
                          end
                  (* Partition the graph into killSet and restoreTbl *)
                  val () = case (node2Tid n, node2Cont n)
                             of (SOME tid, NONE  ) => HashTable.putCond partitionTbl someAndSmaller (tid, NONE)
                              | (SOME tid  , SOME _) => (* a node that needs restoring *)
                                                      HashTable.putCond partitionTbl someAndSmaller (tid, SOME n)
                              | _ => error "Impossible node props: missing tid"
                  (* Create list of all references we need to "clean" *)
                  val nodeRefUndos = !(node2RefStack n)
                  val () = refUndoList := nodeRefUndos @ (!refUndoList)
                  val () = (node2RefStack n) := [] (* clear out the ref stack *)
              in ()
              end
            val () = dfs(startNode, nodeWork, ()(*acc*))

            (* (2) Clean up CML *)
            (* Generate a kill and restore list *)
            fun splitUp((tid, SOME n), (killAcc, restoreAcc)(*acc*)) = (killAcc       , (tid, n) :: restoreAcc)
              | splitUp((tid, NONE  ), (killAcc, restoreAcc)(*acc*)) = (tid :: killAcc, restoreAcc            )
            val (tids2Kill, threads2Restore2) = HashTable.fold partitionTbl ([], [])(*acc*) splitUp

            val _ = debug(fn() => "threads 2 restore =" ^ Int.toString(List.length(threads2Restore2)))
            val _ = debug(fn() => "about to find compensations\n")
            (*TODO: Clean up the following code: *)
            (* this should use ids and thus we can store less crap *)
            fun getCompensations(node: unit Node.t, compStack:(unit Node.t * (unit -> unit) option) list,
                                f: unit -> unit) =
              case compStack
                of (n, NONE)::xs => if Node.equals(node, n)
                                    then (f, xs)
                                    else getCompensations(node, xs, f)
                 | (n, SOME g)::xs => if Node.equals(node, n)
                                      then (g o f, xs)
                                      else getCompensations(node, xs, g o f)
                 | [] => (f, [])
            (*
             * Restore all references to their previous state.
             * - Use the list generated in dfs
             * - Bucket-sort the list into "ary" with potential empty spots
             * - Run through list in reverse, restoring references.
             *)
            val _ = debug(fn() => "starting ref undos\n")
            val () =
              case !refUndoList
                of [] => print "Api - NO refs to restore.\n"
                 | refUndoList =>
                      let fun minMax (cmp) ((id, _), acc) =
                            if cmp (id, acc)
                            then id
                            else acc
                          val min = ListN.fold'(refUndoList, 0, minMax (op <))
                          val max = ListN.fold'(refUndoList, 0, minMax (op >))
                          val _ = debug(fn() => "min=" ^ Int.toString min ^ "\n")
                          val size = max - min + 1
                          val _ = debug(fn() => "Creating array of size=" ^ Int.toString size ^ "\n")
                          val ary = Array.array(size, NONE)
                          fun putter(id, v) = (Array.update(ary, id - min, SOME v))
                          val () = ListN.foreach(refUndoList, putter)
                          (* Array is filled (undo's are sorted), now run through them. *)
        			            fun revLoop(~1) = ()
        			              | revLoop(i) =
        			              let val () =
        			                    case Array.sub(ary, i) (* "sub"script *)
        			                      of NONE    => ()
        			                       | SOME fx => (print "Api - RESTORING REFERENCE\n";fx()) (* Restore the reference *)
        			              in revLoop(i - 1)
        			              end
                          val () = revLoop(max - min)
                      in ()
                      end

            val tidAndThreads2Restore = ListN.fold'(threads2Restore2, [], (fn ((tid, x), acc) =>
                                              let val _ = debug(fn() => ("restoring thread tid=" ^ Tid.tidToString(tid) ^ "\n"))
                                                  val (compensation, newCompStack) = getCompensations(x, tid2CompStack(tid), fn () => ())
                                                  val _ = tidSetCompStack(tid, newCompStack)
                                              in case node2Cont x
                                                   of NONE => acc
                                                    | SOME c => (tid, Thread.prepend(Thread.new (!c), compensation) handle ex => (print "EXN RAISED!!"; raise ex)) :: acc
                                              end))

            val _ = debug(fn() => "done with compensations\n")

            (* (3) Clean up the graph state *)

            (* (3.1) For some tids (thise affected by stabilize) the currentThreads information must be updated.
             * two cases:
             * 1. The restoreNode has a parent stable section:
             *    Use the parent SS as the current node
             * 2. Nothing is live so create a new node
             *)
            val _ = ListN.foreach (threads2Restore2,
                      (fn ( _ (* tid *), restoreNode) =>
                        ((* We:
                          *   - Remove all outgoing edges
                          *   - Possibly pop the top element of the stable stack (if this node was a stable node)
                          *   - Remove all ref-undo's
                          *   - Remove continuation
                          *   - Remove tid (used to mark the node is "hollow")
                          *)
                         let val _ = Graph.removeAllEdges (restoreNode, Basic.tidNum ())
                             val newStack = case node2Stack restoreNode
                                              of (top::tail) => if (node2Id top) = (node2Id restoreNode)
                                                                then tail
                                                                else (top::tail)
                                               | [] => []
                             (* Remove all ref undo's *)
                             val () = (node2RefStack restoreNode) := []
                         in setNodeEnv(restoreNode, {continuation = NONE,
                                              nodeId = node2Id restoreNode,
                                              stableStack = newStack,
                                              tid = NONE})
                         end
                         )
                      ))

            val endTime = Time.now()
            (** Debugging lines: safe to remove the debug prints **)

            (* count up the size of the reachable graph ONLY for debugging *)
            fun reachableWork(_(*node*), acc) = acc + 1
            val reachableSize = dfs(startNode, reachableWork, 0(*acc*))
            val _ = print ("Api - nodeIdCounter(before)=" ^ Int.toString (counterBefore) ^ "\n")
            val _ = print ("Api - reachableSize=" ^ Int.toString (reachableSize) ^ "\n")
            val _ = print ("Api - threads2Restore2.size=" ^ Int.toString (List.length threads2Restore2) ^ "\n")
            val _ = print ("Api - tids2Kill       .size=" ^ Int.toString (List.length tids2Kill) ^ "\n")
            val _ = print ("Api - nodeIdCounter(after)=" ^ Int.toString (!nodeCount) ^ "\n")
            val _ = debug(fn() => "Api - Tids2Kill:\n")
            val _ = if DEBUG then ListN.foreach (tids2Kill, fn (tid) => print ("tid2Kill=" ^ Tid.tidToString tid ^ "\n")) else ()
            val _ = debug(fn() => "STABILIZE FINISH: " ^ Time.toString endTime ^ "\n")
            val _ = print ("Api - STABILIZE TOOK: " ^ Time.toString (Time.- (endTime, startTime)) ^ "\n")
            val _ = Thread.atomicEnd()
         in (tidAndThreads2Restore, tids2Kill)
         end

      fun debug msg = if DEBUG then print msg else ()
    end

  end;
