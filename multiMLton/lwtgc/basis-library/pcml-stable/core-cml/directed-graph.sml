(* From MLton's DIrectedGraph. Removed internal list ("nodes" func) and unused funcs
 *
 * Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(*structure PropertyList = PropertyList (ExnHetContainer ())*)
structure GCAbleGraph =
struct


structure Types =
   struct
      datatype node = Node of {successors: edge list ref,
			       plist: PropertyList.t}
      and edge = Edge of {from: node,
			  to: node,
			  plist: PropertyList.t}
   end

structure Edge =
   struct
      datatype t = datatype Types.edge

      local
	 fun make sel (Edge r) = sel r
      in
	 val to = make #to
      end
   end

structure Node =
   struct
      datatype t = datatype Types.node

      fun layout _ = Layout.str "node"

      fun successors (Node {successors, ...}) = !successors
      fun plist (Node {plist, ...}) = plist

      fun new () = Node {successors = ref [],
			 plist = PropertyList.new ()}

      fun equals (n, n') = PropertyList.equals (plist n, plist n')

   end

structure Edge =
   struct
      open Edge

      fun new {from, to} =
	 Edge {from = from,
	       to = to,
	       plist = PropertyList.new ()}

   end

(*---------------------------------------------------*)
(*                  graph datatype                   *)
(*---------------------------------------------------*)

datatype t = T of {nodes: Node.t list ref}

val lock = Lock.initCmlLock ()

fun addEdge (e as {from = Node.Node {successors, ...}, ...}, tid) =
   let
     val _ = MLtonThread.atomicBegin ()
     val _ = Lock.getCmlLock lock tid
      val e = Edge.new e
      val () = successors := e::(!successors)
     val _ = Lock.releaseCmlLock lock tid
     val _ = MLtonThread.atomicEnd ()
   in
      e
   end

fun removeAllEdges(Node.Node{successors, ...}, tid) =
   let
     val _ = MLtonThread.atomicBegin ()
     val _ = Lock.getCmlLock lock tid
     val _ = successors:= []
     val _ = Lock.releaseCmlLock lock tid
     val _ = MLtonThread.atomicEnd ()
   in
      ()
   end

structure Node =
   struct
      open Node

      type 'a t = t
   end

end

