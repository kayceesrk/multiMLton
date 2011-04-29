
structure CML = MLton.Pacml

(***** CML BOILERPLATE *****)
val _ = run (fn() => let
(***** START CODE *****)


(**** BASIC FUNCTIONS ***)
val fold = foldr
val atomicBegin = MLton.Thread.atomicBegin
val atomicEnd = MLton.Thread.atomicEnd


(**** random number generation ****)
(*** randRange(range)::
     generates a random number within the range
 ***)
fun randRange(range) =
  let val randWord = MLton.Random.rand()
      val randWord = MLton.Random.rand()
  in Word.toInt(Word.mod(randWord, Word.fromInt(range)))
  end

fun randRangeR(rand, range) =
  Word.toInt(Word.mod(Word.fromInt(rand), Word.fromInt(range)))

(*** randList(inputRanges)::
     generates a list of length = len(inputRanges) of random numbers
     dictated by a list of ranges
 ***)
fun randList(inputRanges) =
  fold (fn(element, acc) =>
          acc@[randRange(element)])
       []
       inputRanges
(*** selectRandomElement(list)::
     selects a random element from the argument list
     TODO:: check if nth expects 0 aligned or 1 aligned
 ***)
fun selectRandomElement(lst) =
  List.nth(lst, randRange(List.length(lst)))

fun selectRandomElementR(rand, lst) =
  List.nth(lst, randRangeR(rand, List.length(lst)))

(********* debuging ***)
val debugSetup = false
val debugRun = false
val debugConsistencyCheck = false
val debugServer = false
fun debugPrint(str, check) =
    if check
    then (print("DEBUG::" ^ (str ()) ^ "\n"))
    else ()

(***** runtime metrics ***)
val abortCount = ref 0
val commitCount = ref 0
val aMemoUsedCount = ref 0
val commitedOnFirstTry = ref 0
val noMemoUsedCount = ref 0
val memosUsedAfterAbort = ref 0
val memosNotUsedAfterAbort = ref 0
val startCount = ref 0
val modifyCount = ref 0

(***** Benchmark parameters ***)
val iterations = ref 0
val transactions = ref 0
val subTransactions = ref 0
val assemblyLevels = ref 0
val assemblyFanout = ref 0
val totalAssemblies = ref 0
val totalComponents = ref 0
val totalGraphNodes = ref 0
val components = ref 0
val partGraphSize = ref 0
val rwRatio = ref 0
val tbRatio = ref 0
val topDownCount = ref 0
val bottomUpCount = ref 0


(****** Errors ***)

exception Error
fun error(str) = (print("ERROR:: " ^ str ^ "\n"); raise Error)


(****** Datatypes ****)

(* root node will be ID 0, all other nodes start with ID 1 *)
val serverCount = ref 0

fun newID() =
  let val id = !serverCount +1
      val _ = serverCount := id
  in id
  end

fun getID() = !serverCount

datatype Data = Base_Assembly
              | Complex_Assembly
              | Component
              | Part

fun DataToString(d) =
  case d
    of Base_Assembly => "Base_Assembly"
     | Complex_Assembly => "Complex_Assembly"
     | Component => "Component"
     | Part => "Part"

datatype Node = Clear
              | N of {parents:  int list,
                      children: int list,
                      value:    int,
                      dtype:    Data}

fun listToString(l) =
  let fun l2s(l, s) =
       case l
         of x::xs => l2s(xs, s ^ Int.toString(x) ^ ",")
	  | [] => s

  in "["^ l2s(l,"") ^"]"
  end

fun nToString(n) =
  case n
    of Clear => "Clear"
     | N{parents,
         children,
         value,
         dtype} =>("N of {parents:  " ^ listToString(parents) ^ ",\n"^
                   "      children: " ^ listToString(children) ^ ",\n"^
                   "      value:    " ^ Int.toString(value) ^ ",\n"^
                   "      dtype:    " ^ DataToString(dtype) ^ ",\n")

datatype 'a Server = Empty
                   | S of {ID:   int,
                           cOut: 'a CML.chan,
                           cIn:  'a CML.chan}


(****** Light Weight Servers ****)
val serverList = ref BinarySearchTree.empty
val baseAssemblyList = ref BinarySearchTree.empty
val complexAssemblyList = ref BinarySearchTree.empty
val componentList = ref BinarySearchTree.empty
val partList = ref BinarySearchTree.empty

val serverLock = CML.MutexLock.initLock ()

fun create_server(x) =
    let val _ = ()
        val (cOut, cIn) = (CML.channel(), CML.channel())
        fun server(n) =
          let val _ = CML.send(cOut, n)
	      val _ = debugPrint(fn () => "[server](sending n) \n"^ nToString(n), debugRun)
              val _ = debugPrint(fn () => "[server](sending n) \n"^ nToString(n), debugServer)
              val n = CML.recv(cIn)
              val _ = debugPrint(fn () => "[server](receiving n) \n"^ nToString(n) , debugRun)
              val _ = debugPrint(fn () => "[server](receiving n) \n"^ nToString(n), debugServer)
          in server(n)
          end
         val _ = atomicBegin()
         val _ = CML.MutexLock.getLock serverLock
         val id = newID()
         val _ = debugPrint(fn () => "[server](id) "^ Int.toString(id) ^"\n", debugServer)
         val newServer = S{ID=id,cOut=cOut, cIn=cIn}
         val _ = serverList := BinarySearchTree.insert(!serverList, (id, newServer))
         val _ = CML.MutexLock.releaseLock serverLock
         val _ = atomicEnd()
    in (CML.spawn(fn () => (
                    debugPrint(fn () => "[server](spawning with x) \n" ^ nToString(x), debugRun);
                    debugPrint(fn () => "[server](spawning with x) \n" ^ nToString(x), debugServer);
                    server(x)));
        CML.yield(); newServer)
    end

 fun sPut(S{cOut, cIn,...},  n) =
   let val previousN = CML.recv(cOut)
       val _ = debugPrint(fn () => "[sPut](previousN) \n" ^ nToString(previousN), debugRun)
       val _ = CML.send(cIn, n)
       val _ = debugPrint(fn () => "[sPut](n) \n" ^ nToString(n), debugRun)
   in ()
   end
   | sPut(Empty, n) =  error "[sPut]::trying to put on empty server"

 fun sGet(S{cOut, cIn,...}) =
   let val n = CML.recv (cOut)
       val _ = debugPrint(fn () => "[sGet](n) " ^ nToString(n), debugRun)
       val _ = CML.send (cIn, n)
       val _ = debugPrint(fn () => "[sGet](n) " ^ nToString(n), debugRun)
   in n
   end
   | sGet(Empty) = error "[sGet]::trying to get on empty server"


(**** argument processing ***)
fun getArg(arg) =
  let val argv = CommandLine.arguments()
      fun get([], _) = error("[getArg](arg)-- " ^ Int.toString(arg) ^ " --Command line arg too few")
        | get(x::_, 0) =
            let val i = Int.fromString(x)
            in case i
               of SOME(i) => i
               | _ => error("[getArg](x)-- " ^ x ^ " --Command line arg incorrect arg")
            end
        | get(x::xs, c) = get(xs,c-1)
  in get(argv, arg) end

(**** setup benchmark parameters ***)
val _ = iterations := getArg(0)
val _ = transactions := getArg(1)
val _ = subTransactions := getArg(2)
val _ = assemblyLevels := getArg(3)
val _ = assemblyFanout := getArg(4)
val _ = components := getArg(5)
val _ = partGraphSize := getArg(6)
val _ = rwRatio := getArg(7)
val _ = tbRatio := getArg(8)

(*** print benchmark parameters****)
 val _ = print("STMBench7 Input Parameters:\n")
 val _ = print("    Iterations: " ^ Int.toString(!iterations) ^ "\n")
 val _ = print("    Transactions: " ^ Int.toString(!transactions) ^ "\n")
 val _ = print("    Sub-transactions: " ^ Int.toString(!subTransactions) ^ "\n")
 val _ = print("    Assembly levels: " ^ Int.toString(!assemblyLevels) ^ "\n")
 val _ = print("    Assembly fanout: " ^ Int.toString(!assemblyFanout) ^ "\n")
 val _ = print("    Components: " ^ Int.toString(!components) ^ "\n")
 val _ = print("    Parts grpah size: " ^ Int.toString(!partGraphSize) ^ "\n")
 val _ = print("    R/W Ratio: " ^ Int.toString(!rwRatio) ^ "%\n")
 val _ = print("    Top/Bottom Ratio: " ^ Int.toString(!tbRatio) ^ "%\n")
 val _ = print("\n")


(*** setup benchmark structure ***)
fun cal_assemblies(levels, fanout) =
  let val _ = debugPrint(fn () => "[cal_assemblies](levels) " ^ Int.toString(levels), debugSetup)
      val _ = debugPrint(fn () => "[cal_assemblies](fanout) " ^ Int.toString(fanout), debugSetup)
      val _ = if (levels < 1)
              then error ("[cal_assemblies]:: levels cannot be 0 or negative")
              else ()
      val _ = if (fanout < 1)
              then error ("[cal_assemblies]:: fanout cannot be 0 or negative")
              else ()
      fun assembliesC(levels, fanout) =
        if levels = 0
        then 1
        else (fanout * assembliesC(levels-1, fanout))
      val assemblies = assembliesC(levels, fanout)
      val _ = debugPrint(fn () => "[cal_assemblies](assemblies) " ^ Int.toString(assemblies), debugSetup)
  in assemblies
  end

fun listRange(start, tail) =
  let fun construct(l, cur) =
        if cur = tail
        then l@[tail]
        else l@[cur]@construct(l, cur+1)
  in construct([], start)
  end

fun spawn_complex_assemblies(number, curLevel, assembly_list) =
  if (number = 0)
  then assembly_list
  else let val (parent, children) =
             if curLevel = !assemblyLevels
             then error "[spawn_complex_assemblies] these are base assemblies"
              else let val fanOut = !assemblyFanout
                       val id = getID()+1 (*this will be the id assigned to me *)
                       val start = (id)*fanOut
                       val tail = start+(fanOut-1)
                   in ([], listRange(start,tail))
                   end
           val new_assembly = create_server(N{parents  = parent,
                                              children = children,
                                              value    = getID()+1,
                                              dtype    = Complex_Assembly})
           val new_assembly_list = new_assembly::spawn_complex_assemblies(number-1, curLevel, assembly_list)
       in new_assembly_list
       end

fun spawn_base_assemblies(number, assembly_list, firstComponent, lastComponent) =
  if (number = 0)
  then assembly_list
  else let val new_assembly = create_server(N{parents  = [],
                                              children = listRange(firstComponent, lastComponent),
                                              value    = getID()+1,
                                              dtype    = Base_Assembly})
           val new_assembly_list = new_assembly::spawn_base_assemblies(number-1,
	       			   				       assembly_list,
								       firstComponent,
								       lastComponent)
       in new_assembly_list
       end


fun spawn_Components(number, component_list, graphStart) =
  if (number = 0)
  then component_list
  else let val new_component = create_server(N{parents  = [],
                                              children = [graphStart + randRange(!partGraphSize)],
                                              value    = getID()+1,
                                              dtype    = Component})
           val new_component_list = new_component::spawn_Components(number-1,
	       			    			            component_list,
								    graphStart)
       in new_component_list
       end


(* this function assumes the graph has a sparcity of 3
   we should probably take this as an argunment, but STMBench7 does not support this*)
fun spawn_GraphNodes(number, node_list, graphStart) =
  let val nodeID = getID()+1
      fun generateChildren(limit,  xs) =
       if (List.length(xs) = limit)
       then xs
       else let val x = graphStart + randRange(!partGraphSize)
            in if List.exists (fn(element) => if (x = element)
                                              then true
                                              else false)   xs
               then generateChildren(limit, xs)
	       else if x = nodeID
                    then generateChildren(limit, xs)
                    else generateChildren(limit, x::xs)
            end
  in if (number = 0)
     then node_list
     else let val new_node = create_server(N{parents  = [],
                                          children = generateChildren(3,[]),
                                          value    = getID()+1,
                                          dtype    = Part})
              val new_node_list = new_node::spawn_GraphNodes(number-1,
	       		       				  node_list,
							  graphStart)
          in new_node_list
       	  end
  end

fun setParent(parent, cs)=
  case cs
    of x::xs =>
         (case (sGet(BinarySearchTree.lookup (!serverList) (x)))
             of N{parents, children, value, dtype} =>
                 (sPut(BinarySearchTree.lookup (!serverList) (x),
                       N{parents = parent::parents,
                         children = children,
                    	 value = value,
                    	 dtype = dtype}); setParent(parent, xs))
              | Clear => error "[setParent] shouldnt be talking to servers with empty nodes" )
     | [] => ()

fun setupParents(limit)=
  if limit = 0
  then print("Done setting parent links.\n")
  else (print (Int.toString(limit)^"\n");
       case (sGet(BinarySearchTree.lookup (!serverList) (limit)))
         of N{parents, children, value,...} =>(setParent(value, children);setupParents(limit-1))
          | Clear => error "[setupParents] shouldnt be talking to servers with empty nodes")

fun test_assemblies(assembly_list, n) =
  case assembly_list
    of x::xs => (sGet(x); test_assemblies(xs, n+1); ())
     | [] => print (" got from " ^ Int.toString(n) ^ " assembly servers. Testing Done\n")

fun test_Components(component_list, n) =
  case component_list
    of x::xs => (sGet(x); test_Components(xs, n+1); ())
     | [] => print (" got from " ^ Int.toString(n) ^ " component servers. Testing Done\n")

fun test_GraphNodes(node_list, n) =
  case node_list
    of x::xs => (sGet(x); test_GraphNodes(xs, n+1); ())
     | [] => print (" got from " ^ Int.toString(n) ^ " parts graph node servers. Testing Done\n")



fun allAssemblyLevels(curLevel, assemblyPerLevel) =
  if curLevel = 0 (* root of the tree *)
  then let val totalAssembliesForThisLevel = 1
           val _ = print("    Spawning Assemblies: " ^ Int.toString(totalAssembliesForThisLevel) ^ " ... ")
           val _ = totalAssemblies := (!totalAssemblies + totalAssembliesForThisLevel)
           val assembly_list = spawn_complex_assemblies(totalAssembliesForThisLevel, curLevel, [])
           val _ = print("    Assemblies Spawned\n")
           val _ = print("    Testing Assemblies: " ^ Int.toString(totalAssembliesForThisLevel) ^ " ... ")
           val _ = test_assemblies(assembly_list, 0)
           val _ = print("    Level: " ^ Int.toString(curLevel) ^ " created and tested\n")
       in allAssemblyLevels(curLevel+1, assemblyPerLevel)
       end
  else
    if curLevel > (!assemblyLevels)
    then (print("    Assemblies Done\n"))
    else if curLevel = (!assemblyLevels) (* Create Base_Assemblies *)
         then
         let val totalAssembliesForThisLevel = cal_assemblies(curLevel, assemblyPerLevel)
	     val firstComponent = totalAssembliesForThisLevel + getID() + 1 (*this is the id of the first component*)
             val lastComponent = firstComponent+(!components)-1
             val _ = print("    Spawning Assemblies: " ^ Int.toString(totalAssembliesForThisLevel) ^ " ... ")
             val _ = totalAssemblies := (!totalAssemblies + totalAssembliesForThisLevel)
             val assembly_list = spawn_base_assemblies(totalAssembliesForThisLevel, [], firstComponent, lastComponent)
             val _ = print("    Assemblies Spawned\n")
             val _ = print("    Testing Assemblies: " ^ Int.toString(totalAssembliesForThisLevel) ^ " ... ")
             val _ = test_assemblies(assembly_list, 0)
             val _ = print("    Level: " ^ Int.toString(curLevel) ^ " created and tested\n")
         in allAssemblyLevels(curLevel+1, assemblyPerLevel)
         end
         else (* Create Complex_Assemblies *)
         let val totalAssembliesForThisLevel = cal_assemblies(curLevel, assemblyPerLevel)
       	     val _ = print("    Spawning Assemblies: " ^ Int.toString(totalAssembliesForThisLevel) ^ " ... ")
             val _ = totalAssemblies := (!totalAssemblies + totalAssembliesForThisLevel)
             val assembly_list = spawn_complex_assemblies(totalAssembliesForThisLevel, curLevel, [])
	     val _ = print("    Assemblies Spawned\n")
	     val _ = print("    Testing Assemblies: " ^ Int.toString(totalAssembliesForThisLevel) ^ " ... ")
      	     val _ = test_assemblies(assembly_list, 0)
             val _ = print("    Level: " ^ Int.toString(curLevel) ^ " created and tested\n")
         in allAssemblyLevels(curLevel+1, assemblyPerLevel)
       	 end

fun allComponentHashes(components, graphStart) =
  if components < 1
  then error ("[cal_assemblies]:: levels cannot be 0 or negative")
  else let val _ = print("    Spawning Assemblies: " ^ Int.toString(components) ^ " ... ")
           val _ = totalComponents := (components)
           val component_list = spawn_Components(components, [], graphStart)
           val _ = print("    Components Spawned\n")
           val _ = print("    Testing Componenets: " ^ Int.toString(components) ^ " ... ")
           val _ = test_Components(component_list, 0)
           val _ = print("    Componenets: " ^ Int.toString(components) ^ " created and tested\n")
       in print("    Components Done\n")
       end

fun allPartsGraphs(partGraphSize, graphStart) =
  if partGraphSize < 1
  then error ("[allPartsGraph]:: parts graph cannot contain 0 or negative nodes")
  else let val _ = print("    Spawning Graph Nodes: " ^ Int.toString(partGraphSize) ^ " ... ")
           val _ = totalGraphNodes := (partGraphSize)
           val node_list = spawn_GraphNodes(partGraphSize, [], graphStart)
           val _ = print("    Parts Graph Nodes Spawned\n")
           val _ = print("    Testing Parts Graph Nodes: " ^ Int.toString(partGraphSize) ^ " ... ")
           val _ = test_GraphNodes(node_list, 0)
           val _ = print("    Parts Graph: " ^ Int.toString(partGraphSize) ^ " created and tested\n")
       in print("    Parts Graph Done\n")
       end

fun setup() =
  let val _ = print("STMBench7 Setup:\n")
      val _ = debugPrint(fn () => "[setup]:: Running with DebugSetup ON ", debugSetup)
      val _ = allAssemblyLevels(0, !assemblyFanout)
      val graphStart = getID() + !components + 1
      val _ = allComponentHashes(!components, graphStart)
      val _ = allPartsGraphs(!partGraphSize, graphStart)
      val _ = setupParents(getID())
  in print("\n")
  end

(*** benchmark metadata checks ***)
(*** checkRatio()::
     basic ratio check: returns true/false based on modular arithmetic on random numbers
 ***)
fun checkRatio(ratio) =
  let val mod10 = randRange(10)
      val _ = debugPrint(fn () => "[checkRatio](mod10) " ^ Int.toString(mod10), debugRun)
  in case (!ratio)
       of 0 => false
        | 10 =>if(mod10 = 0)
               then true
               else false
     	| 20 =>if(randRange(5) = 0)
               then true
               else false
     	| 30 =>if(mod10 = 0 orelse mod10 = 1 orelse mod10 =2)
               then true
               else false
     	| 40 =>if(mod10 = 0 orelse mod10 = 1 orelse mod10 =2 orelse mod10 = 3)
               then true
               else false
     	| 50 => if(randRange(2) = 0)
                then true
                else false
     	| 60 =>if(mod10 = 0 orelse mod10 = 1 orelse mod10 =2 orelse mod10 = 3)
               then false
               else true
     	| 70 =>if(mod10 = 0 orelse mod10 = 1 orelse mod10 =2)
               then false
               else true
     	| 80 =>if(randRange(5) = 0)
               then false
               else true
     	| 90 =>if(mod10 = 0)
               then false
               else true
     	| 100 => true
        | _ => error("[checkRatio](ratio)-- " ^ Int.toString(!ratio) ^ " --Invalid Ratio")
  end

(*** checkRead()::
       checks to see if a read or a write is necessary:
       returns true if its a read
       returns false if its a write
   ***)
fun checkRead() =
  let val ret =  checkRatio(rwRatio)
      val _ = debugPrint(fn () => "[checkRead](ret) " ^ Bool.toString(ret), debugRun)
  in ret
  end
(*** checkTop()::
       checks to see if a top down or a bottom up traversal is necessary:
       returns true if its a top down
       returns false if its a bottom up
   ***)
fun checkTop() =
  let val ret =  checkRatio(tbRatio)
      val _ = debugPrint(fn () => "[checkTop](ret) " ^ Bool.toString(ret), debugRun)
  in ret
  end


(***********  transaction *************)

fun transaction(s) =
  let fun getRangeList(top) =
        let val len = !assemblyLevels *2
            val _ = debugPrint(fn () => "[getRangeList](len) " ^ Int.toString(len), debugRun)
            val range = !totalAssemblies
            fun createL(len) =
              if len = 0
              then []
              else range::createL(len-1)
            val initialL = createL(len)
            val _ = debugPrint(fn () => "[getRangeList]::intialL created ", debugRun)
            val l = fold (fn(element, acc) => (acc@[element]))
                         []
                         initialL
            val _ = debugPrint(fn () => "[getRangeList]::l created ", debugRun)
        in if top
           then let val _ = debugPrint(fn () => "[getRangeList]::reversing list ", debugRun)
                in List.rev(l)
                end
           else let val _ = debugPrint(fn () => "[getRangeList]::using initial list ", debugRun)
                in l
                end
        end

      fun generateTraversal () =
        let val read = ref (checkRead())
            val _ = () (*print "memo based on arg?\n"*)
            val _ = debugPrint(fn () => "[generateTraversal](read) " ^ Bool.toString(!read), debugRun)
            val top = checkTop()
            val _ = debugPrint(fn () => "[generateTraversal](top) " ^ Bool.toString(top), debugRun)
            val graphWalkList = randList(getRangeList(top))
            val _ = debugPrint(fn () => "[generateTraversal]::graphWalkList created ", debugRun)
            val treeWalkList = [selectRandomElement(getRangeList(top))]@
                               [selectRandomElement(getRangeList(top))]@
                               [selectRandomElement(getRangeList(top))]@
                               [selectRandomElement(getRangeList(top))]@
                               [selectRandomElement(getRangeList(top))]@
                               [selectRandomElement(getRangeList(top))]@
                               [selectRandomElement(getRangeList(top))]@
                               [selectRandomElement(getRangeList(top))]@graphWalkList
            val _ = debugPrint(fn () => "[generateTraversal]::treeWalkList created ", debugRun)
            fun walkTree(treeWalkList, choices) =
              case treeWalkList
                of x::xs =>
                   if (!read)
                   then case (sGet(BinarySearchTree.lookup (!serverList) (selectRandomElementR(x, choices))))
                          of N{children,...} => walkTree(xs, children)
                           | Clear => error "[generateTraversal] shouldnt be talking to servers with empty nodes"

                   else
                     let val node = sGet(BinarySearchTree.lookup (!serverList) (selectRandomElementR(x, choices)))
                     in ()
                     end
                 | [] => ()
            val _ = walkTree(treeWalkList, [1])
        in ()(*print("Traversal done\n")*)
        end
  in generateTraversal()
  end
(*
val s = create_server(N{parents  = [1],
                        children = [2,3,4],
                        value    = 42,
                        dtype    = Complex_Assembly})*)
fun loop(x) =
  let

     (* fun g() = (sGet(s); (*print "memo based on recv\n?"; *)sGet(s); sGet(s); sGet(s); sGet(s); sGet(s);sGet(s))*)
      val f = stab transaction
      val count = ref 0
      (*fun runF() = (print ("I got "^ Int.toString(f()) ^"\n"); sPut(s, N{parents  = [1],
                                              		 	 	   children = [2,3,4],
                                              				   value    = 4,
                                              				   dtype    = Complex_Assembly});
                    print ("I got "^ Int.toString(f()) ^"\n"); sPut(s, 2);
                    print ("I got "^ Int.toString(f()) ^"\n"))*)
    (*  fun runF() = (f(); sPut(s, N{parents  = [1],
                                   children = [2,3,4],
                                   value    = 4,
                                   dtype    = Complex_Assembly});
                    f(); sPut(s, N{parents  = [1],
                                   children = [2,3,4],
                        	   value    = 42,
                        	   dtype    = Complex_Assembly}); f())*)
  (*     fun runF() = (
         let val x = 4
             val _ = f("hello")
             val _ = print (Int.toString(x))
         in let val x = 5
                val _ = f("hello")
                val _ = print (Int.toString(x))
            in let val x = 6
                   val _ = f("hello")
               in print (Int.toString(x))
               end
            end
         end)*)
    val f =
      (fn () =>
      let
        val ch = CML.channel ()
        val _ = CML.spawn (fn () => (f (); CML.send (ch, ())))
      in
        ch
      end)

    fun runF () =
      let
        val l = List.tabulate (16, fn _ => f ())
      in
        List.app (fn ch => CML.recv ch) l
      end

  in if x = 0
     then print "done wit loop\n"
     else (runF(); loop(x-1))
  end

in
  let val t0 = Time.now()
      val _ = setup()
      val _ = print("STMBench7 Setup: Complete\n")
      val _ = print("STMBench7 Experiment:\n")
      val t1 = Time.now()
      val _ = loop(!iterations)
      val t2 = Time.now()

      val _ = print("\n")
      val _ = print("STMBench7 Experiment: Complete\n")
      val _ = print("STMBench7 Results:\n")
      val _ = print("    Transaction starts: " ^ Int.toString(!startCount) ^ "\n")
      val _ = print("    Transaction aborts: " ^ Int.toString(!abortCount) ^ "\n")
      val _ = print("    Transaction commits: " ^ Int.toString(!commitCount) ^ "\n")
      val _ = print("    Transaction never aborted: " ^ Int.toString(!commitedOnFirstTry) ^ "\n")
      val _ = print("    Modify runs: " ^ Int.toString(!modifyCount) ^ "\n")
      val _ = print("    Top down runs: " ^ Int.toString(!topDownCount) ^ "\n")
      val _ = print("    Bottom up runs: " ^ Int.toString(!bottomUpCount) ^ "\n")
      val _ = print("    A memo was used (after abort): " ^ Int.toString(!aMemoUsedCount) ^ "\n")
      val _ = print("    No memo was used (after abort): " ^ Int.toString(!noMemoUsedCount) ^ "\n")
      val _ = print("    Number of memos used (after abort): " ^ Int.toString(!memosUsedAfterAbort) ^ "\n")
      val _ = print("    Number of memos not used (after abort): " ^ Int.toString(!memosNotUsedAfterAbort) ^ "\n")

      (* val _ = Stable.printMemoStats()*)
      fun format(t) = LargeInt.toString(Time.toMilliseconds(t))
      open Time
      val _ = print("    Run-time: " ^ format(t2-t1) ^ " (milliseconds)\n")
      val _ = print("    Do-work-time: " ^ format(t1-t0) ^ " (milliseconds)\n")
      val _ = OS.Process.exit OS.Process.success

  in ()
  end
end)
