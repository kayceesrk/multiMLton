(* Signature for dictionaries *)
(*
   For simplicity, we assume keys are strings, while stored entries
   are of arbitrary type.  This is prescribed in the signature.

   Existing entries may be "updated" by inserting a new entry
   with the same key.
*)

(*signature DICT =
sig
  type key = string
  type 'a entry = key * 'a

  type 'a dict

  val empty : 'a dict
  val lookup : 'a dict -> key -> 'a
  val insert : 'a dict * 'a entry -> 'a dict
end*)


(*structure RedBlackTree :> DICT =*)
(*struct*)

val debug = false (* debug tree *)
val debug2 = false (* debug test infrastructure *)
  fun debugPrint(s) =
    if debug
    then print(s)
    else ()

fun debugPrint2(s) =
    if debug2
    then print(s)
    else ()

fun getArg(arg) =
  let val argv = CommandLine.arguments()
      fun get([], _) = raise Fail ("Command line arg too few")
        | get(x::_, 0) =
            let val i = Int.fromString(x)
            in case i
               of SOME(i) => i
               | _ => raise Fail ("Command line arg incorrect arg")
            end
        | get(x::xs, c) = get(xs,c-1)
  in get(argv, arg) end


val write_percent = getArg(0)
val repeate_percent = getArg(1)
val repeater =
  if repeate_percent = 0
  then 0
  else repeate_percent div 10
val writer =
  if write_percent = 0
  then 0
  else write_percent div 10
val test_size = getArg(2)
val _ = print "*** Starting Test ***\n"
val _ = print "write %: "
val _ = (print(Int.toString(write_percent)); print "\n")
val _ = print "repeate %: "
val _ = (print(Int.toString(repeate_percent)) ; print "\n")
val _ = print "size: "
val _ = (print(Int.toString(test_size)) ; print "\n")
val _ = print "repeater: "
val _ = (print(Int.toString(repeater)) ; print "\n")
val _ = print "writer: "
val _ = (print(Int.toString(writer)) ; print "\n")
(* 0 % *)
val nothing = fn x => x
(* 10 % *)
val mod10 = fn x => x mod 10
(* 20 % *)
val mod5 = fn x => x mod 5
(* 50 % *)
val mod2 = fn x => x mod 2

val write_mod = fn x =>
  if Word.toInt(Word.mod(MLton.Random.rand(), Word.fromInt(10))) < writer
  then (debugPrint2(" write_mod selected true\n"); true)
  else (debugPrint2(" write_mod selected false\n"); false)

val repeate_mod =  fn x =>
  if Word.toInt(Word.mod(MLton.Random.rand(), Word.fromInt(10))) < repeater
  then (debugPrint2(" repeate_mod selected true\n"); true)
  else (debugPrint2(" repeate_mod selected false\n"); false)



(**
val repeate_list = ref []
val write_list = ref []
val no_repeate_list = ref []
val counter = ref 0
**)




exception Error
fun error(str) = (print(str ^ "\n"); raise Error)


  type value = int
  val size = ref 0
  datatype color = red
                 | black
  datatype rbtree = Empty
                  | N of {cOut: node CML.chan, cIn: node CML.chan}
       and node = Node of {color: color, value: int,
                           left:rbtree, right:rbtree}
                  | EmptyNode
  fun work(x)=
    if x = 1
    then ()
    else work(x-1)
  val worker = 10000
  fun spin(lock) =
          if (!lock) = 0
          then (lock := 1)
          else spin(lock)
  fun release(lock) =
    lock := 0
  fun create_node(color:color, value:int, left:rbtree, right:rbtree):rbtree =
    let val _ = ()
        val (cOut, cIn) = (CML.channel(), CML.channel())
        val n = Node{color=color, value=value, left=left, right=right}
        fun server(n) =
          (*CML.sync(CML.choose([CML.wrap(CML.sendEvt(cOut, n), (fn x => server(n)) ),
                               CML.wrap(CML.recvEvt(cIn), (fn x =>
                               server(x)))])) *)
          let val _ = CML.send(cOut, n)
              val n = CML.recv(cIn)
          in server(n)
          end
       (*fun server(n) =
         let val _ = spin(lock)
             val n = case CML.recvPoll(cIn)
               of SOME n => (debugPrint "server recieved an input\n"; n)
                | NONE => (debugPrint "server sending a value\n";
                release(lock); CML.send(cOut, n);  n)
         in release(lock); (work(worker);server(n))
         end*)
    in CML.spawn(fn () => (debugPrint "server Spawned\n";
                           debugPrint "initial value\n"; server(n))); CML.yield(); N{cOut=cOut, cIn=cIn}
    end

  fun equal1(n, m) =
    MLton.eq(n, m)


  fun send'(N{cOut, cIn}:rbtree,  n:node):unit =
    (debugPrint "sending to server\n"; (*spin(lock); *)
     CML.recv(cOut);
     debugPrint "sending stripped value from server\n";
     CML.send(cIn, n);
     debugPrint "sending to server complete\n")
    | send'(Empty, n) = error "impossible situation 1\n"

  fun recv'(N{cOut, cIn}:rbtree):node =
    let val _ = debugPrint "receiving from server\n";
        (*val _ = spin(lock)*)
        val n = CML.recv(cOut)
        val _ = CML.send(cIn, n)
        (*val _ = release(lock)*)
        val _ = debugPrint "received from server\n"
    in n
    end
    | recv'(Empty) = (debugPrint " -- recv on empty\n"; EmptyNode)

  fun contains (n:int, t:rbtree):bool =
    let fun check(n, t) =
     case t
       of Empty => false
        | _ =>
          (case recv'(t)
             of  Node {value,left,right,...} =>
                   (case Int.compare (value, n)
                     of EQUAL => true
                      | GREATER => check (n,left)
                      | LESSER => check (n,right))
              | _ => error "impossible situation 2\n")
    in  check(n, t)
    end

  fun contains_node (n:int, t:rbtree):rbtree =
    let fun check(n, t) =
     case t
       of Empty => Empty
        | _ =>
          (case recv'(t)
             of  Node {value,left,right,...} =>
                   (case Int.compare (value, n)
                     of EQUAL => t
                      | GREATER => check (n,left)
                      | LESSER => check (n,right))
              | _ => error "impossible situation 3\n")
    in  check(n, t)
    end

  fun decrement(n:int, t:rbtree) =
    let val node = contains_node(n, t)
    in case t
         of Empty => error "decremented a non existing node "
          | _ =>
             (case recv'(t)
                of  Node {value,left,right,color} => send'(t, Node{value = value-1, color = color,
                                                                   left = left, right = right})
                 | _ => error "impossible situation 4\n")
    end

  fun rotate(v:int, v1:int, v2:int, t:rbtree, t2:rbtree, t3:rbtree, t4:rbtree):rbtree =
   let val _ = debugPrint "rotate: \n"
   in  create_node(red, v1, create_node(black,v,t,t2),
                         create_node(black,v2,t3,t4))
   end
  fun balance (t:rbtree): rbtree =
    let val _ = debugPrint("balance:\n")
    in case t
      of Empty => (debugPrint(" -- tree is empty\n"); t)
       | _ => (debugPrint " -- tree not empty about to recv\n";
    (case recv'(t)
       of  Node{color=black, value=v,
               left=l,      right=r} => (debugPrint " -- received from server\n";
            (case(recv'(l), recv'(r))
               of (Node{color=red, value=v1,
                        left=l1,   right=r1}, _) => (debugPrint " -- case one\n";
                     (case(recv'(l1), recv'(r1))
                        of (Node{color=red, value=v2,
                                 left=l2,   right=r2}, _) => rotate(v2,v1,v,l2,r2,r1,r)
                         | (_, Node{color=red, value=v2,
                                    left=l2,   right=r2}) => rotate(v1,v2,v,l1,l2,r2,r)
                         | _ => (debugPrint("  -- one subtree empty\n"); t)))
                | (_, Node{color=red, value=v1,
                           left=l1,   right=r1}) => (debugPrint " -- case two\n";
                     (case(recv'(l1), recv'(r1))
                        of (Node{color=red, value=v2,
                                 left=l2,   right=r2}, _) => rotate(v,v2,v1,l,l2,r2,r1)
                         | (_, Node{color=red, value=v2,
                                    left=l2,   right=r2}) => rotate(v,v1,v2,l,l1,l2,r2)
                         | _ => (debugPrint("  -- one subtree empty\n"); t)))
                | _ => (debugPrint("  -- one subtree empty\n"); t)))
        | _ => (debugPrint "this case\n"; t)))
    end

  fun makeBlack (t:rbtree): rbtree =
    let val _ = debugPrint("makeBlack:\n");
    in case t
       of Empty => t
        | _ =>
        (case recv'(t)
           of Node {color,value,left,right} =>(
             send'(t, Node {color=black, value=value,
                            left=left, right=right} ); t )
            | _ => error "impossible situation 5\n")
    end



fun walk (t:rbtree, n:int):rbtree =
 let val _ = debugPrint("walk:\n")
    fun walk(t:rbtree) =
    case t
       of Empty => (debugPrint " -- empty tree creating node\n"; size := (!size) +1;create_node(red, n, Empty, Empty))
        | _ => (case recv'(t)
                  of Node {color,value,left,right} =>
                      (case Int.compare (value,n)
                         of EQUAL => t
                          | GREATER => (debugPrint " -- walk greater\n";
                                        (*balance(create_node(color, value, walk left, right))*)
                                        send'(t,
                                         Node {color=color,
                                                          value=value,
                                                          left=walk left,
                                                          right=right}
                                              ); balance(t))
                          | LESSER => (debugPrint " -- walk lesser\n";
				       send'(t,
                                         Node {color=color,
                                                          value=value,
                                                          left=left,
                                                          right=walk right}
                                              ); balance(t)))
                    | _ => error "impossible situation 8\n")
 in walk(t)
 end

fun add(t:rbtree, n:int) =
  makeBlack(walk(t, n))

fun addList(t, l) =
  foldl (fn(n, t) => add(t, n)) t l
fun checkList(t, l) =
  foldl (fn(n, b) =>
    if b
    then ((print ("sucess!\n contains " ^ Int.toString(n) ^ " ")); (contains (n, t)))
    else (print "fail!\n"; b)) true l

fun rand(lastNum) =
  let open Word
      val a = fromInt(214013)
      val b = fromInt(2531011)
      val prod = !lastNum * a
      val sum = prod + b
      val shifted = >>(sum, fromInt(16))
      val result = andb(shifted, fromInt(0x7fff))
      val _ = lastNum := result
  in
    !lastNum
  end

fun random(modval) =
  let val r = MLton.Random.rand()
      val modvalWord = Word.fromInt(modval)
      val randNum = Word.toInt(Word.mod(r, modvalWord))
  in
    randNum
  end

fun incWord(num) =
  let val numRef = ref num
      val _ = rand(numRef)
  in !numRef
  end

fun selectRandomElement(list, count) =
  let val randNum = random(count)
      fun get(0, x::_) = x
        | get(n, _::lst) = get(n-1, lst)
        | get(_, _) = error("List length was incorrect")
  in
    get(randNum, list)
  end


fun createList(size, l) =
  if size = 0
  then size::l
  else createList(size-1, size::l)

fun get(l::ls, x) =
      if x <> 0
      then get(ls, x-1)
      else l
  | get([], x) = error "element out of bounds"

val repeate_list = ref []
val write_list = ref []
val no_repeate_list = ref []
val counter = ref 0
val repeate_list_size = ref 0
(*
val _ = write_list := [(contains, 79)]
val _ = no_repeate_list := [(contains, 79)]
val _ = repeate_list := [(contains, 79)]*)

(* initial test tree *)
val t = ref(Empty)
val x = test_size


fun format(t) = LargeInt.toString(Time.toMilliseconds(t))

fun test() =
let
val _ = print "Testing: RedBlack Tree - one inserts\n"
val t' = add(Empty, 1)
val _ = print "Testing: RedBlack Tree - five inserts\n"
val t' = add(add(add(add(add(add(add(add(add(t', 2), 3), 4), 5), 10), 8), 13), 15), 12)
val _ = print "Testing: Contains 5 ... "
val _ = if contains(5, t')
        then print "success\n"
        else print "fail\n"

val _ = print "Testing: Contains 4 ... "
val _ = if contains(4, t')
        then print "success\n"
        else print "fail\n"
val _ = print "Testing: Contains 3 ... "
val _ = if contains(3, t')
        then print "success\n"
        else print "fail\n"
val _ = print "Testing: Contains 2 ... "
val _ = if contains(2, t')
        then print "success\n"
        else print "fail\n"
val _ = print "Testing: Contains 1 ... "
val _ = if contains(1, t')
        then print "success\n"
        else print "fail\n"

val _ = print "Testing: batch instert\n"
val l = [20, 41, 21, 42, 22, 50, 100, 23, 90, 38, 24, 16, 49, 92, 93, 94, 14, 60, 25, 17, 48, 26, 95, 96,
         18, 27, 19, 28 ,29, 80, 101, 70, 75, 85, 35, 30, 36, 37, 81, 82, 51, 61, 52, 62, 71, 72, 73, 74]
val t' = addList(t', l)
val _ = print "Testing: batch instert completed\n"
val _ = print "Testing: batch contains\n"
val _ = if checkList(t', l)
        then print "Testing: batch contains passed\n"
        else print "Testing: batch contains failed\n"

val _ = print "Starting Test\n"
in ()
end

val _ =  RunCML.doit(test, NONE)


