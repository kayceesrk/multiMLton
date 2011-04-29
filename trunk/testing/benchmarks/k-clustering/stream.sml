open ConvexHull
structure C = ConvexHull

fun pr msg = print ("test: " ^ msg ^ "\n")


fun run(f) = MLton.RunPCML.doit(f, NONE)


fun error msg = (pr msg; OS.Process.exit OS.Process.success)

fun getArg(arg) =
  let val argv = CommandLine.arguments()
      fun get([], _) = error("Command line arg too few")
        | get(x::_, 0) =
            let val i = Int.fromString(x)
            in case i
               of SOME(i) => i
               | _ => error("Command line arg incorrect arg")
            end
        | get(x::xs, c) = get(xs,c-1)
  in get(argv, arg) end

val num = getArg(11)         (* 8 *)
val iterations = getArg(12)  (* 400 *)
val clusterSize = getArg(13) (* 70 *)
val randomness = getArg(14)  (* 0 *)

val count = ref iterations

val chFilter1 = MLton.PCML.channel()
val chFilter2 = MLton.PCML.channel()
val start = MLton.PCML.channel()
val finish = MLton.PCML.channel()


fun getPair p = (case p of
                     (x,y) => (Real.floor x,Real.floor y))

fun getPairList [] = []
  | getPairList (p::rest) = getPair(p)::getPairList(rest)

fun hasPoint((x,y), points) = case (List.find (fn ((xt, yt)) => ((x =xt)
  andalso (y = yt))) points) of
                        NONE => false
                      | _ => true

fun spin(x) =
  let val y = ref 1
  in if x = 0
     then ()
     else spin(x - (!y))
  end

fun comp((y1,y2), (x1, x2)::xs, res) =
  let val res1 = Math.sqrt(Real.fromInt((y1-x1)*(y1-x1) + (y2-x2)*(y2-x2)))
      val newRes = if res < res1
                   then res1
                   else res
  in comp((x1, x2), xs, newRes)
  end
  | comp((y1, y2), [], res) = res

fun largestDistance(x::xs, curPoint, largestD) =
  let val curPoint =
        (case curPoint
           of SOME(x1, y1) => (Real.fromInt(x1), Real.fromInt(y1))
            | NONE => (0.0, 0.0))
      val largestD = comp(x, xs, largestD)
  in largestDistance(xs, NONE, largestD)
  end
  | largestDistance([], curPoint, largestD) = largestD

fun calc(set) =
  let val x = [(10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345),
               (10, 86),(45,89),(345,209),(4,675),(894,231),(4673,3),(6,9),(345,89),
               (39,23748),(34768, 34),(78, 1),(346, 67),(69,69), (4,5), (6,7), (188,345)]
      val largestD = largestDistance(x, NONE, 0.0)
      val largestD = largestDistance(x, NONE, 0.0)
      val largestD = largestDistance(x, NONE, 0.0)
      val largestD = largestDistance(x, NONE, 0.0)

  in largestD
  end

fun genSet () = (C.initPoints clusterSize)

val dataSet = Array.tabulate(num, fn _ => genSet ())
val channels = Array.tabulate(num+1, fn _ => MLton.PCML.channel())
val pref = ref([])


fun first_half (points) =
  let (*val points = !pref*)
      val _ = ()(*print (" aaaaaaaaaaaaaaaaaaaaaaaaaaa\n")*)
  in
      C.quickhull(points)
  end

fun second_half (points, newPoint) =
  let
       (*val ch = Array.sub(channels, n)
       val _ = print("waiting to recieve\n")
       val newPoint = (MLton.PCML.recv(ch))*)
      val result2 = C.quickhull((newPoint::points))
  in
    (result2, newPoint)
  end



fun filter1 (n, a, first_half, second_half) =
  let
    val points  = (Array.sub(dataSet, n))
    val _ = pref := points
      val (result1, crap) = (first_half) (points)
    val ch = Array.sub(channels, n)
    val newPoint = (MLton.PCML.recv(ch))

    val ((result2, newPoint), crap) = (second_half) (points, newPoint)
    val fwdCh = Array.sub(channels, n+1)
  in  if (C.sameHulls(result1, result2) andalso
            (not (hasPoint((newPoint), (points)))))
      then
        let
          val (L((xHull, yHull), _)::_) = result1  (* gets a point on the hull*)
          val xHull = Real.floor xHull
          val yHull = Real.floor yHull
          fun foo (x,y) = not ((x = xHull) andalso (y = yHull))
          val rest = List.filter foo points
          val _ = Array.update (dataSet, n, (rest @ [newPoint]))
        in
          (MLton.PCML.send(fwdCh, (xHull, yHull)); false)
        end
      else (MLton.PCML.send(fwdCh, newPoint);true)
  end



fun newfilter1() =
 print("I shouldnt see this yet !!!!!!!!!!!!!!!!!!!!!!!!!\n")




fun loop1(x, n, fh, sh) =
  let (*val _ = print("calling filter\n")*)
      val pref = ref([])
      val ret = (filter1 (n, pref, fh, sh))
  in
    if x = 0
          then ()
          else loop1(x-1, n, fh, sh)
  end


val streamData =
  let
    fun loop n =
      if n=0 then
        let
          val v = C.getNextPoint()
          val x = (10-randomness)*(iterations div 10)
        in
          List.tabulate(x, fn _ => v)
        end
      else
        C.getNextPoint()::loop(n-1)
     fun subLoop n =
       if n=0 then []
       else C.getNextPoint()::subLoop(n-1)

     val data = loop ((iterations div 10) * randomness)
     val data = data @ (subLoop (iterations mod 10))
  in
    data
  end



fun generator() =
  let
    val ch = Array.sub(channels, 0)
    fun loop0() =
        if !count = 0
      	then ()
       	else (count:=(!count -1); (*print("generating a value\n");*)
              MLton.PCML.send(ch, List.nth(streamData, (!count))); loop0())
      val _ = print("waiting for start\n")
      val _ = MLton.PCML.recv(start)
      val _ = print("got the start\n")
  in loop0()
  end

fun loop2(x) =
 let val _ = () (*print ("I consumed a value\n")*)
     val myCh = Array.sub(channels, num)
 in if x=0
    then MLton.PCML.send(finish, 0)
    else (MLton.PCML.recv(myCh);
          loop2(x-1))
 end



fun format(t) = LargeInt.toString(Time.toMilliseconds(t))


fun test() =
let val _ = print("starting test\n")
    val a = MLton.PCML.spawn(fn() => loop2(iterations))

    val b = List.tabulate(num,
                fn (n) => MLton.PCML.spawn(fn() => (let val _  = print("loop1 is a go\n")
                                                  val fh = MLton.PCML.Stable.stable first_half
     						 val sh = MLton.PCML.Stable.stable second_half
                                             in loop1(iterations, n, fh, sh)
                                             end)))
    val c = MLton.PCML.spawn(fn() => (print("generator is a go\n"); generator()))
    val t1 = Time.now()
    val _ = MLton.PCML.send(start, 0)
    val _ = print("gave the start signal\n")
    val _ = MLton.PCML.recv(finish)
    val t2 = Time.now()
    open Time
  val _ = (print "Testing: complete on stream of size: "; print
(Int.toString(iterations)); print "\n"; print "Run-time: ";
    print(format(t2-t1)); print" (milliseconds)\n")

in
    OS.Process.exit OS.Process.success
end

val _ =  MLton.RunPCML.doit(test, NONE)


