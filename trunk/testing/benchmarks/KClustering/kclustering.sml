structure ConvexHull =
struct

  structure R = MLton.Random
  structure M = Math

  datatype line = L of ((real*real) * (real*real))

  val zero = Real.fromInt(0)

  fun printPoint ((x1,y1)) = print ("("^Real.toString(x1)^","^
                                         Real.toString(y1)^")")
  fun printList [] = ()
    | printList (p::rest) = (printPoint(p); print "  "; printList (rest))


  fun printLine(L((x1,y1),(x2,y2))) = print ("\n("^Real.toString(x1)^", "^Real.toString(y1)^") -> ("
        ^Real.toString(x2)^", "^Real.toString(y2)^")")

  fun getNext () =
    (Word.toInt(Word.mod(R.rand(),Word.fromInt(1000))))

  fun getNextPoint () =
    ((getNext()), (getNext()))

  fun slope (L ( (x1, y1),  (x2, y2))) =
    if Real.==(x1,x2)
    then
        NONE
    else
        (if Real.==(y1,y2)
        then
            SOME(zero)
        else
            SOME( (y2 - y1) / (x2 - x1)))

  fun orderPoints (p1 as (x1,y1),p2 as (x2,y2)) =
    if x1 < x2 then (p1,p2)
    else
      if x1 > x2 then (p2, p1)
      else
        if y1 < y2 then (p1, p2)
        else (p2,p1)

  fun sameLines (L((x1,y1),(x2,y2)), L((x3,y3), (x4,y4))) =
    Real.==(x1,x3) andalso Real.==(y1,y3) andalso Real.==(x2,x4) andalso
    Real.==(y2,y4)

  fun hasLine(l, h) = case (List.find (fn(a) => sameLines(a,l)) h) of
                        NONE => 0
                      | _ => 1

  fun hasPoint((x,y), points) = case (List.find (fn ((xt, yt)) => (Real.==(x,xt)
  andalso Real.==(y,yt))) points) of
                        NONE => false
                      | _ => true

 fun removeDuplicatePoints([], n) = []
   | removeDuplicatePoints(p::rest, n) =
   if n=0 then p::rest
   else
    if hasPoint(p, rest) then
      removeDuplicatePoints(rest, n -1)
    else
      removeDuplicatePoints(rest @ [p], n-1)


  fun sameHulls (h1, h2) =
    let
      val count = foldl (fn(l, acc) => hasLine(l, h2) + acc) 0 h1
    in
      count = List.length(h2)
    end


  fun onLeft((px,py), line as L((x1,y1),(x2,y2))) =
      case slope(line) of
           NONE => (if (px < x1)
                    then true
                    else
                        (if Real.==(px,x1)
                        then
                            (if ((px > y1) andalso (py < y2))
                                orelse
                                ((py > y2) andalso (py < y1))
                            then true
                            else false)
                        else false)
                    )
         | SOME slope =>
                 let
                     val tenK = Real.fromInt(10000)
                     val x3 = (((px + slope * (slope * x1 - y1 + py)) /
                             (Real.fromInt(1) + slope * slope)) * tenK)
                     val y3 = ((slope * (x3 / tenK - x1) + y1) * tenK)
                 in
                    if (Real.==(slope,zero))
                    then
                        if ((py*tenK) > y3) then true else false
                    else
                        (if slope > (zero)
                        then
                            if (x3 > (px * tenK)) then true else false
                        else
                            if ((px * tenK) > x3) then true else false)
                 end

  fun initPoints n =
    if n=0 then []
    else (getNext(), getNext())::initPoints(n-1)


  fun bruteforce (points : (int*int) list) =
    let
        val points = List.map (fn(x,y) => (Real.fromInt(x),Real.fromInt(y))) points
        val points = removeDuplicatePoints (points, List.length(points))
        val points = Array.fromList (points)
        val hull = ref []
        val leftMost = ref true
        val rightMost = ref true
        fun loop1 cx =
            if cx=Array.length(points) then ()
            else
            (* Loop1 body *)
            let
                fun loop2 cy =
                    if cy = Array.length(points) then ()
                    else
                    (* Loop2 body *)
                    let
                        val leftMost = ref true
                        val rightMost = ref true
                        val temp = L(Array.sub(points, cx),
                                     Array.sub(points, cy))
                        fun loop3 cz =
                            if cz = Array.length(points) then ()
                            else
                            (* Loop3 body *)
                            let
                                val _ = (if (not (cz=cx)) andalso (not (cz=cy))
                                        then
                                            (if onLeft(Array.sub(points, cz),
                                            temp)
                                            then leftMost := false
                                            else rightMost := false)
                                        else ())

                            in
                                loop3 (cz+1)
                            end
                            (* Loop3 body ends *)
                        val _ = loop3 0 (* run loop3 *)
                        val p1 as (x1,y1) = Array.sub(points, cx)
                        val p2 as (x2,y2) = Array.sub(points, cy)
                        val newLn = L(orderPoints(p1,p2))
                        val _ = if ((!leftMost) orelse (!rightMost))
                                then hull := newLn::(!hull)
                                else ()
                    in
                        loop2 (cy+1)
                    end
                    (* Loop2 body ends *)
              val _ = loop2 (cx+1) (* run loop2 *)
            in
                loop1 (cx+1)
            end
            (* Loop1 body ends *)
        val _ = loop1 0 (* run loop1 *)

    in
        !hull
    end


  fun splitAt(p, l as (lx, ly), r as (rx,ry)) =
  let
      val maxDist = ref 0.0
      val newLn = L(l,r)
      val x3 = ref 0.0
      val y3 = ref 0.0
      val farPt = ref 0

      fun loop1 i =
          if i=(Array.length(p) -2) then ()
          else
              let
                val (xi, yi) = Array.sub(p,i)
                val s = slope(newLn)

                val _ = (case s of
                        NONE => (x3 := lx;
                                    y3 := yi
                                    )
                      | SOME s => if Real.==(ry, ly) then
                                        (x3 :=  xi
                                        ;y3 := ly)
                                    else
                                        (x3 := ((xi + s * ((s * lx) - ly + yi))
                                                / (1.0 + (s * s)));
                                        y3 := ((s * ((!x3) - lx)) + ly)))
                val distance = M.sqrt((M.pow((yi - (!y3)), 2.0)) +
                (M.pow((xi-(!x3)), 2.0)))

                val _ = if (distance) > (!maxDist) then
                            (maxDist := distance;
                            farPt := i)
                        else ()
              in
                  loop1 (i+1)
              end
      val _ = loop1 0
  in
      !farPt
  end

 fun appendHulls(h1, h2) =
   if (List.length(h1) = 1) andalso (hasLine(List.nth(h1,0), h2) = 1) then h2
   else if (List.length(h2) = 1) andalso (hasLine(List.nth(h2,0), h1) = 1) then
     h1
        else h1 @ h2


 fun quick(p, l, r, faceDir) =
  let

      val p = Array.fromList(p)
      val res : line list  = if (Array.length(p) = 2) then
                [L(orderPoints(Array.sub(p,0), Array.sub(p,1)))]
              else
            let
                val hAt = splitAt(p, l, r)
                val lh = L(l, Array.sub(p, hAt))
                val hr = L(Array.sub(p, hAt), r)
                val p1 = ref []
                val p2 = ref []

                fun loop1 i =
                    if i=(Array.length(p) - 2)  then ()
                    else
                        let
                          val pi = Array.sub(p,i)
                          val _ = if (not (i=hAt)) then
                                    (if (faceDir = 0) then
                                    (if (onLeft(pi, lh)) then
                                        p1 := ((!p1) @ [pi])
                                    else ();
                                     if (onLeft(pi, hr)) then
                                         p2 := ((!p2) @ [pi])
                                     else ()
                                    )
                                    else
                                    (if not (onLeft(pi, lh)) then
                                        p1 := ((!p1) @ [pi])
                                     else ();
                                     if not (onLeft(Array.sub(p, i), hr)) then
                                         p2 := ((!p2) @ [pi])
                                     else ()))
                                  else ()
                        in
                            loop1 (i+1)
                        end
                val _ = loop1 0

                val h = Array.sub(p, hAt)
                val _ = p1 := ((!p1) @ [l])
                val _ = p1 := ((!p1) @ [h])

                val _ = p2 := ((!p2) @ [h])
                val _ = p2 := ((!p2) @ [r])

                val res = (if (faceDir = 0) then
                                    appendHulls((quick(!p1, l, h, 0)),(quick(!p2, h, r, 0)))
                                    else
                                    appendHulls((quick(!p1, l, h, 1)),(quick(!p2, h, r, 1))))

            in
                res
            end

  in
      res
  end


  fun quickhull (points) : line list =
  let
      val points = List.map (fn(x,y) => (Real.fromInt(x),Real.fromInt(y))) points
      val points = removeDuplicatePoints (points, List.length(points))
      val points = Array.fromList (points)
      val hull = ref []
      val p1 = ref []
      val p2 = ref []
      val (x1,y1) = Array.sub (points, 0)
      val l = ref (Array.sub(points, 0))
      val r = ref (Array.sub(points, 0))
      val minX = ref x1
      val maxX = ref x1
      val minAt = ref 0
      val maxAt = ref 0

      fun loop1 i =
          if i=(Array.length(points)) then ()
          else
              let
                val (xi, yi) = Array.sub(points, i)
                val _ =
            (if xi > (!maxX) then
                (r := Array.sub(points, i);
                maxX := xi;
                maxAt := i)
            else
            (if xi < (!minX) then
                (l := Array.sub(points, i);
                minX := xi;
                minAt := i)
            else
                ()))
              in
                  loop1 (i+1)
              end
      val _ = loop1 1
      val lr = L( !l, !r)
      fun loop2 i =
          if i=Array.length(points) then ()
          else
              let
                val pi = Array.sub(points, i)
                val _ =
                (if ((not (i=(!maxAt))) andalso (not (i=(!minAt))))
                then
                    (if( onLeft(pi, lr)) then
                        (p1 := ((!p1) @ [pi]))
                    else
                        (p2 := ((!p2) @ [pi])))
                else
                    ())
              in
                  loop2 (i+1)
              end
      val _ = loop2 0

      val _ = p1 := ((!p1) @ [!l])
      val _ = p1 := ((!p1) @ [!r])

      val _ = p2 := ((!p2) @ [!l])
      val _ = p2 := ((!p2) @ [!r])
    val res1 = quick ((!p1), (!l), (!r), 0)
    val res2 = quick ((!p2), (!l), (!r), 1)

  in
      appendHulls( res1, res2 )
  end





  val testList =
    [(0.3215348546593775, 0.03629583077160248),
    (0.02402358131857918, ~0.2356728797179394),
    (0.04590851212470659, ~0.4156409924995536),
    (0.3218384001607433, 0.1379850698988746),
    (0.11506479756447, ~0.1059521474930943),
    (0.2622539999543261, ~0.29702873322836),
    (~0.161920957418085, ~0.4055339716426413),
    (0.1905378631228002, 0.3698601009043493),
    (0.2387090918968516, ~0.01629827079949742),
    (0.07495888748668034, ~0.1659825110491202),
    (0.3319341836794598, ~0.1821814101954749),
    (0.07703635755650362, ~0.2499430638271785),
    (0.2069242999022122, ~0.2232970760420869),
    (0.04604079532068295, ~0.1923573186549892),
    (0.05054295812784038, 0.4754929463150845),
    (~0.3900589168910486, 0.2797829520700341),
    (0.3120693385713448, ~0.0506329867529059),
    (0.01138812723698857, 0.4002504701728471),
    (0.009645149586391732, 0.1060251100976254),
    (~0.03597933197019559, 0.2953639456959105),
    (0.1818290866742182, 0.001454397571696298),
    (0.444056063372694, 0.2502497166863175),
    (~0.05301752458607545, ~0.06553921621808712),
    (0.4823896228171788, ~0.4776170002088109),
    (~0.3089226845734964, ~0.06356112199235814),
    (~0.271780741188471, 0.1810810595574612),
    (0.4293626522918815, 0.2980897964891882),
    (~0.004796652127799228, 0.382663812844701),
    (0.430695573269106, ~0.2995073500084759),
    (0.1799668387323309, ~0.2973467472915973),
    (0.4932166845474547, 0.4928094162538735),
    (~0.3521487911717489, 0.4352656197131292),
    (~0.4907368011686362, 0.1865826865533206),
    (~0.1047924716070224, ~0.247073392148198),
    (0.4374961861758457, ~0.001606279519951237),
    (0.003256207800708899, ~0.2729194320486108),
    (0.04310378203457577, 0.4452604050238248),
    (0.4916198379282093, ~0.345391701297268),
    (0.001675087028811806, 0.1531837672490476),
    (~0.4404289572876217, ~0.2894855991839297)]


(*val points = List.tabulate(20, fn _ => (0.0,0.0))
  val points = removeDuplicatePoints ((1.0,1.0)::points, List.length(points))
  val _ = printList points

  val resultHull = quickhull(points)*)
(*  val res = bruteforce(testList)

  val _ = if sameHulls(resultHull, res) then print "\nWorks" else
   print "\nFails" *)
  fun printRes ([]) = ()
    | printRes (l::rest) =
        ( printLine l
        ; printRes rest)

(*  val _ = printRes (resultHull)*)
end



open ConvexHull
structure C = ConvexHull

fun pr msg = print ("test: " ^ msg ^ "\n")


fun run(f) = MLton.Pacml.run(f)


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

val num = getArg(1)         (* 8 *)
val iterations = getArg(2)  (* 400 *)
val clusterSize = getArg(3) (* 70 *)
val randomness = getArg(4)  (* 0 *)

val count = ref iterations

val chFilter1 = MLton.Pacml.channel()
val chFilter2 = MLton.Pacml.channel()
val start = MLton.Pacml.channel()
val finish = MLton.Pacml.channel()


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
val channels = Array.tabulate(num+1, fn _ => MLton.Pacml.channel())
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
       val newPoint = (MLton.Pacml.recv(ch))*)
      val result2 = C.quickhull((newPoint::points))
  in
    (result2, newPoint)
  end



fun filter1 (n, a, first_half, second_half) =
  let
    val points  = (Array.sub(dataSet, n))
    val _ = pref := points
      val (result1) = (first_half) (points)
    val ch = Array.sub(channels, n)
    val newPoint = (MLton.Pacml.recv(ch))

    val ((result2, newPoint)) = (second_half) (points, newPoint)
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
          (MLton.Pacml.send(fwdCh, (xHull, yHull)); false)
        end
      else (MLton.Pacml.send(fwdCh, newPoint);true)
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
              MLton.Pacml.send(ch, List.nth(streamData, (!count))); loop0())
      val _ = print("waiting for start\n")
      val _ = MLton.Pacml.recv(start)
      val _ = print("got the start\n")
  in loop0()
  end

fun loop2(x) =
 let val _ = () (*print ("I consumed a value\n")*)
     val myCh = Array.sub(channels, num)
 in if x=0
    then MLton.Pacml.send(finish, 0)
    else (MLton.Pacml.recv(myCh);
          loop2(x-1))
 end



fun format(t) = LargeInt.toString(Time.toMilliseconds(t))


fun test() =
let val _ = print("starting test\n")
    val a = MLton.Pacml.spawn(fn() => loop2(iterations))

    val b = List.tabulate(num,
                fn (n) => MLton.Pacml.spawn(fn() => (let val _  = print("loop1 is a go\n")
                                                  val fh = first_half
     						                                  val sh =  second_half
                                             in loop1(iterations, n, fh, sh)
                                             end)))
    val c = MLton.Pacml.spawn(fn() => (print("generator is a go\n"); generator()))
    val t1 = Time.now()
    val _ = MLton.Pacml.send(start, 0)
    val _ = print("gave the start signal\n")
    val _ = MLton.Pacml.recv(finish)
    val t2 = Time.now()
    val d = Time.-(t2, t1)
    val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
in
    OS.Process.exit OS.Process.success
end

val _ =  MLton.Pacml.run (test)
