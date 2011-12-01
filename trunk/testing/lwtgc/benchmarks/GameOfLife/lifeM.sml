(* Conway's game of Life in parallel
 * ----------------------------------
 * The board is split into multiple columns. Master assigns each column to
 * a slave.
 *
 * TODO: Master gathers result in the same order in which the slaves were
 * created. Use select() to receive results from any of the slaves.
 *)


structure Main =
  struct
    open MLton.Pacml
    val print = TextIO.print

    fun map f [] = []
      | map f (a::x) = f a :: map f x

    exception ex_undefined of string
    fun error str = raise ex_undefined str

    fun accumulate f = let
          fun foldf a [] = a
            | foldf a (b::x) = foldf (f a b) x
          in
            foldf
          end

    fun filter p = let
          fun consifp x a = if p a then a::x else x
          in
            rev o accumulate consifp []
          end


    fun exists p = let fun existsp [] = false
                     | existsp (a::x) = if p a then true else existsp x
                in existsp end

    fun equal a b = (a  = b)

    fun member x a = exists (equal a) x

    fun C f x y = f y x

    fun cons a x = a::x

    fun revonto x = accumulate (C cons) x

    fun length x = let fun count n a = n+1 in accumulate count 0 x end

    fun repeat f = let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
                       fun check n = if n<0 then error "repeat<0" else n
                    in rptf o check end

    fun copy n x = repeat (cons x) n []

    fun spaces n = concat (copy n " ")

    local
      fun lexordset [] = []
        | lexordset (a::x) = lexordset (filter (lexless a) x) @ [a] @
                             lexordset (filter (lexgreater a) x)
      and lexless(a1:int,b1:int)(a2,b2) =
           if a2<a1 then true else if a2=a1 then b2<b1 else false
      and lexgreater pr1 pr2 = lexless pr2 pr1
      fun collect f list =
             let fun accumf sofar [] = sofar
                   | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
              in accumf [] list
             end
      fun occurs3 x =
          (* finds coords which occur exactly 3 times in coordlist x *)
          let fun f xover x3 x2 x1 [] = diff x3 xover
                | f xover x3 x2 x1 (a::x) =
                   if member xover a then f xover x3 x2 x1 x else
                   if member x3 a then f (a::xover) x3 x2 x1 x else
                   if member x2 a then f xover (a::x3) x2 x1 x else
                   if member x1 a then f xover x3 (a::x2) x1 x else
                                       f xover x3 x2 (a::x1) x
              and diff x y = filter (not o member y) x
           in f [] [] [] [] x end
     in
      abstype generation = GEN of (int*int) list
        with
          fun alive (GEN livecoords) = livecoords
          and mkgen coordlist = GEN (lexordset coordlist)
          and filterRows s e gen =
            let val living = alive gen
                fun good(x,y) = x>=s andalso x <=e
                val filtered = filter good living
            in
                mkgen (filtered)
            end
          and getMinMaxX gen =
            let
              val living = alive gen
              val maxInt = case Int.maxInt of
                                        NONE => 9999
                                      | SOME(x) => x
              val minInt = case Int.minInt of
                                        NONE => (0-9999)
                                      | SOME(x) => x
              fun foo(a,b) =
                  let
                      val (x,y) = a
                      val (min,max) = b
                      val nMin = if x<min then x else min
                      val nMax = if x>max then x else max
                  in
                      (nMin,nMax)
                  end
            in
              foldl foo (maxInt, minInt) living
            end
          and mk_nextgen_fn neighbours gen =
              let val living = alive gen
                  val isalive = member living
                  val liveneighbours = length o filter isalive o neighbours
                  fun twoorthree n = n=2 orelse n=3
                  val survivors = filter (twoorthree o liveneighbours) living
                  val newnbrlist = collect (filter (not o isalive) o neighbours) living
                  val newborn = occurs3 newnbrlist
               in mkgen (survivors @ newborn) end
     end
    end

    fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
                            (i,j-1),(i,j+1),
                            (i+1,j-1),(i+1,j),(i+1,j+1)]

    local val xstart = 0 and ystart = 0
          fun markafter n string = string ^ spaces n ^ "0"
          fun plotfrom (x,y) (* current position *)
                       str   (* current line being prepared -- a string *)
                       ((x1,y1)::more)  (* coordinates to be plotted *)
              = if x=x1
                 then (* same line so extend str and continue from y1+1 *)
                      plotfrom(x,y1+1)(markafter(y1-y)str)more
                 else (* flush current line and start a new line *)
                      str :: plotfrom(x+1,ystart)""((x1,y1)::more)
            | plotfrom (x,y) str [] = [str]
           fun good (x,y) = x>=xstart andalso y>=ystart
     in  fun plot coordlist = plotfrom(xstart,ystart) ""
                                 (filter good coordlist)
    end


    infix 6 at
    fun coordlist at (x:int,y:int) = let fun move(a,b) = (a+x,b+y)
                                      in map move coordlist end
    val rotate = map (fn (x:int,y:int) => (y,~x))

    val glider = [(0,0),(0,2),(1,1),(1,2),(2,1)]
    val bail = [(0,0),(0,1),(1,0),(1,1)]
    fun barberpole n =
       let fun f i = if i=n then (n+n-1,n+n)::(n+n,n+n)::nil
                       else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
        in (0,0)::(1,0):: f 0
       end

    val genB = mkgen(glider at (2,2) @ bail at (2,12)
                     @ rotate (barberpole 4) at (5,20))

    fun printGen gen = let
      val living = alive gen
      fun printPoint x =
        let
          val (a,b) = x
        in
          print ("("^Int.toString(a)^","^Int.toString(b)^") ")
        end
    in
      (List.map printPoint living; print "\n")
    end

    fun slave id () : ((generation * int * int * int * int) option chan * generation chan)= let
      val ch1 = channel()
      val ch2 : generation chan = channel()
      fun work () =
        case recv ch1 of
             NONE => ()
           | SOME (gen, s, e, min, max) =>
                let
                  val gen = filterRows (s-1) (e+1) gen
                  val u_ngen = mk_nextgen_fn neighbours gen
                  val maxInt = case Int.maxInt of
                                    NONE => 9999
                                  | SOME(x) => x
                  val minInt = case Int.minInt of
                                    NONE => (0-9999)
                                  | SOME(x) => x

                  val s = if s=min then minInt else s
                  val e = if e=max then maxInt else e
                  val f_ngen = filterRows s e u_ngen
                  val _ = send (ch2, f_ngen)
                in
                  work ()
                end
    in
      spawn work;
      (ch1, ch2)
    end


   fun master gen sl numSlaves =
   let
     val (min,max) = getMinMaxX gen
     val size = (max-min+1) div numSlaves
     val r = ((max-min+1) mod numSlaves)
     val pos = min
     (* Creates a slave, assigns work to it and returns channel *)
     fun assign ((ch, _), (pos, r)) =
      let
        val _ = MLton.size gen
        (* must have 1 row extra at the top and bottom *)
        val s = pos
        val e = if r > 0 then (pos + size) else (pos + size -1)
        val (r, pos) = if r > 0 then (r - 1, pos + 1) else (r, pos)
        val _ = send (ch, SOME (gen, s, e, min, max))
        val pos = pos + size
      in
        (pos, r)
      end

     val _ = List.foldl assign (pos, r) sl
     val resultList = List.map (fn (_,ch) => alive (recv ch)) sl
     val result = List.foldl (fn (a,b) => a@b) [] resultList
   in
     mkgen result
   end

   fun show pr = (app (fn s => (pr s; pr "\n"))) o plot o alive

   fun nthgen_cml g 0 sl numSlaves =
                     ((* print "0 : ";
                      printGen g; *)
                      show print g;
                      print "\n\n";
                      g)
     | nthgen_cml g i sl numSlaves =
                    (print (Int.toString(i)^" "^(Int.toString(List.length(alive g)))^"\n");
                     (*printGen g;
                     print "------------------------------------------------\n";
                     show print g; *)
                     nthgen_cml (master g sl numSlaves) (i-1) sl numSlaves)

    val gun = mkgen
     [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
      (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
      (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
      (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
      (9,29),(9,30),(9,31),(9,32)]


    val alGenB = alive genB
    val genC = mkgen (alGenB at (2,2) @
                      alGenB at (3,3) @
                      alive(gun) at (4, 4) @
                      glider at (8, 2) @
                      (barberpole 50) at (0,0) @
                      (barberpole 50) at (5,5) @
                      (barberpole 50) at (5,10) @
                      (barberpole 50) at (15,10) @
                      (barberpole 50) at (5,20) @
                      (barberpole 50) at (25,20) @
                      (barberpole 50) at (5,30) @
                      (barberpole 50) at (35,30) @
                      (barberpole 50) at (5,40))

    fun foo numSlaves gens () =
      let
        val sl = List.tabulate (numSlaves, fn id => slave id ())
        val g = (nthgen_cml genC gens sl numSlaves)
      in
        (shutdown OS.Process.success)
      end

    fun doit n gens =
        run (foo n gens)
        (* n = ANY *)
  end (* Life *)

val (n, gens) =
   case CommandLine.arguments () of
     s1::s2::_ => (case (Int.fromString s1,
                        Int.fromString s2) of
                       (SOME n, SOME gens) => (n, gens)
                     | _ => (16, 1000))
    | _ => (16, 1000)

val ts = Time.now ()
val _ = TextIO.print "Starting main\n"
val _ = TextIO.print ("numThreads: "^(Int.toString n)^"\n")
val _ = TextIO.print ("numGenerations: "^(Int.toString gens)^"\n")
val _ = Main.doit n gens
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
