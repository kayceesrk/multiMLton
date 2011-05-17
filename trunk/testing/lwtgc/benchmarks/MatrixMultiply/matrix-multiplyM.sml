(* Matrix-multiply Pacml
 * -------------------
 *  This implementation uses network of channels to multiply 2 nXn matrices.
 *)

structure Main =
struct
  open MLton.Pacml
  structure Array = Array2

  val dim = ref 5

  fun createLoop(rowNum, optInCh, firstInLink, rootLink, leftMat, resMat) =
  let
    val (inCh,firstInLink) = (case optInCh of
                 NONE => let val c = channel() in (c,c) end
               | SOME(ch) => (ch,firstInLink))
    val outCh = (if rowNum= (!dim - 1)
                 then firstInLink
                 else channel())
    val _ = (if rowNum= (!dim -1)
             then ()
             else createLoop(rowNum+1, SOME(outCh), firstInLink, rootLink,
             leftMat, resMat))

    (* initialize *)
    (* val _ = print("\nInitializing : "^Int.toString(rowNum)) *)
    val argV = Vector.fromList([Real.fromInt(rowNum)])
    val _ = send (rootLink, argV)
    val colV = recv rootLink
    val rowV = Array.row(leftMat, rowNum)

    fun work(colV, colNum) =
    let
        fun mult(res, n) =
          (case n of
               0 => Vector.sub(rowV,0) * Vector.sub(colV,0) + res
             | _ => mult(Vector.sub(rowV,n) * Vector.sub(colV,n) + res, n-1)
          )
        (* Get the result slot *)
        val res = Array.sub(!resMat,rowNum, colNum)
        val _ = res := mult(0.0, !dim-1)
        val newColV =
          if rowNum = 0 then
            let
   (*val _ = print("\n\nReceiving at "^Int.toString(rowNum)^  " column
     "^Int.toString((colNum -1 + !dim) mod !dim))*)
              val c = recv inCh
    (*val _ = print("\nSending at "^Int.toString(rowNum)^" column "
              ^Int.toString(colNum))*)
              val _ = send(outCh, colV)
            in
              c
            end
          else
            ((*print("\n\nSending at "^Int.toString(rowNum)^" column "
            ^Int.toString(colNum));*)
            send(outCh, colV);
             (*print("\nReceiving at "^Int.toString(rowNum)^" column "
             ^Int.toString((colNum - 1 + !dim) mod !dim));*)
            recv inCh)
      val newColNum = (colNum - 1 + !dim) mod (!dim)

    in
      if newColNum=rowNum
      then
        (if rowNum = 0
         then
            send(rootLink, Vector.fromList([Real.fromInt(~1)]))
         else
           ()
        )
      else
        work(newColV, newColNum)
    end

    val _ = spawn(fn()=> work(colV, rowNum))

  in
    ()
  end

  fun doit' () = run(fn()=>
    let
      val _ = dim := 100
      val a = (Array.tabulate Array.RowMajor (!dim, !dim, fn (r, c) =>
                            Real.fromInt (r + c)))
      val resMat = ref (Array.tabulate Array.RowMajor (!dim, !dim, fn(r,c) =>
                            let
                              val x = ref 0.0
                            in
                              x
                            end))

      val ch = channel()

      fun initializer() =
      let
        val v = Vector.sub((recv ch), 0)
      in
        if Real.== (v,Real.fromInt(~1))
        then
          ((*Array.app Array.RowMajor (fn(x) => print(Real.toString(!x)^" "))

        (!resMat);*) ())
        else
          (send(ch, Array.column(a,Real.floor(v)));initializer())
      end

      val _ = spawn(fn()=>
      createLoop(0,NONE,channel()(*dummy*),ch, a, resMat))
    in
      initializer()
    end)

  fun doit n =
    if n =0 then ()
    else (doit'();doit(n-1))
    (* n = 2 *)
end

val n =
   case CommandLine.arguments () of
      [] => 1
    | s::_ => (case Int.fromString s of
                  NONE => 1
                | SOME n => n)

val ts = Time.now ()
val _ = TextIO.print "\nStarting main"
val _ = Main.doit n
val te = Time.now ()
val d = Time.-(te, ts)
val _ = TextIO.print (concat ["Time start: ", Time.toString ts, "\n"])
val _ = TextIO.print (concat ["Time end:   ", Time.toString te, "\n"])
val _ = TextIO.print (concat ["Time diff:  ", LargeInt.toString (Time.toMilliseconds d), "ms\n"])
