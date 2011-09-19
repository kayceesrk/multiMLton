(* fun-queue.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure CirQueue : CIR_QUEUE =
struct

  datatype 'a t = T of {arr : 'a option array ref, size: int ref, rp: int ref, wp: int ref}

  exception CirQueueFull
  exception CirQueueEmpty

  fun new (startSize) =
  let
    val arr = Array.tabulate (startSize, fn _ => NONE)
  in
    T {arr = ref arr, size = ref startSize, rp = ref 0, wp = ref 0}
  end

  fun grow (T {rp, wp, size, arr}) =
  let
    val oldSize = !size
    val newSize = oldSize * 2
    val (rpv, wpv) = (!rp, !wp)
    val newArr = Array.tabulate (oldSize * 2, fn i =>
                   if (i < oldSize) then
                    let
                      val index = (rpv + i) mod oldSize
                    in
                      Array.sub (!arr, index)
                    end
                   else
                     NONE)
    val _ = arr := newArr
    val _ = rp := 0
    val _ = wp := oldSize
    val _ = size := newSize
    (* val _ = print ("CirQueue.grow to size "^(Int.toString newSize)^"\n") *)
  in
    ()
  end

  fun shrink (T {rp, wp, size, arr}) =
    if (!size < 64) then
      ()
    else
      let
        (* val _ = print ("CirQueue.shrink to size 64\n") *)
        val newArr = Array.tabulate (64, fn _ => NONE)
        val _ = arr := newArr
        val _ = rp := 0
        val _ = wp := 0
        val _ = size := 64
      in
        ()
      end

  fun isFull (T {rp, wp, size, ...}) =
    (!rp) = (!wp)

  fun isEmpty (T {rp, wp, size, ...}) =
    (!rp) = (!wp)

  fun enque (q as T {arr, size, wp, ...}, e) =
    let
      val _ = case e of
                    NONE => raise Fail "Trying to insert none-sence"
                  | _ => ()
      val _ = Array.update (!arr, !wp, e)
      val _ = wp := ((!wp + 1) mod (!size))
    in
      if (isFull (q)) then
        grow (q)
      else
        ()
    end

  fun deque (q as T {arr, size, rp, ...}) =
    if (isEmpty (q)) then
      (shrink (q);
       NONE)
    else
      let
        val e = Array.sub (!arr, !rp)
        val _ = Array.update (!arr, !rp, NONE)
        val _ = rp := ((!rp + 1) mod !size)
      in
        e
      end

  (* val enque =
    fn (q as T {arr, size, rp, wp}, e) =>
    let
      val _ = print (concat ["Before Enque: rp=",Int.toString (!rp)," wp=", Int.toString (!wp), "\n"])
      val res = enque (q, e)
      val _ = print (concat ["After Enque: rp=",Int.toString (!rp)," wp=", Int.toString (!wp), "\n"])
    in
      res
    end

  val deque =
    fn (q as T {arr, size, rp, wp}) =>
    let
      val _ = print (concat ["Before Deque: rp=",Int.toString (!rp)," wp=", Int.toString (!wp), "\n"])
      val res = deque (q)
      val _ = print (concat ["After Deque: rp=",Int.toString (!rp)," wp=", Int.toString (!wp), "\n"])
    in
      res
    end *)
end
