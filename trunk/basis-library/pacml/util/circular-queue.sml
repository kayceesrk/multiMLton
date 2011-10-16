(* circular-queue.sml
 * @authors KC Sivaramakrishnan (chandras@cs.purdue.edu)
 *)

structure CirQueue : CIR_QUEUE =
struct

  datatype 'a t = T of {arr : 'a option array ref, size: int ref, rp: int ref, wp: int ref}

  exception CirQueueEmpty

  local
    (* NOTE: startSize must not be less than 2 *)
    val startSize = 2
    fun grow (arrv, rpv, wpv, sizev) =
    let
      val oldSize = sizev
      val newSize = oldSize * 2
      val newArr = Array.tabulate (oldSize * 2, fn i =>
                    if (i < oldSize) then
                      let
                        val index = (rpv + i) mod oldSize
                      in
                        Array.sub (arrv, index)
                      end
                    else
                      NONE)
      (* val _ = print ("CirQueue.grow to size "^(Int.toString newSize)^"\n") *)
    in
      (newArr, 0, oldSize, newSize)
    end

   fun numElements (rpv, wpv, sizev) =
     if (rpv = ~1) then 0
     else
      (wpv - rpv + sizev) mod sizev

   fun shrink (arrv, rpv, wpv, sizev) =
    let
      val live = numElements (rpv, wpv, sizev)
    in
      (* If the live size is 0, resort to initial parameters *)
      if (live = 0 andalso sizev = startSize) then
        (arrv, rpv, wpv, sizev)
      else if (live = 0) then
        (Array.tabulate (startSize, fn _ => NONE), ~1, 0, startSize)
      (* shrink if the live size is < 25% of the buffer size *)
      else if (Int.div (sizev, live) >= 4 andalso (not (sizev = startSize))) then
        let
          fun getNewSize s =
            if (s < live orelse s < startSize) then s*2
            else getNewSize (Int.div (s, 2))
          val newSize = getNewSize (sizev)
          val newArray =
            Array.tabulate (newSize, fn i =>
              if (i < live) then
                let
                  val index = (rpv + i) mod sizev
                in
                  Array.sub (arrv, index)
                end
              else
                NONE)
        in
          (newArray, 0, live, newSize)
        end
      else
        (arrv, rpv, wpv, sizev)
    end

  in

    fun new () =
    let
      val arr = Array.tabulate (startSize, fn _ => NONE)
    in
      T {arr = ref arr, size = ref startSize, rp = ref ~1, wp = ref 0}
    end

    fun newExplicit ({arr, rp, wp, size}) =
      T {arr = ref arr, rp = ref rp, wp = ref wp, size = ref size}

    fun isEmpty (q as T{rp, ...}) =
      !rp = ~1

    fun enque (q as T {arr, size, rp, wp}, e) =
      let
        (* read the references *)
        val (arrv, rpv, wpv, sizev) = (!arr, !rp, !wp, !size)
        val _ = case e of NONE => raise Fail "Trying to insert none-sence" | _ => ()
        (* insert the element *)
        val _ = Array.update (arrv, wpv, e)
        (* update the write pointer *)
        val wpv = (wpv + 1) mod (sizev)
        (* If we are enqueuing to an empty buffer, move the read pointer *)
        val rpv = if (rpv = ~1) then 0 else rpv
        (* If the buffer is full, grow *)
        val arg = (arrv, rpv, wpv, sizev)
        val (arrv, rpv, wpv, sizev) = if (rpv = wpv) then grow arg else arg
        (* update the references *)
        val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
      in
        ()
      end

    (* Enque in front.
     * NOTE: We assume that there is space in the queue for the element. *)
    fun undeque (q as T {arr, size, rp, wp}, e) =
      let
        (* read the references *)
        val (arrv, rpv, wpv, sizev) = (!arr, !rp, !wp, !size)
        val _ = case e of NONE => raise Fail "Trying to insert none-sence" | _ => ()
        val (arrv, rpv, wpv, sizev) =
          if (rpv = ~1) then
            let
              val _ = Array.update (arrv, 0, e)
            in
              (arrv, 0, 1, sizev)
            end
          else
            let
              val rpv = (rpv - 1) mod (sizev)
              (* insert the element *)
              val _ = Array.update (arrv, rpv, e)
            in
              (arrv, rpv, wpv, sizev)
            end
        (* update the references *)
        val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
      in
        ()
      end


    fun deque (q as T {arr, wp, rp, size}) =
      if (isEmpty (q)) then
        NONE
      else
        (let
          (* read the references *)
          val (arrv, rpv, wpv, sizev) = (!arr, !rp, !wp, !size)
          (* fetch the element *)
          fun getElement rpv =
            case Array.sub (arrv, rpv) of
                NONE => getElement ((rpv + 1) mod sizev)
              | SOME v => (SOME v, rpv)
          val (e, rpv) = getElement rpv
          (* Erase the element from the array *)
          val _ = Array.update (arrv, rpv, NONE)
          (* update read pointer *)
          val rpv = (rpv + 1) mod sizev
          (* If the buffer is empty, update the read and write pointer *)
          val (rpv, wpv) = if (rpv = wpv) then (~1, 0) else (rpv, wpv)
          (* shrink *)
          val (arrv, rpv, wpv, sizev) = shrink (arrv, rpv, wpv, sizev)
          (* update the references *)
          val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
        in
          e
        end)

    fun cleanPrefix (q as T {arr, size, rp, wp}, clean) =
    let
      (* read the references *)
      val (arrv, rpv, wpv, sizev) = (!arr, !rp, !wp, !size)
      fun walk ptr =
        (* If the buffer is empty *)
        if ptr = ~1 then (~1, 0)
        (* If the buffer becomes empty as we clean *)
        else if ptr = wpv then (~1, 0)
        else
          (case Array.sub (arrv, ptr) of
                NONE => walk ((ptr+1) mod sizev)
              | SOME v =>
                ((* the element needs to be kept *)
                  if (not (clean v)) then
                    (ptr, wpv)
                  else
                    (* the element needs to be cleaned *)
                    (Array.update (arrv, ptr, NONE);
                    walk ((ptr+1) mod sizev))))


      (* Walk the prefix and clean *)
      val (rpv, wpv) = walk rpv

      (* shrink *)
      val (arrv, rpv, wpv, sizev) = shrink (arrv, rpv, wpv, sizev)

      (* update the references *)
      val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
    in
      ()
    end

    fun cleanSuffix (q as T {arr, size, rp, wp}, clean) =
    let
      (* read the references *)
      val (arrv, rpv, wpv, sizev) = (!arr, !rp, !wp, !size)
      fun walk ptr =
        (* If the buffer is empty *)
        if rpv = ~1 then (~1, 0)
        (* If the buffer becomes empty as we clean *)
        else if ((rpv > ptr) andalso (rpv - ptr = 1)) then (~1, 0)
        else
          (case Array.sub (arrv, ptr) of
                NONE => walk ((ptr - 1) mod sizev)
              | SOME v =>
                ((* the element needs to be kept *)
                 if (not (clean v)) then
                   (rpv, (ptr + 1) mod sizev)
                 else
                   (* the element needs to be cleaned *)
                   (Array.update (arrv, ptr, NONE);
                    walk ((ptr - 1) mod sizev))))

      (* Walk the suffix and clean *)
      val (rpv, wpv) = walk ((wpv - 1) mod sizev)

      (* shrink *)
      val (arrv, rpv, wpv, sizev) = shrink (arrv, rpv, wpv, sizev)

      (* update the references *)
      val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
    in
      ()
    end

   fun printLayout ( T{arr, size, rp, wp}, f) =
   let
     val _ = print "["
     val _ = List.tabulate (!size, fn i => (case Array.sub (!arr, i) of
                                               NONE => print "N"
                                             | SOME v => print (f v);
                                           if (not (i = !size - 1)) then print ", " else ()))
     val _ = print "] "
     val _ = print ("rp = "^(Int.toString (!rp))^" ")
     val _ = print ("wp = "^(Int.toString (!wp))^" ")
     val _ = print ("size = "^(Int.toString (!size))^"\n")
   in
     ()
   end
   (*val enque =
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
end
