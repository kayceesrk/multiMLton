(* circular-queue.sml
 * @authors KC Sivaramakrishnan (chandras@cs.purdue.edu)
 *)

structure CirQueue : CIR_QUEUE =
struct

  datatype 'a t = T of {arr : 'a option array ref, size: int ref, rp: int ref, wp: int ref}

  exception CirQueueEmpty

  local
    fun grow ({arrv, rpv, wpv, sizev}) =
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
      {arrv = newArr, rpv = 0, wpv = oldSize, sizev = newSize}
    end
  in

    fun new (startSize) =
    let
      val arr = Array.tabulate (startSize, fn _ => NONE)
    in
      T {arr = ref arr, size = ref startSize, rp = ref ~1, wp = ref 0}
    end

    fun new' ({arr, rp, wp, size}) =
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
        val arg = {arrv = arrv, rpv = rpv, wpv = wpv, sizev = sizev}
        val {arrv, rpv, wpv, sizev} = if (rpv = wpv) then grow arg else arg
        (* update the references *)
        val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
      in
        ()
      end

    fun deque (q as T {arr, wp, rp, size}) =
      let
        (* read the references *)
        val (arrv, rpv, wpv, sizev) = (!arr, !rp, !wp, !size)
        (* If the buffer is empty, raise exception *)
        val _ = if (rpv = ~1) then raise CirQueueEmpty else ()
        (* fetch the element *)
        fun getElement rpv =
          case Array.sub (arrv, rpv) of
               NONE => getElement ((rpv + 1) mod sizev)
             | SOME v => SOME v
        val e = getElement rpv
        (* Erase the element from the array *)
        val _ = Array.update (arrv, rpv, NONE)
        (* update read pointer *)
        val rpv = (rpv + 1) mod sizev
        (* If the buffer is empty, update the read and write pointer *)
        val (rpv, wpv) = if (rpv = wpv) then (~1, 0) else (rpv, wpv)
        (* update the references *)
        val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
      in
        e
      end

    fun cleanPrefix (q as T {arr, size, rp, wp}, keep) =
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
                  if keep v then
                    (ptr, wpv)
                  else
                    (* the element needs to be cleaned *)
                    (Array.update (arrv, ptr, NONE);
                    walk ((ptr+1) mod sizev))))


      (* Walk the prefix and clean *)
      val (rpv, wpv) = walk rpv

      (* TODO SHRINK *)

      (* update the references *)
      val _ = (arr := arrv, rp := rpv, wp := wpv, size := sizev)
    in
      ()
    end

    fun cleanSuffix (q as T {arr, size, rp, wp}, keep) =
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
                 if keep v then
                   (rpv, (ptr + 1) mod sizev)
                 else
                   (* the element needs to be cleaned *)
                   (Array.update (arrv, ptr, NONE);
                    walk ((ptr - 1) mod sizev))))

      (* Walk the suffix and clean *)
      val (rpv, wpv) = walk ((wpv - 1) mod sizev)

      (* TODO SHRINK *)

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
end
