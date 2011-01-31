(* fun-queue.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure CirQueue : CIR_QUEUE =
struct

  datatype 'a t = T of {arr : 'a option array, size: int ref, rp: int ref, wp: int ref}

  exception CirQueueFull
  exception CirQueueEmpty

  val startSize = 1024

  fun new () =
  let
    val arr = Array.tabulate (startSize, fn _ => NONE)
  in
    T {arr = arr, size = ref startSize, rp = ref 0, wp = ref 0}
  end

  fun isFull (T {rp, wp, size, ...}) =
    ((!wp + 1) mod !size) = !rp

  fun isEmpty (T {rp, wp, size, ...}) =
    (!rp) = (!wp)

  fun enque (q as T {arr, size, rp, wp}, e) =
    if (isFull (q)) then
      raise CirQueueFull
    else
      let
        val _ = Array.update (arr, !wp, e)
        val _ = wp := ((!wp + 1) mod (!size))
      in
        ()
      end

  fun deque (q as T {arr, size, rp, wp}) =
    if (isEmpty (q)) then
      NONE
    else
      let
        val e = Array.sub (arr, !rp)
        val _ = rp := ((!rp + 1) mod !size)
      in
        e
      end
end
