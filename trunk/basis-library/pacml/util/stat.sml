(* assert.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure Statistics =
struct
  val statFlag = false

  fun doit (f : unit -> unit) =
    if statFlag then
      f ()
    else ()
end
