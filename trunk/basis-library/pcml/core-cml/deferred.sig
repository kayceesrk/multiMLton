(* deferred.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature DEFERRED =
sig
    val deferSpawn: (unit->unit) -> unit
    val getSpawn: unit -> (unit -> unit) option
end
