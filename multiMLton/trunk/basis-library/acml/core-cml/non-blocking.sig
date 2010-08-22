signature NON_BLOCKING =
sig
    type proc

    (* creates an abstract processor *)
    val createProcessor : unit -> proc option

    val executeOn : proc -> (unit -> 'a) -> 'a
    val execute: (unit -> 'a) -> 'a
end

signature NON_BLOCKING_EXTRA =
sig
    include NON_BLOCKING
    val mkNBThread : unit -> unit
end
