signature TEXT_WRITER =
sig

    val openText: string -> unit
    val closeText: unit -> unit
    val nl: unit -> unit
    val write: string -> unit
    val writeLn: string -> unit
    val writeLst: string list -> unit
    val sswrite: substring -> unit
    val sswriteLn: substring -> unit
    val getStream: unit -> TextIO.outstream

end
