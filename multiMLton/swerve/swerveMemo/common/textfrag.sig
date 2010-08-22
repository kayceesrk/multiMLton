signature TEXT_FRAG =
sig
    type t

    (* empty *)
    val empty: t

    (* newline *)
    val nl: t

    (* carriage return + line feed *)
    val crlf: t

    (* space *)
    val sp: t

    (* lift a string to t *)
    val str: string -> t

    (* lift a list of strings *)
    val concat: string list -> t

    (* combine a list of t to t *)
    val seq: t list -> t

    (* sequence a list with a separator between each element *)
    val seqSep: t -> t list -> t

    val length: t -> int

    (* apply a function to each text frag *)
    val app: (string -> unit) -> t -> unit

    val toString: t -> string
end
