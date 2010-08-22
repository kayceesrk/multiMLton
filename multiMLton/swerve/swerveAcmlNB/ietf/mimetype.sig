signature MIME_TYPE = 
sig
    structure TF: TEXT_FRAG

    type t 
    type param = string * string

    val simpleType: string -> string -> t

    val mkMimeType: string -> string -> param list -> t

    val fromString: string -> t

    val isUnknown: t -> bool

    val format: t -> TF.t
end
