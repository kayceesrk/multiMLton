signature BASE64 =
sig

    type base64 

    val decode:	base64 -> string option
    val encode:	string -> base64

    val toString: base64 -> string
end
