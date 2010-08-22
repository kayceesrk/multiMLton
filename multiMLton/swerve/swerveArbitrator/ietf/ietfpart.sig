signature IETF_PART =
sig
    datatype part =
	     Token of string	(* including quoted strings *)
	   | Spec of char
	   | WS of string	(* the white space *)
	   | Bad  of char	(* invalid character *)
	   | EOF
end
