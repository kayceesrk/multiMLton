signature IETF_LINE =
sig

    structure IP : IETF_PART 

    (*	Split a string into tokens and characters. *)
    val split: string -> IP.part list

    (* This is used to reconstitute a header field from
     * its parts. The result must preserve the semantics of the original
     * header. The supplied parts should contain the original white
     * space except for leading white space.  We will remove trailing
     * white space.
							    
     * Tokens may contain embedded space and specials if they came from
     * a quoted string.
							 
     * A token containing a " character cannot be represented in HTTP1.0
     * since it would have to be within a quoted string but one isn't
     * allowed within a quoted string!  HTTP1.1 allows \" in a quoted
     * string. We will drop " characters for HTTP1.0.
     * REVISIT - Fix this for HTTP1.1.
							 
     * Bad characters are stripped. *)
    val join: IP.part list -> string
			   			   
    (* If a string contains special characters then quote the field.
     * Control characters are not allowed.						     
     * We can use the lexer to recognise the different kinds of 
     * characters and just rejoin them with quoting. *)
    val quoteField: string -> string
			      			      
    (*	Dump for testing. *)
    val dump: IP.part list -> unit

end
