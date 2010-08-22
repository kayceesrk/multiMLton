signature GUID = 
sig	    

    type guid

    val next: unit -> guid	      
    val peek: unit -> guid
    val eq: guid * guid -> bool

    val toString: guid -> string
    val sPeek: unit -> string

    val layout: guid -> Layout.t
end	
