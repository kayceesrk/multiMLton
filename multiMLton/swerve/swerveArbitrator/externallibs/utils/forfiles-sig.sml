signature FOR_FILES =
sig
    
    type path 
    type regex
    type dirstream
    type filepath
    type line
	 
    type outname_generator = filepath -> filepath
    type line_transformer = unit SMLofNJ.Cont.cont -> line -> line option
		
    val mkListDirStream: filepath list -> dirstream	    
    val mkDirSelectionStream: path * regex -> dirstream
    val xlateEachFileInDirStream: dirstream -> outname_generator -> line_transformer -> unit
    val simpleCopy: unit -> unit
										      
end
