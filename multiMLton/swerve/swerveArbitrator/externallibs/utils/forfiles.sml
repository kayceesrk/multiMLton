structure ForFiles: FOR_FILES = 

struct

structure FP = OS.Path
structure FS = OS.FileSys
structure BT = RegExpFn(structure P=AwkSyntax structure E=BackTrackEngine)

type path = string
type filename = string
type filepath = {dir: path, file: filename}

type line = string
type regex = string

type dirstream = unit -> filepath option
type outname_generator = filepath -> filepath
type line_transformer = unit SMLofNJ.Cont.cont -> line -> line option
	       		     
fun openSrcFile fpath = 
    (TextIO.openIn (FP.joinDirFile fpath) 
     handle Exception => (print ("Open failed " ^ (FP.joinDirFile fpath)); raise Exception))
    
fun closeSrcFile ins = TextIO.closeIn ins


fun mkListDirStream files = let val files = ref files
				fun next [] = NONE
				  | next (f::fs) = (files := fs; SOME f)
			    in
			     fn () => next (!files)
			    end
		       
fun xlateEachFileInDirStream dirstrm outfnamegen linetransformer = 
    let fun foreachFile () = 
	    let val ifp = dirstrm ()
	    in
		case ifp of 
		    NONE => ()
		  | SOME ifp => let val ofp = outfnamegen ifp
				    val ios = openSrcFile ifp
				    val oos = TextIO.openOut (FP.joinDirFile ofp)
				    fun closeAll () = (TextIO.closeIn ios; TextIO.closeOut oos)
				    fun foreachLine kesc = let val line = TextIO.inputLine ios
							   in
							       case line of 
								   NONE => ()
								 | SOME l => (case linetransformer kesc  l of
										  NONE => foreachLine kesc
										| SOME l => (TextIO.output (oos, l); foreachLine kesc))
							   end
				in
				    SMLofNJ.Cont.callcc foreachLine;
				    closeAll();
				    foreachFile()
				end
	    end
    in
	foreachFile()
    end
    
fun mkDirSelectionStream (srcpath, srcregex) =
    let val dir = FS.openDir srcpath
	val rgx = BT.compileString srcregex
	fun nextMatch () = 
	    let val dirfile = FS.readDir dir
	    in
		case dirfile of
		    NONE => (FS.closeDir dir; NONE)
		  | SOME afile => let val m = StringCvt.scanString (BT.find rgx) afile
				  in
				      case m of 
					  NONE => nextMatch()
					| SOME _ => SOME {dir=srcpath, file=afile}
				  end
	    end
    in
	nextMatch
    end
    
fun simpleCopy () =
    let val testdir = "/home/ray/test"
	val regex = ".+[.]txt$"
	fun genOutName (fpath:filepath) = 
	    let val {dir,file}= fpath
		val fname = #file fpath
		val {base,ext} = FP.splitBaseExt fname
	    in
		{dir=dir, file=base ^ ".bu"}
	    end
	fun xlateLine kesc l = SOME l
    in
	xlateEachFileInDirStream (mkDirSelectionStream (testdir,regex))
				 genOutName
				 xlateLine
    end
    
end
