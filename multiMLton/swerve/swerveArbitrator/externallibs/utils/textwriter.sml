functor TextWriter(): TEXT_WRITER =
struct

val canClose = ref false	       
val logos = ref TextIO.stdOut
	    
fun openText fpath = (logos := TextIO.openOut fpath;
		      canClose := true)

fun write s = TextIO.output(!logos,s)
fun sswrite ss = TextIO.outputSubstr(!logos,ss)

fun nl() = write "\n"

fun writeLn s = (write s; nl())
fun sswriteLn ss = (sswrite ss; nl())

fun newline() = nl()

fun writeLst [] = ()
  | writeLst (s::ss) = (write s; writeLst ss)

fun getStream() = !logos

fun closeText() = if !canClose 
		  then (TextIO.flushOut (!logos);
			TextIO.closeOut (!logos);
			canClose := false)
		  else ()
end

