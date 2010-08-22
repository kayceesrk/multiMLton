structure Attribute: ATTRIBUTE =
struct  

  structure TIO = TextIO

  datatype attribute = SAttr of string * string
		     | IAttr of string * int
		     | EAttr of string 			       

  fun strAttr s v = SAttr (s,v)
  fun intAttr s v = IAttr (s,v)
  fun emptyAttr v = EAttr v

  fun attrToString a = case a of
 			   SAttr (n,v) => {name=n,value=v}
			 | IAttr (n,v) => {name=n,value=Int.toString v}
			 | EAttr n => {name=n,value=""}
				      
  fun outputAV (os,{name,value}) = (TIO.output(os," ");
				    TIO.output(os,name);
				    if String.size value > 0
				    then(TIO.output(os,"=\"");
					 TIO.output(os,value);
					 TIO.output(os,"\""))
				    else ())

  fun outputAttributes (_,[]) = ()
    | outputAttributes (os,a::attrs) = (outputAV(os,attrToString a); outputAttributes(os,attrs))
				       
  fun action v = SAttr ("action",v)
  fun align v = SAttr ("align",v)
  fun alink v = SAttr ("alink",v)
  fun alt v = SAttr ("alt",v)
  fun altcode v = SAttr ("altcode",v)
  fun archive v = SAttr ("archive",v)
  fun background v = SAttr ("background",v)
  fun base v = SAttr ("base",v)
  fun bgcolor v = SAttr ("bgcolor",v)
  fun border v = IAttr ("border",v)
  fun bordercolor v = SAttr ("bordercolor",v)
  fun cellpadding v = IAttr ("cellpadding",v)
  fun cellspacing v = IAttr ("cellspacing",v)
  val checked = EAttr "checked"
  fun clear v = SAttr ("clear",v)
  fun code v = SAttr ("code",v)
  fun codebase v = SAttr ("codebase",v)
  fun color v = SAttr ("color",v)
  fun cols v = SAttr ("cols",v)
  fun colspan v = IAttr ("colspan",v)
  val compact = EAttr "compact"
  fun content v = SAttr ("content",v)
  fun coords v = SAttr ("coords",v)
  fun enctype v = SAttr ("enctype",v)
  fun face v = SAttr ("face",v)
  fun frameborder v = IAttr ("frameborder",v)
  fun height v = IAttr ("height",v)
  fun href v = SAttr ("href",v)
  fun hspace v = IAttr ("hspace",v)
  fun httpequiv v = SAttr ("httpequiv",v)
  fun id v = IAttr ("id",v)
  val ismap = EAttr "ismap"
  fun lang v = SAttr ("lang",v)
  fun link v = SAttr ("link",v)
  fun marginheight v = IAttr ("marginheight",v)
  fun marginwidth v = IAttr ("marginwidth",v)
  fun maxlength v = IAttr ("maxlength",v)
  fun method v = SAttr ("method",v)
  val multiple = EAttr "multiple"                      
  fun name v = SAttr ("name",v)
  val nohref = EAttr "nohref"
  val noresize = EAttr "noresize"
  val noshade = EAttr "noshade"
  val nowrap = EAttr "nowrap"
  fun rel v = SAttr ("rel",v)
  fun rev v = SAttr ("rev",v)
  fun rows v = SAttr ("rows",v)
  fun rowspan v = IAttr ("rowspan",v)
  fun rules v = SAttr ("rules",v)
  fun scrolling v = SAttr ("scrolling",v)
  val selected = EAttr "selected"
  fun shape v = SAttr ("shape",v)
  fun size v = SAttr ("size",v)
  fun src v = SAttr ("src",v)
  fun start v = IAttr ("start",v)
  fun target v = SAttr ("target",v)
  fun text v = SAttr ("text",v)
  fun class v = SAttr ("class",v)
  fun style v = SAttr ("style",v)
  fun thetype v = SAttr ("type",v)
  fun title v = SAttr ("title",v)
  fun usemap v = SAttr ("usemap",v)
  fun valign v = SAttr ("valign",v)
  fun value v = SAttr ("value",v)
  fun version v = SAttr ("version",v)
  fun vlink v = SAttr ("vlink",v)
  fun vspace v = IAttr ("vspace",v)
  fun width v = SAttr ("width",v)
end

structure Html: HTML =
struct

  structure A = Attribute
  structure TIO = TextIO

  datatype element = 
	   Empty
	 | SData of string (* plain text but escaped.  &copy; &amb;... *)
	 | Tag of {name: string,
		   attrs: A.attribute list,
		   content: html}
       and html = Element of element | Html of html list
		
  type tag = A.attribute list * html -> html

  val empty = Element Empty
			       
  fun str s =  Element (SData s)
  fun strEsc s = "FIX ME" (* loop and escape html meta chars *)
  fun seq hs = Html hs
  fun mkTag name (attrs, html) = Element (Tag {name    = name,
					       attrs   = attrs,
					       content = html})

  fun outputElement (os,elem) = case elem of
				    Empty => ()
				  | SData s => TIO.output(os,s)
				  | Tag {name,attrs,content} =>
				    (TIO.output(os,"<");
				     TIO.output(os,name);
				     A.outputAttributes(os,attrs);
				     TIO.output(os,">");
				     output(os,content);
				     TIO.output(os,"</");
				     TIO.output(os,name);
				     TIO.output(os,">"))

  and outputHtml (os,html) = List.app (fn h => output(os,h)) html
  and output (os,html) = case html of
			     Element elem => outputElement (os,elem)
			   | Html html => outputHtml (os,html)

  val address             =  mkTag "ADDRESS"
  val anchor              =  mkTag "A"
  val applet              =  mkTag "APPLET"
  val area                =  mkTag "AREA"
  val basefont            =  mkTag "BASEFONT"
  val big                 =  mkTag "BIG"
  val blockquote          =  mkTag "BLOCKQUOTE"
  val body                =  mkTag "BODY"
  val bold                =  mkTag "B"
  val br                  =  mkTag "BR"
  val caption             =  mkTag "CAPTION"
  val center              =  mkTag "CENTER"
  val cite                =  mkTag "CITE"
  val ddef                =  mkTag "DD"
  val define              =  mkTag "DFN"
  val dlist               =  mkTag "DL"
  val dterm               =  mkTag "DT"
  val emphasize           =  mkTag "EM"
  val fieldset            =  mkTag "FIELDSET"
  val font                =  mkTag "FONT"
  val form                =  mkTag "FORM"
  val frame               =  mkTag "FRAME"
  val frameset            =  mkTag "FRAMESET"
  val h1                  =  mkTag "H1"
  val h2                  =  mkTag "H2"
  val h3                  =  mkTag "H3"
  val h4                  =  mkTag "H4"
  val h5                  =  mkTag "H5"
  val h6                  =  mkTag "H6"
  val head                =  mkTag "HEAD"
  val hr                  =  mkTag "HR"
  val image               =  mkTag "IMG"
  val input               =  mkTag "INPUT"
  val italics             =  mkTag "I"
  val keyboard            =  mkTag "KBD"
  val legend              =  mkTag "LEGEND"
  val li                  =  mkTag "LI"
  val meta                =  mkTag "META"
  val noframes            =  mkTag "NOFRAMES"
  val olist               =  mkTag "OL"
  val option              =  mkTag "OPTION"
  val para                =  mkTag "P"
  val param               =  mkTag "PARAM"
  val pre                 =  mkTag "PRE"
  val sample              =  mkTag "SAMP"
  val select              =  mkTag "SELECT"
  val small               =  mkTag "SMALL"
  val strong              =  mkTag "STRONG"
  val style               =  mkTag "STYLE"
  val sub                 =  mkTag "SUB"
  val sup                 =  mkTag "SUP"
  val table               =  mkTag "TABLE"
  val td                  =  mkTag "TD"
  val textarea            =  mkTag "TEXTAREA"
  val th                  =  mkTag "TH"
  val base                =  mkTag "BASE"
  val code                =  mkTag "CODE"
  val hdiv                =  mkTag "DIV"
  val html                =  mkTag "HTML"
  val link                =  mkTag "LINK"
  val hmap                =  mkTag "MAP"
  val span                =  mkTag "SPAN"
  val title               =  mkTag "TITLE"
  val tr                  =  mkTag "TR"
  val tt                  =  mkTag "TT"
  val ulist               =  mkTag "UL"
  val underline           =  mkTag "U"
  val variable            =  mkTag "VAR"
end			     

(*
structure Test = 
struct

open Attribute
open Html

val h = html ([],body ([], seq [para ([], str "ray"),
				para ([], str "here is "),
				bold ([id 3,color "blue"], str "bold"),
				para ([], str " text")]))

val _ = output(TextIO.stdOut,h)
end
*)
