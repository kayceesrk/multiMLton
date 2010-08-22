signature ATTRIBUTE =
sig
    type attribute

    val strAttr: string -> string -> attribute
    val intAttr: string -> int -> attribute
    val emptyAttr: string -> attribute
    val attrToString: attribute -> {name:string,value:string}
    val outputAttributes: TextIO.outstream * attribute list -> unit

    val action:               string -> attribute
    val align:                string -> attribute
    val alink:                string -> attribute
    val alt:                  string -> attribute
    val altcode:              string -> attribute
    val archive:              string -> attribute
    val background:           string -> attribute
    val base:                 string -> attribute
    val bgcolor:              string -> attribute
    val border:               int    -> attribute
    val bordercolor:          string -> attribute
    val cellpadding:          int    -> attribute
    val cellspacing:          int    -> attribute
    val checked:                        attribute
    val clear:                string -> attribute
    val code:                 string -> attribute
    val codebase:             string -> attribute
    val color:                string -> attribute
    val cols:                 string -> attribute
    val colspan:              int    -> attribute
    val compact:                        attribute
    val content:              string -> attribute
    val coords:               string -> attribute
    val enctype:              string -> attribute
    val face:                 string -> attribute
    val frameborder:          int    -> attribute
    val height:               int    -> attribute
    val href:                 string -> attribute
    val hspace:               int    -> attribute
    val httpequiv:            string -> attribute
    val id:                   int    -> attribute
    val ismap:                          attribute
    val lang:                 string -> attribute
    val link:                 string -> attribute
    val marginheight:         int    -> attribute
    val marginwidth:          int    -> attribute
    val maxlength:            int    -> attribute
    val method:               string -> attribute
    val multiple:                       attribute
    val name:                 string -> attribute
    val nohref:                         attribute
    val noresize:                       attribute
    val noshade:                        attribute
    val nowrap:                         attribute
    val rel:                  string -> attribute
    val rev:                  string -> attribute
    val rows:                 string -> attribute
    val rowspan:              int    -> attribute
    val rules:                string -> attribute					
    val scrolling:            string -> attribute
    val selected:                       attribute
    val shape:                string -> attribute
    val size:                 string -> attribute
    val src:                  string -> attribute
    val start:                int    -> attribute
    val target:               string -> attribute
    val text:                 string -> attribute
    val class:                string -> attribute
    val style:                string -> attribute
    val thetype:              string -> attribute
    val title:                string -> attribute
    val usemap:               string -> attribute
    val valign:               string -> attribute
    val value:                string -> attribute
    val version:              string -> attribute
    val vlink:                string -> attribute
    val vspace:               int    -> attribute
    val width:                string -> attribute
end

signature HTML = 
sig	 
    structure A: ATTRIBUTE

    type html 		 
    type tag

    val empty : html			  
    val str: string -> html
    val seq: html list -> html
    val mkTag: string -> A.attribute list * html -> html

    val output: TextIO.outstream * html -> unit
		
    val address:    tag
    val anchor:     tag
    val applet:     tag
    val area:       tag
    val basefont:   tag
    val big:        tag
    val blockquote: tag
    val body:       tag
    val bold:       tag
    val br:         tag
    val caption:    tag
    val center:     tag
    val cite:       tag
    val ddef:       tag
    val define:     tag
    val dlist:      tag
    val dterm:      tag
    val emphasize:  tag
    val fieldset:   tag
    val font:	    tag
    val form:	    tag
    val frame:	    tag
    val frameset:   tag
    val h1:	    tag
    val h2:	    tag
    val h3:	    tag
    val h4:	    tag
    val h5:	    tag
    val h6:	    tag
    val head:	    tag
    val hr:	    tag
    val image:	    tag
    val input:	    tag
    val italics:    tag
    val keyboard:   tag
    val legend:	    tag
    val li:	    tag
    val meta:	    tag
    val noframes:   tag
    val olist:	    tag
    val option:	    tag
    val para:       tag
    val param:	    tag
    val pre:	    tag
    val sample:	    tag
    val select:	    tag
    val small:	    tag
    val span:       tag
    val strong:	    tag
    val style:	    tag
    val sub:	    tag
    val sup:	    tag
    val table:	    tag
    val td:	    tag
    val textarea:   tag
    val th:	    tag
    val base:       tag
    val code:       tag
    val hdiv:	    tag
    val html:       tag
    val link:       tag
    val hmap:	    tag
    val title:      tag
    val tr:	    tag
    val tt:	    tag
    val ulist:	    tag
    val underline:  tag
    val variable:   tag
end

