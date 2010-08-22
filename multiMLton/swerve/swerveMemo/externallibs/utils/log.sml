
signature LOG =
sig
    type logger
    type msg = string
				      
    datatype level = DEBUG1 | DEBUG2 | DEBUG3 | INFO | WARN | ERROR | FATAL

    type handler = logger * level * msg -> unit
								      
    val mkLogger: string -> handler -> level -> logger
    val levelEnabled: logger -> level -> bool
    val log: logger -> level -> string -> unit
    val levelToString: level -> string
    val asStrings: logger -> {name:string, level: string}
end
    
structure Log:> LOG = 
struct
  
  type msg = string

  datatype level = DEBUG1 | DEBUG2 | DEBUG3 | INFO | WARN | ERROR | FATAL
								    
  datatype logger = Logger of {name: string, handler: handler, level: level, rank: int}
  withtype handler = logger * level * msg -> unit
					
  fun levelToString level = 
      case level of
	  DEBUG1 => "Debug1"
	| DEBUG2 => "Debug2"
	| DEBUG3 => "Debug3"
	| INFO => "Info"
	| WARN => "Warn"
	| ERROR => "Error"
	| FATAL => "Fatal"
		   
  fun levelToRank level = 
      case level of
	  DEBUG1 => 1
	| DEBUG2 => 2
	| DEBUG3 => 3
	| INFO => 4
	| WARN => 5
	| ERROR => 6
	| FATAL => 7	     		 
		   
  fun levelOk(msgRank,logRank) = msgRank >= logRank
				 
  fun mkLogger name handler level = Logger {name = name, handler = handler,level = level,rank = levelToRank level}
				    
  fun levelEnabled (Logger {rank,...}) msglevel = levelOk (levelToRank msglevel,rank)
						  
  fun log (logger as Logger {name, handler, rank,...}) msglevel msg = 
      if levelOk (levelToRank msglevel,rank)
      then handler(logger,msglevel,msg)
      else ()
	   
  fun asStrings (Logger {name, level,...}) = {name = name, level = levelToString level}
					     
end


signature FORMAT =
sig
    type format
end

structure Format: FORMAT =
struct

  structure L = Log 
  type format = TextIO.outstream * L.logger * L.level * L.msg -> unit

end

signature LOG_HANDLER =
sig
    
    structure L: LOG
    structure F: FORMAT
	 
    val simpleFormat: F.format

    val mkFileHandler: string * F.format -> L.handler
    val mkConsoleHandler: F.format -> L.handler
    val mkSplitHandler: L.handler * L.handler -> L.handler
    val mkNSplitHandler: L.handler list -> L.handler
				  
end 
        
structure LogHandler:> LOG_HANDLER where type L.logger = Log.logger 
                                   and   type F.format = Format.format 
				   and   type L.level = Log.level =
				  
struct

  structure L = Log 
  structure F = Format

  fun simpleFormat (os,logger,mlevel,msg) = let val {name,level} = Log.asStrings logger
						fun put s = TextIO.output (os,s)
						val tstamp = Time.toString(Time.now())
(*						val date  = Date.fromTimeLocal(Time.now())
						val fdate = Date.fmt "%Y %b %d %H:%M:%S" date *)
					    in
(*						List.app put [fdate,"::",name,"-",L.levelToString mlevel,": ",msg,"\n"] *)
						List.app put [tstamp," ",name," ",L.levelToString mlevel,": ",msg,"\n"]
					    end

  fun mkConsoleHandler format = fn (logger,level,msg) => format (TextIO.stdOut,logger,level,msg)
			       
  fun mkFileHandler (fname,format) = mkConsoleHandler format

  fun mkSplitHandler (h1,h2) = fn logmsg => (h1 logmsg; h2 logmsg)

  fun mkNSplitHandler hs = fn logmsg => List.app (fn h => h logmsg) hs
			    
end 
  

structure LoggerManager =
struct

  fun ctorLogger (name,level) = let val hndlr = LogHandler.mkConsoleHandler LogHandler.simpleFormat
				    val logger = Log.mkLogger name hndlr level
				in
				    logger
				end
end
    

(* val _  = let val clog = LogHandler.mkConsoleHandler LogHandler.simpleFormat
	      val logger = Log.mkLogger "mylog" clog Log.INFO
	  in
	      Log.log logger Log.INFO "hi";
	      Log.log logger Log.DEBUG1 "ho";
	      Log.log logger Log.ERROR "away"
	  end  *)
