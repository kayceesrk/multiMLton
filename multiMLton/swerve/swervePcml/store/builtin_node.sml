(*
Original Code - Copyright (c) 2001 Anthony L Shipman
MLton Port Modifications - Copyright (c) Ray Racine

Permission is granted to anyone to use this version of the software
for any purpose, including commercial applications, and to alter it and
redistribute it freely, subject to the following restrictions:

    1. Redistributions in source code must retain the above copyright
    notice, this list of conditions, and the following disclaimer.

    2. The origin of this software must not be misrepresented; you must
    not claim that you wrote the original software. If you use this
    software in a product, an acknowledgment in the product documentation
    would be appreciated but is not required.

    3. If any files are modified, you must cause the modified files to
    carry prominent notices stating that you changed the files and the
    date of any change.

Disclaimer

    THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
    INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
    IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

Modification History
====================
Ray Racine 6/3/2005 - MLton Port and idiomatic fixups.
*)


(*  Copyright (c) 2001 Anthony L Shipman *)

(* Simple built-ins have no state. They just generate a canned response.
 * The kind of reponse is determined by the configuration. *)

structure SimpleBuiltinHandler: NODE_HANDLER =
struct
  
  structure M      = MLton.PCML.Mailbox
  structure Cfg    = Config
  structure Req    = HTTPMsg
  structure Status = HTTPStatus
  structure U      = ResponseUtils
  structure TF     = TextFrag
  structure Cmn    = Common

  open Node
       
  type CreateArg  = string
		    
  fun canTakeLast _ = false
  fun canTakeRest _ = false
		      		      
  (* We check the name is legal here. REVISIT It
   * should be checked when the main config is read. *)
  fun init name =
      let val mbox  = M.mailbox()
      in
	  case get_maker name of
	      NONE   => Log.error ["Invalid built-in handler name: ", name]
	    | SOME _ => ();
	  
	  MLton.PCML.spawn (server mbox);
	  
	  (mbox, NONE)
      end

    and server mbox () =
    let
	fun loop() =
	(
	    case M.recv mbox of
	      msg as HndReq _ => handle_request msg 

	    ; loop ()
	)
    in
	loop ()
    end



    and handle_request 
	    (msg as HndReq {config, rchan, request, ...})
	    =
    let
	val Cfg.NodeConfig {kind, ...} = config

	fun reply response =
	(
	    MLton.PCML.send(rchan, HndResponse(msg, response))
	)
    in
	case kind of
	  Cfg.NodeIsBuiltin {name} =>
	(
	    case get_maker name of
	      NONE   => reply (U.mkServerFail())
	    | SOME f => reply (f request)
	)

	| _ => raise Cmn.InternalError "SimpleBuiltin,handleRequest"
    end



    and get_maker name =
    (
	case name of
	  "hw"	    => SOME (fn _ => U.mkHelloWorld())
	| "reject"  => SOME (fn _ => U.mkNotFound())
	| "sleep"   => SOME sleep
	| _         => NONE
    )




    (*	This sleeps for a number of seconds given by the time-out
	value in the query.  It is useful for holding a connection
	open while testing. At the moment we can't run more than one of
	these concurrently.

	Don't forget to abort the sleep if the connection goes down.
    *)
    and sleep request =
    let	val Req.Request  {url, abort, ...} = request
	val URL.HTTP_URL {query, ...} = url
				       
	val timeout =  case query of
			   NONE => 1
			 | SOME q => getOpt(Int.fromString q, 1)
				     
	val t_evt = MLton.PCML.timeOutEvt(Time.fromSeconds(Int.toLarge timeout))
    in
	MLton.PCML.select [ MLton.PCML.wrap ( t_evt, fn _ => () ),
		     MLton.PCML.wrap ( Abort.evt abort, fn _ => () ) ];
	
	U.mkHTML Status.OK ( TF.concat [ "<html><body><p>", 
					 "Slept for ", Int.toString timeout, " seconds",
					 "</body></html>" ] )
    end


end
