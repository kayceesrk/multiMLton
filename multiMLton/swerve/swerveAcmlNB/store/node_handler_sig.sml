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

(* $Id: node_handler_sig.sml,v 1.5 2001/08/15 19:03:16 felix Exp $ *)

(*  All node handlers are specialised with one of these structures.

@#34567890123456789012345678901234567890123456789012345678901234567890
*)

signature NODE_HANDLER =
sig

    (*	A value of this type is passed to the create function for the
	handler.
    *)
    type CreateArg

    (*	Create the thread for the handler. Optionally a new
	node configuration can be returned to update the original
	configuration.	All security-related initialisation must be
	done in here so that the master node will be blocked until
	it is ready.

	The caller should be prepared to handle exceptions from here if
	the creation fails.
    *)
    val init:	CreateArg -> Node.HndMbox * (Config.SwerveConfig option)


    (*	This tests if the handler will take the last segment
	of the URL path. For example a directory node wants
	the last segment as a file name.
    *)
    val	canTakeLast:	Config.NodeConfig -> bool


    (*	This tests if the handler will take all of the
	rest of the URL path if there are no child nodes.
    *)
    val	canTakeRest:	Config.NodeConfig -> bool

end

