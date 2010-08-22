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


(* This is a pattern for singleton objects that are implemented
 * as threads that receive a message stream from a channel or mailbox.

 * For example when using a channel

 * type CtrMsg = ...
 * fun server ...

 * structure Counter = Singleton
       ( type input    = CtrMsg CML.chan
         val  newInput = CML.channel
         val  object   = server )

 * If the object terminates it won't be restarted. All attempts to
 * communicate with it will hang. *)

signature SINGLETON =
sig
    type input

    (* This function returns the port into the singleton object. *)
    val get: unit -> input
end


functor Singleton ( type input
                    val  newInput: unit -> input
                    val  object: input -> unit -> unit )
	: SINGLETON =
struct
  structure SV = MLton.PCML.SyncVar
  structure L = MLton.PCML.MutexLock

  type input = input

  val input: input option ref = ref NONE

  val lock = L.initLock ()

  structure T = MLton.Thread

  fun get () =
    case !input of
         NONE =>
            let
              val _ = T.atomicBegin ()
              val _ = L.getLock lock
              val res = case !input of
                            SOME i => i
                          | NONE =>
                              let
                                val i = newInput ()
                                val _ = Debug.debug' "Starting logger thread\n"
                                val _ = input := SOME i
                                val _ = MLton.PCML.spawn (object i)
                              in
                                i
                              end
              val _ = L.releaseLock lock
              val _ = T.atomicEnd ()
            in
              res
            end
      | SOME i => i
end
