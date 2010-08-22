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

(*  Copyright (c) 2001 Anthony L Shipman *)


structure Common =
struct

(*-------------------------------------------------*)
(*  A hash table with string keys. *)

    structure STRT_key =
    struct
	type hash_key = string
	val hashVal = HashString.hashString
	fun sameKey (s1, s2) = (s1 = s2)
    end

    structure STRT = HashTableFn(STRT_key)

    exception NotFound

(*-------------------------------------------------*)

end


structure Environ:
    sig
	val get:    string -> string option
    end =
struct
    open Common
    open SMLofNJ.Weak

    type Table = string STRT.hash_table

    val cache: (Table option) ref weak ref = ref (weak (ref NONE))

    fun table() : Table =
    let
	(*  This takes a NAME=VALUE string *)
	fun fill tbl env =
	let
	    val ss = Substring.all env
	    val str = Substring.string
	    val fields = Substring.fields
			    (fn c => c = #"=") ss
	in
	    case fields of
	      [n, v] => STRT.insert tbl (str n, str v)
	    | [n]    => STRT.insert tbl (str n, "")
	    | _      => ()	(* unrecognisable *)
	end

	fun build() =
	let
	    val tbl = STRT.mkTable(101, NotFound)
	in
	    print "building\n";
	    app (fill tbl) (Posix.ProcEnv.environ());
	    cache := weak (ref (SOME tbl));
	    tbl
	end
    in
	case strong (!cache) of
	  NONE => build()	(* has been collected *)

	| SOME rtbl =>
	(
	    case !rtbl of
	      NONE     => build() (* is not yet built *)
	    | SOME tbl => tbl	  (* table is available *)
	)
    end

    fun get k = STRT.find (table()) k
end


structure Main =
struct
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    fun main(arg0, argv) =
    let
	fun data() = ignore(List.tabulate(100000, fn n => n))
    in
	SMLofNJ.Internals.GC.messages true;
	print(concat["The PATH is ",
		     valOf(Environ.get "PATH"), "\n"]);
	data();
	print(concat["The PATH is ",
		     valOf(Environ.get "PATH"), "\n"]);

        OS.Process.success
    end
    handle x =>
	(toErr(concat["Uncaught exception: ", exnMessage x, "\n"]);
    	    OS.Process.failure)

    val _ = SMLofNJ.exportFn("weak", main)
end


