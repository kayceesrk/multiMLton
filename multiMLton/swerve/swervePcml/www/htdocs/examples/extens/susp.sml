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


structure Table:
    sig
	val set:    string * string -> unit
	val get:    string -> string option
    end =
struct
    open Common

    type Table = string STRT.hash_table

    val susp = SMLofNJ.Susp.delay(fn () =>
    		STRT.mkTable(101, NotFound): Table)

    fun table() = SMLofNJ.Susp.force susp

    fun set (k, v) = STRT.insert (table()) (k, v)
    fun get k      = STRT.find (table()) k
end


structure Main =
struct

    fun main(arg0, argv) =
    let
    in
	Table.set("fox", "brown");
	print(concat["The fox is ", valOf(Table.get "fox"), "\n"]);

        OS.Process.success
    end

    val _ = SMLofNJ.exportFn("susp", main)
end



