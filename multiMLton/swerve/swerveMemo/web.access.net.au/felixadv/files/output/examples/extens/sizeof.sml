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


structure Main=
struct
    structure O = Unsafe.Object
    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    (*	Estimate the size of v in 32-bit words.
	Boxed objects have an extra descriptor word
	which also contains the length for vectors
	and arrays.
    *)
    fun sizeof v =
    let
	fun obj_size obj =
	(
	    case O.rep obj of
	      O.Unboxed	=> 1	(* inline 31 bits *)
	    | O.Real	=> 1+2

	    | O.Pair      => tup_size obj
	    | O.Record    => tup_size obj
	    | O.RealArray => tup_size obj

	    | O.PolyArray => arr_size obj

	    (* includes Word8Vector.vector
	       and CharVector.vector
	    *)
	    | O.ByteVector => 1 +
		((size(O.toString obj)+3) div 4)

	    (* includes Word8Array.array
	       and CharArray.array
	    *)
	    | O.ByteArray =>  1 +
		((Array.length(O.toArray obj)+3) div 4)

	    | _ => 2	(* punt for other objects *)
	)

	(*  Count the record plus the size of
	    pointed-to objects in the heap.
	*)
	and tup_size obj =
	let
	    fun sz obj =
		if O.boxed obj
		then
		    1 + (obj_size obj)
		else
		    1
	in
	    Vector.foldl
		(fn (obj, s) => s + (sz obj))
		1
		(O.toTuple obj)
	end

	and arr_size obj =
	let
	    fun sz obj =
		if O.boxed obj
		then
		    1 + (obj_size obj)
		else
		    1
	in
	    Array.foldl
		(fn (obj, s) => s + (sz obj))
		1
		(O.toArray obj)
	end
    in
	obj_size(O.toObject v)
    end



    fun main(arg0, argv) =
    let
	fun show name v = print(concat[
		"Size of ", name,
		" = ", Int.toString(sizeof v),
		" 32-bit words\n"])
    in
	show "integer"	3;
	show "real"	3.3;
	show "string"	"abc";

	show "pair"	("abc", 42);
	show "record"	{a = 1, b = 4.5, c = "fred"};

        OS.Process.success
    end
    handle x => (
	toErr(concat["Uncaught exception: ",
	             exnMessage x, "\n"]);
	OS.Process.failure)


    val _ = SMLofNJ.exportFn("sizeof", main)
end

