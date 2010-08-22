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

    fun toErr s = TextIO.output(TextIO.stdErr, s)

    val strings = [ "fred",
    		    "wilma",
		    "barney",
		    "betty",
    		    "wilma",
		    "pebbles",
		    "bambam"
		]


    fun sort_strings (data: string list) =
    (
	ListMergeSort.sort (op >) data
    )


    fun unique_strings (data: string list) =
    (
	ListMergeSort.uniqueSort String.compare data
    )

    fun show_list lst =
    (
	app (fn s => (print " "; print s)) lst;
	print "\n"
    )


    local
	datatype Gender = Male | Female

	type Pair = string * Gender

	(*  Compare two pairs. *)
	fun compare ((n1, _), (n2, _)) = String.compare(n1, n2)

	structure PairArray = MonoArrayFn(type elem = Pair)
	structure Searcher  = BSearchFn(PairArray)
	structure Sorter    = ArrayQSortFn(PairArray)

	val gender = [  ("fred",	Male),
			("wilma",	Female),
			("barney",	Male),
			("betty",	Female),
			("wilma",	Female),
			("pebbles",	Female),
			("bambam",	Male)
		    ]

	val sorted_gender = PairArray.fromList gender
	val _ = Sorter.sort compare sorted_gender
    in
	fun find_gender name =
	let
	    (* Compare a key with a pair *)
	    fun cmp (key, (n, _)) = String.compare(key, n)
	in
	    case Searcher.bsearch cmp (name, sorted_gender) of
	      NONE             => NONE
	    | SOME (_, (_, g)) => SOME g
	end


	fun show_gender Male   = "male"
	|   show_gender Female = "female"
    end



    fun main(arg0, argv) =
    (
	show_list(sort_strings strings);
	show_list(unique_strings strings);

	let
	    fun get name =
		case find_gender name of
		  NONE   => "unknown"
		| SOME g => show_gender g
	in
	    print(concat["The gender of wilma is ", get "wilma", "\n"])
	end;

	OS.Process.success
    )
    handle x =>
    (
	toErr(exnMessage x); toErr("\n");
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("sort", main)
end
