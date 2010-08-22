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

    (*	A hash table with string keys. *)
    structure STRT_key =
    struct
	type hash_key = string
	val hashVal = HashString.hashString
	fun sameKey (s1, s2) = (s1 = s2)
    end

    structure STRT = HashTableFn(STRT_key)
    exception NotFound


    structure PL = PropList

    (*	Associate a plist holder with each person. *)
    val people: PL.holder STRT.hash_table = STRT.mkTable(101, NotFound)

    (*	Add someone to the table. *)
    fun define name = STRT.insert people (name, PL.newHolder())

    (*	Define some properties.
	Weight is a real measure in kilograms. Father is a string.
    *)
    val weight_prop = PL.newProp (STRT.lookup people, fn _ => 0.0)
    val father_prop = PL.newProp (STRT.lookup people, fn _ => "unknown")

    (*	Functions to set and get the properties. *)
    fun set prop (name, value) =
    let
	val {peekFn, getFn, setFn, clrFn} = prop
    in
	setFn(name, value)
    end


    fun get prop name =
    let
	val {peekFn, getFn, setFn, clrFn} = prop
    in
	getFn name
    end


    val names = ["fred", "wilma", "barney", "betty", "wilma",
                 "pebbles", "bambam"]


    fun show_father name = print(concat[
    			    name, "\thas father ",
			    get father_prop name,
			    "\n"])

    fun show_weight name = print(concat[
    			    name, "\thas weight ",
			    Real.toString(get weight_prop name),
			    "kg\n"])

    fun run() =
    let
    in
	app define names;

	app (set father_prop) [("pebbles", "fred"),
	    		       ("bambam", "barney")
			       ];

	app (set weight_prop) [("fred", 111.0),
			       ("wilma", 73.0),
			       ("barney", 82.5),
			       ("betty", 68.5),
			       ("pebbles", 15.1),
			       ("bambam", 18.7)
			       ];
	app show_father names;
	app show_weight names;
	()
    end


    fun main(arg0, argv) =
    (
	run();
	OS.Process.success
    )
    handle x =>
    (
	toErr(exnMessage x); toErr("\n");
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("plist", main)
end

