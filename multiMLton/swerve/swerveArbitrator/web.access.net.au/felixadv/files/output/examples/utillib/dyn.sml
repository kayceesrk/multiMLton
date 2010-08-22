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


    type PList = exn list

    exception PFruit of string
    exception PNum   of int

    val fruit = [PFruit "apple", PNum 5]

    fun get_fruit []            = NONE
    |   get_fruit (PFruit f::_) = SOME f
    |   get_fruit (_::rest)     = get_fruit rest


    fun new_prop dflt =
    let
	exception E of 'a

	fun get []        = NONE
	|   get (E v::_)  = SOME v
	|   get (_::rest) = get rest

	fun set props v = (E v)::props

	fun dummy() = E dflt
    in
	(get, set)
    end

    val (get_colour, set_colour) = new_prop "colour"
    val props2       = set_colour fruit "red"

    val (get_weight, set_weight) = new_prop 0.0
    val props3       = set_weight props2 0.75

    fun report() =
    (
	print(concat[
	    "The colour is ", valOf(get_colour props3),
	    " and the weight is ",
	    Real.toString(valOf(get_weight props3)),
	    "\n"])
    )


    fun main(arg0, argv) =
    (
	report();
	OS.Process.success
    )
    handle x =>
    (
	toErr(exnMessage x); toErr("\n");
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("dyn", main)
end

