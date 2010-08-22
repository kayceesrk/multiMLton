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

    structure F = Format

    fun test_format() =
    (
	F.formatf
	    "A decimal %d, some hex %#08x and some real %.4f\n"
	    print
	    [F.INT ~23, F.WORD 0wxbeef, F.REAL 3.14159265]
    )


    structure S = Scan

    fun test_scan() : unit =
    let
	val items = valOf(S.sscanf "%d %s %f" "123 abc 3.45")

	val display = ListFormat.fmt {
			init  = "[",
			sep   = " ",
			final = "]",
			fmt   = show_item
			} items
    in
	print display; print "\n"
    end


    and show_item (S.INT n)  = Int.toString n
    |   show_item (S.STR s)  = s
    |   show_item (S.REAL r) = Real.toString r
    |   show_item _          = "unknown"



    fun test_scan_io() =
    let
	val _ = print "Enter 3 integers\n"
    in
	case TextIO.scanStream (S.scanf "%d %d %d") TextIO.stdIn of
	  SOME items => (
	    print "got ";
	    print (ListFormat.listToString show_item items);
	    print "\n"
	    )

	| NONE => print "The reading failed\n"
    end



    fun main(arg0, argv) =
    (
	test_format();
	test_scan();
	test_scan_io();
	OS.Process.success
    )
    handle x =>
    (
	toErr(exnMessage x); toErr("\n");
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("format", main)
end
