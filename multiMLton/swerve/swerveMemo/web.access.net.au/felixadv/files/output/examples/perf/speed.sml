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


(*  This runs a variety of speed measurements.

*)

structure Main =
struct

    structure SS    = Substring
    structure S     = String


    fun toErr msg = TextIO.output(TextIO.stdErr, msg)

    fun toTime n = Time.fromSeconds(LargeInt.fromInt n)

    (*----------------------------------------------------------*)


    (*	Count up and down to max_cnt *)

    fun int_arg [] = raise Fail "Missing an integer argument"
    |   int_arg (arg::_) = 
    (
	case Int.fromString arg of
	  NONE => raise Fail(concat["Bad integer argument ", arg])

	| SOME n => n
    )


    fun countdown args =
    let
	val max_cnt = int_arg args

	fun loop 0 = 0			(* it returns something *)
	|   loop n = loop (n-1)
    in
	Timing.timeIt "countdown" (fn () => ignore(loop max_cnt))
    end


    fun countup args =
    let
	val max_cnt = int_arg args

	fun loop n =
	(
	    if n = max_cnt
	    then
		0
	    else
		loop (n+1)
	)
    in
	Timing.timeIt "countup" (fn () => ignore(loop 0))
    end


    (*----------------------------------------------------------*)

    (*	Count lines in a file. 
	The entire file is read in as a string and the new-line chars
	are counted.  This measures simple I/O and string operations.
    *)

    fun countlines args =
    let
	val file =
	    case args of
	      []   => raise Fail "Missing the file name"
	    | f::_ => f

	fun read_all() =
	let
	    val strm = TextIO.openIn file
	in
	    TextIO.inputAll strm
	    before
		TextIO.closeIn strm
	end

	fun report len = 
	(
	    print(concat["Number of lines is ", Int.toString len, "\n"])
	)

	fun count_tokens text =
	let
	    val lines = SS.tokens (fn c => c = #"\n") (SS.all text)
	in
	    length lines
	end


	(*  See if isCntrl is faster. *)
	fun count_cntrl text =
	let
	    val lines = SS.tokens Char.isCntrl (SS.all text)
	in
	    length lines
	end


	(*  Count the characters individually using substring. *)
	fun count_getc text =
	let
	    fun loop ss n =
	    (
		case SS.getc ss of
		  NONE => n
		| SOME (c, rest) =>
		    loop rest (if c = #"\n" then n+1 else n)
	    )
	in
	    loop (SS.all text) 0
	end


	(*  Use plain indexing. This is range-checked so it's slow. *)

	fun count_slowix text =
	let
	    val len = size text

	    fun loop 0 l = l
	    |   loop n l = 
	    (
		loop (n-1) (if S.sub(text, n) = #"\n" then l+1 else l)
	    )
	in
	    loop (len-1) 0
	end


	(*  Use unchecked indexing. This is not range-checked so it's faster. *)

	fun count_fastix text =
	let
	    val len = size text

	    fun loop 0 l = l
	    |   loop n l = 
	    (
		loop (n-1) (if Unsafe.CharVector.sub(text, n) = #"\n"
		            then l+1 else l)
	    )
	in
	    loop (len-1) 0
	end




	val text = Timing.timeIt "countlines readall" read_all
	val () = Timing.timeIt "countlines length " (fn()=>ignore(size text))

	val () = report(Timing.timeIt "countlines tokens " (fn()=>count_tokens text))
	val () = report(Timing.timeIt "countlines cntrl  " (fn()=>count_cntrl text))
	val () = report(Timing.timeIt "countlines getc   " (fn()=>count_getc  text))
	val () = report(Timing.timeIt "countlines slowix " (fn()=>count_slowix text))
	val () = report(Timing.timeIt "countlines fastix " (fn()=>count_fastix text))
    in
	()
    end
    handle x =>
	toErr(concat["Uncaught exception: ", exnMessage x, " in countlines\n"])


    (*----------------------------------------------------------*)

    (*	Memory speed testing. *)

    (* lst should be garbage after this function ends *)
    fun build max_cnt =
    let
	fun loop 0 rslt = rslt
	|   loop n rslt = loop (n-1) (n::rslt)

	val lst = loop max_cnt []
    in
	print(concat["Built a list with length ",
		      Int.toString(length lst), "\n"])
    end


    fun linkedlist args =
    let
	val max_cnt = int_arg args

	fun run() =
	(
	    build max_cnt;
	    SMLofNJ.Internals.GC.doGC 0
	)
    in
	run(); run();			(* go for steady state *)
	SMLofNJ.Internals.GC.messages true;
	SMLofNJ.Internals.GC.doGC 10;	(* clear the heap *)
	print "Starting the run\n";
	Timing.timeIt "linkedlist" run;
	SMLofNJ.Internals.GC.messages false;
	()
    end

    (*----------------------------------------------------------*)


    fun run argv () =
    let

	fun finish when = 
	let
	in
	    Timing.report()
	end
    in
	ignore(RunCML.addCleaner("report", [RunCML.AtShutdown], finish));

	case argv of
	  [] => toErr "no test was specified\n"

	| ("countup"::rest)	=> countup rest
	| ("countdown"::rest)	=> countdown rest
	| ("countlines"::rest)	=> countlines rest
	| ("linkedlist"::rest)	=> linkedlist rest

	| (t::_) => toErr(concat["Unrecognised test ", t, "\n"])
    end
    handle
      Fail msg => (toErr msg; toErr "\n")



    fun main(arg0, argv) =
    let
    in
	RunCML.doit(run argv, NONE);
        OS.Process.success
    end
    handle
      x =>
    (
	toErr(concat["Uncaught exception: ", exnMessage x, " from\n"]);
	app (fn s => (print "\t"; print s; print "\n")) (SMLofNJ.exnHistory x);
	OS.Process.failure
    )

    val _ = SMLofNJ.exportFn("speed", main)
end



