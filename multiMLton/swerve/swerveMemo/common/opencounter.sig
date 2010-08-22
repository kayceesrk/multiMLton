
(*  This manages open files.  It blocks the caller if there are no
 * more file descriptors.  It reaps open files that become garbage
 * because of broken connections etc.

 * There is a generic module for the logic of opening and closing
 * and finalising of files. This is specialised to BinIO and TextIO
 * and directories.  A file stream will be closed if the file token
 * becomes garbage.

 * There is a separate open file counter that allocates file descriptors
 * to limit the number of simultaneous open files. We use a limit of
 * 1/2 of the maximum to allow slack for unknown file activity.

 * Clients can just allocate file descriptors by themselves for example
 * each socket connection will allocate and release a file descriptor
 * to make the books balance.

 * We limit the number of open files to one half of the maximum
 * number of files that can be open. This allows a file descriptor
 * for a connection and another for the file and one more for luck.
 * But we also check to see if we have run out of file descriptors
 * anyway and make the client wait until more are available.

 * REVISIT - instead subtract the maximum number of connections from
 * open max and then another 100 for slack.  Check that this is still
 * large enough. *)

(*==============================================================================*)

signature OPEN_COUNTER =
sig

    (* This represents some number of file descriptors. It ensures
     * that a release matches the allocation. *)
    type allocation

    (*	The protocol consists of:

	1.  The client requests n file descriptors. The allocation will
	    be returned on the channel when available.

	2.  When the allocation is received try to open the files.

	3.  Return a success/fail/retry response to the counter. The
	    counter will block waiting for this response.  This serialises
	    all opens but this shouldn't hurt when running on a single
	    processor machine.

	    If the open was successful then go to step 4.

	    If the open failed due to insufficient file descriptors
	    because some other part of the program has taken them all
	    then the client will be queued. When another file descriptor
	    becomes available the client can continue at step 2.

	    If the open failed for some other reason then the allocation
	    will be released.

	4.  Use the files.

	5.  Release the allocation.
    *)

    datatype response =
	    Success
	|   Fail of allocation
	|   Retry of allocation

    (*	Return the response on the supplied channel. *)
    type start = allocation * response CML.chan

    (*	Pass in a channel to receive the start message. *)
    val request:   (int * start CML.chan) -> unit

    (*	Release n file descriptors. *)
    val release:    allocation -> unit

    (*	Return the number open and the number pending. *)
    val stats:	    unit -> int * int
end

