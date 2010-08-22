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


(* This contains functions for operating on files.
 * This will log errors. *)

signature FILEIO =
sig

    (* This removes a file.  It logs errors and raises no exceptions.
     * It is not an error if the file does not exist.
     * The path should be absolute. *)
    val removeFile: string -> unit

    (*	This creates a file while checking for name clashes.
     * It returns true if successful, false if there was a name clash,
     * or logs an error and raises an exception for all other errors.

     * This does not cope with NFS-mounted file systems. *)
    val exclCreate: string -> bool

    (* Return the size of the file in bytes or NONE if the file can't be
     * accessed. *)
    val fileSize: string -> Position.int option

    (* Return the last modification time of the file or NONE if the file
     * doesn't exist. *)
    val modTime: string -> Time.time option

    (*	Set a socket to close-on-exec. *)
    val setCloseExec: OS.IO.poll_desc -> unit

    (*	This handles the opening and closing of an input text file.
     * Exceptions are carefully handled. The default value is returned
     * if an exception happens.

     * This does not use the OpenMgr since it uses TextIO streams
     * directly and callers will want to use the inputLine function.
     * This should just be used during startup for reading configuration
     * files. *)
    val withTextIn: Abort.t -> string -> 'a ->		(* the default *)
		    (TextIO.instream -> 'a) -> 'a

    (* This lists the files in a directory. The names are relative to
     * the given directory. The . and .. directories are not included.
     * This is intended for indexing directories.  The files are in no
     * particular order.
								    
     * This logs and raises an exception if the listing failed
     * e.g. because the directory does not exist or is not accessible.
					    
     * This uses OpenMgr so it is fine for use within connections.
     * The first arg is an abort event. *)
    val listDir: Abort.t -> string -> string list
					     
end



structure FileIO: FILEIO =
struct

  open Common
       
  structure TF  = TextFrag
  structure FS  = Posix.FileSys
  structure IO  = Posix.IO
		  
  fun removeFile file =
      ( if Files.exists file
	then OS.FileSys.remove file
	else () ) handle x => Log.logExnArg file x			      			      
			      
  fun exclCreate file =
      ( 
       IO.close ( FS.createf ( file, FS.O_WRONLY, FS.O.excl,
			       FS.S.flags [ FS.S.irusr, FS.S.iwusr ] ) );
       true 
      ) handle x as OS.SysErr ( _, eopt ) =>
	       ( if isSome eopt andalso valOf eopt = Posix.Error.exist
		 then false	                            (* failed to exclusively create *)
		 else ( Log.logExnArg file x; raise x ) )  (* failed with error *) 
	       
	     | x => ( Log.logExnArg file x; raise x )
		    
  fun fileSize file = 
      SOME ( OS.FileSys.fileSize file ) handle x => ( Log.logExnArg file x; NONE )
		  
  fun modTime file = 
      ( SOME(OS.FileSys.modTime file) )
      handle x => (Log.logExnArg file x; NONE)
		  
  fun setCloseExec poll_desc =
      let	val fd = valOf(FS.iodToFD(OS.IO.pollToIODesc poll_desc))
      in
	  IO.setfd(fd, IO.FD.cloexec)
      end
      
  fun withTextIn abort file default func =
      let in
	  case TextIOReader.openIt abort file of
	      NONE   => default		       (* the open failed *)
	    | SOME h =>
	      ( ( func ( TextIOReader.get h ) )  (* handle an I/O failure with closing *)
		handle x => (Log.logExn x; TextIOReader.closeIt h; default) ) 
	      before ( TextIOReader.closeIt h )
      end handle x => ( Log.logExnArg file x; default ) 
		      
  fun listDir abort dir =
      let fun loop strm rslt =	    
	      case OS.FileSys.readDir strm of
		  NONE => rslt
		| SOME s  => loop strm (s::rslt)
      in
	  case DirReader.openIt abort dir of
	      NONE => []		    (* the open failed *)		    
	    | SOME h => ( loop ( DirReader.get h ) [] ) 
			before ( DirReader.closeIt h )
      end handle x => (Log.logExnArg dir x; raise x) 
		      
end
