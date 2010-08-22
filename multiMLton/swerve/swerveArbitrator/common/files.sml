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


(* This contains utility functions for files.
 * This is leaf module.  Don't add anything that logs error messages. *)

signature FILES =
sig

    (*	Check that a path is absolute. *)
    val absPath:		string -> bool

    (* Get the directory component of a path. If it is not present
     * then it returns "." *)
    val dirName:		string -> string

    (* Get the base component of a path. *)
    val baseName:		string -> string

    (* Split a path into an extension and the rest. If there is
     * no extension then return NONE. The rest will include any
     * directory parts. *)
    val splitExt:		string -> (string * string option)

    (* Append a file name to a directory. *)
    val appendFile:		string -> string -> string

    (* Check that a disk path exists. *)
    val exists:			string -> bool

    (* Check if a disk path is a directory. Symbolic links are followed. *)
    val isDir:			string -> bool

    (* Check if a disk path is a regular file. Symbolic links are followed. *)
    val isReg:			string -> bool

    (* Check if a disk path is a symbolic link. *)
    val isSym:			string -> bool

    (* Check that a disk path is a directory that is
     * accessible for reading and searching. *)
    val accessibleDir:		string -> bool
					  
    (*	Check that a disk path is a directory in which we can create files. *)
    val canCreateInDir:		string -> bool
					  
    (* Check that a disk path is a regular readable file.
     * Symbolic links are followed.
     *)
    val readableReg:		string -> bool
					  
    (* Check that a disk path is a regular readable and writable file.
     * Symbolic links are followed. *)
    val writableReg:		string -> bool
					  
    (*	Check that a disk path is a regular executable file.
     * This does not check for readability. Scripts may need to
     * be readable.
     * Symbolic links are followed. *)
    val execableReg:		string -> bool
					  
end



structure Files: FILES =
struct

  structure FS = Posix.FileSys
  structure P  = OS.Path
		 
  fun absPath path = P.isAbsolute path
		     
  fun dirName path =
      let val {dir, file} = P.splitDirFile path
      in
	  if dir = "" then "." else dir
      end
            
  fun baseName path =
      let val {dir, file} = P.splitDirFile path
      in
	  file
      end
        
    fun splitExt path =
	let val {base, ext} = P.splitBaseExt path
	in
	    (base, ext)
	end    
	
    fun appendFile dir file = P.joinDirFile {dir=dir, file=file}
			      
    fun exists path = FS.access(path, [])
		      
    fun isDir  path = exists path andalso FS.ST.isDir(FS.stat path)
    fun isReg  path = exists path andalso FS.ST.isReg(FS.stat path)
    fun isSym  path = exists path andalso FS.ST.isLink(FS.stat path)
					  
    fun accessibleDir path =
	isDir path andalso FS.access(path, [FS.A_READ, FS.A_EXEC])    

    fun canCreateInDir path =
    	isDir path andalso FS.access(path, [FS.A_READ, FS.A_WRITE, FS.A_EXEC])    

    fun readableReg path =
	isReg path andalso FS.access(path, [FS.A_READ])
    
    fun writableReg path =
    	isReg path andalso FS.access(path, [FS.A_READ, FS.A_WRITE])			   
			   
    fun execableReg path =
    	isReg path andalso FS.access(path, [FS.A_EXEC])
end
