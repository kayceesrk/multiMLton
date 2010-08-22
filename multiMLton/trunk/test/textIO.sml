eqtype 'a array = 'a array
datatype bool = false | true
eqtype char = char
type exn = exn
eqtype int = int
datatype 'a list = nil | :: of 'a * 'a list
datatype 'a option = NONE | SOME of 'a
datatype order = LESS | EQUAL | GREATER
type real = real
datatype 'a ref = ref of 'a
eqtype string = string
eqtype substring = char VectorSlice.slice
eqtype unit = unit
eqtype 'a vector = 'a vector
eqtype word = word
val ! : 'a ref -> 'a
val * : 'a * 'a -> 'a
val + : 'a * 'a -> 'a
val - : 'a * 'a -> 'a
val / : 'a * 'a -> 'a
val := : 'a ref * 'a -> unit
val < : 'a * 'a -> bool
val <= : 'a * 'a -> bool
val <> : 'a * 'a -> bool
val = : 'a * 'a -> bool
val > : 'a * 'a -> bool
val >= : 'a * 'a -> bool
val @ : 'a list * 'a list -> 'a list
exception Bind
exception Chr
exception Div
exception Domain
exception Empty
exception Fail of string
exception Match
exception Option
exception Overflow
exception Size
exception Span
exception Subscript
val ^ : string * string -> string
val abs: 'a -> 'a
val app: ('a -> unit) -> 'a list -> unit
val before: 'a * unit -> 'a
val ceil: real -> int
val chr: int -> char
val concat: string list -> string
val div: 'a * 'a -> 'a
val exnMessage: exn -> string
val exnName: exn -> string
val explode: string -> char list
val floor: real -> int
val foldl: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldr: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val getOpt: 'a option * 'a -> 'a
val hd: 'a list -> 'a
val ignore: 'a -> unit
val implode: char list -> string
val isSome: 'a option -> bool
val length: 'a list -> int
val map: ('a -> 'b) -> 'a list -> 'b list
val mod: 'a * 'a -> 'a
val not: bool -> bool
val null: 'a list -> bool
val o: ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
val ord: char -> int
val print: string -> unit
val real: int -> real
val rev: 'a list -> 'a list
val round: real -> int
val size: string -> int
val str: char -> string
val substring: string * int * int -> string
val tl: 'a list -> 'a list
val trunc: real -> int
val valOf: 'a option -> 'a
val vector: 'a list -> 'a vector
val ~ : 'a -> 'a
structure Array: ARRAY
structure Array2: ARRAY2
		  where type 'a array = 'a Array2.array
		  where type traversal = Word64Array2.traversal
structure ArraySlice: ARRAY_SLICE
		      where type 'a slice = 'a ArraySlice.slice
structure BinIO: BIN_IO
		 where type StreamIO.instream = BinIO.StreamIO.instream
		 where type StreamIO.out_pos = BinIO.StreamIO.out_pos
		 where type StreamIO.outstream = BinIO.StreamIO.outstream
		 where type StreamIO.pos = Int64.int
		 where type StreamIO.reader = BinPrimIO.reader
		 where type StreamIO.writer = BinPrimIO.writer
		 where type instream = BinIO.instream
		 where type outstream = BinIO.outstream
structure BinPrimIO: PRIM_IO
		     where type array = Word8.word array
		     where type array_slice = Word8.word ArraySlice.slice
		     where type elem = Word8.word
		     where type pos = Int64.int
		     where type reader = BinPrimIO.reader
		     where type vector = Word8.word vector
		     where type vector_slice = Word8.word VectorSlice.slice
		     where type writer = BinPrimIO.writer
structure Bool: BOOL
structure BoolArray: MONO_ARRAY
		     where type array = BoolArray.array
		     where type elem = bool
		     where type vector = BoolVector.vector
structure BoolArray2: MONO_ARRAY2
		      where type array = BoolArray2.array
		      where type elem = bool
		      where type vector = BoolVector.vector
structure BoolArraySlice: MONO_ARRAY_SLICE
			  where type array = BoolArray.array
			  where type elem = bool
			  where type slice = BoolArraySlice.slice
			  where type vector = BoolVector.vector
			  where type vector_slice = BoolVectorSlice.slice
structure BoolVector: MONO_VECTOR
		      where type elem = bool
		      where type vector = BoolVector.vector
structure BoolVectorSlice: MONO_VECTOR_SLICE
			   where type elem = bool
			   where type slice = BoolVectorSlice.slice
			   where type vector = BoolVector.vector
structure Byte: BYTE
structure Char: CHAR
		where type char = char
		where type string = string
structure CharArray: MONO_ARRAY
		     where type array = CharArray.array
		     where type elem = char
		     where type vector = string
structure CharArray2: MONO_ARRAY2
		      where type array = CharArray2.array
		      where type elem = char
		      where type vector = string
structure CharArraySlice: MONO_ARRAY_SLICE
			  where type array = CharArray.array
			  where type elem = char
			  where type slice = CharArraySlice.slice
			  where type vector = string
			  where type vector_slice = char VectorSlice.slice
structure CharVector: MONO_VECTOR
		      where type elem = char
		      where type vector = string
structure CharVectorSlice: MONO_VECTOR_SLICE
			   where type elem = char
			   where type slice = char VectorSlice.slice
			   where type vector = string
structure CommandLine: COMMAND_LINE
structure Date: DATE
		where type date = Date.date
		where type month = Date.month
		where type weekday = Date.weekday
structure FixedInt: INTEGER
		    where type int = Int64.int
structure General: GENERAL
		   where type exn = exn
		   where type order = order
		   where type unit = unit
structure GenericSock: GENERIC_SOCK
structure IEEEReal: IEEE_REAL
		    where type float_class = IEEEReal.float_class
		    where type real_order = IEEEReal.real_order
		    where type rounding_mode = IEEEReal.rounding_mode
structure INetSock: INET_SOCK
		    where type inet = INetSock.inet
structure IO: IO
	      where type buffer_mode = IO.buffer_mode
structure Int: INTEGER
	       where type int = int
structure Int1: INTEGER
		where type int = Int1.int
structure Int10: INTEGER
		 where type int = Int10.int
structure Int11: INTEGER
		 where type int = Int11.int
structure Int12: INTEGER
		 where type int = Int12.int
structure Int13: INTEGER
		 where type int = Int13.int
structure Int14: INTEGER
		 where type int = Int14.int
structure Int15: INTEGER
		 where type int = Int15.int
structure Int16: INTEGER
		 where type int = Int16.int
structure Int16Array: MONO_ARRAY
		      where type array = Int16Array.array
		      where type elem = Int16.int
		      where type vector = Int16Vector.vector
structure Int16Array2: MONO_ARRAY2
		       where type array = Int16Array2.array
		       where type elem = Int16.int
		       where type vector = Int16Vector.vector
structure Int16ArraySlice: MONO_ARRAY_SLICE
			   where type array = Int16Array.array
			   where type elem = Int16.int
			   where type slice = Int16ArraySlice.slice
			   where type vector = Int16Vector.vector
			   where type vector_slice = Int16VectorSlice.slice
structure Int16Vector: MONO_VECTOR
		       where type elem = Int16.int
		       where type vector = Int16Vector.vector
structure Int16VectorSlice: MONO_VECTOR_SLICE
			    where type elem = Int16.int
			    where type slice = Int16VectorSlice.slice
			    where type vector = Int16Vector.vector
structure Int17: INTEGER
		 where type int = Int17.int
structure Int18: INTEGER
		 where type int = Int18.int
structure Int19: INTEGER
		 where type int = Int19.int
structure Int2: INTEGER
		where type int = Int2.int
structure Int20: INTEGER
		 where type int = Int20.int
structure Int21: INTEGER
		 where type int = Int21.int
structure Int22: INTEGER
		 where type int = Int22.int
structure Int23: INTEGER
		 where type int = Int23.int
structure Int24: INTEGER
		 where type int = Int24.int
structure Int25: INTEGER
		 where type int = Int25.int
structure Int26: INTEGER
		 where type int = Int26.int
structure Int27: INTEGER
		 where type int = Int27.int
structure Int28: INTEGER
		 where type int = Int28.int
structure Int29: INTEGER
		 where type int = Int29.int
structure Int3: INTEGER
		where type int = Int3.int
structure Int30: INTEGER
		 where type int = Int30.int
structure Int31: INTEGER
		 where type int = Int31.int
structure Int32: INTEGER
		 where type int = int
structure Int32Array: MONO_ARRAY
		      where type array = Int32Array.array
		      where type elem = int
		      where type vector = Int32Vector.vector
structure Int32Array2: MONO_ARRAY2
		       where type array = Int32Array2.array
		       where type elem = int
		       where type vector = Int32Vector.vector
structure Int32ArraySlice: MONO_ARRAY_SLICE
			   where type array = Int32Array.array
			   where type elem = int
			   where type slice = Int32ArraySlice.slice
			   where type vector = Int32Vector.vector
			   where type vector_slice = Int32VectorSlice.slice
structure Int32Vector: MONO_VECTOR
		       where type elem = int
		       where type vector = Int32Vector.vector
structure Int32VectorSlice: MONO_VECTOR_SLICE
			    where type elem = int
			    where type slice = Int32VectorSlice.slice
			    where type vector = Int32Vector.vector
structure Int4: INTEGER
		where type int = Int4.int
structure Int5: INTEGER
		where type int = Int5.int
structure Int6: INTEGER
		where type int = Int6.int
structure Int64: INTEGER
		 where type int = Int64.int
structure Int64Array: MONO_ARRAY
		      where type array = Int64Array.array
		      where type elem = Int64.int
		      where type vector = Int64Vector.vector
structure Int64Array2: MONO_ARRAY2
		       where type array = Int64Array2.array
		       where type elem = Int64.int
		       where type vector = Int64Vector.vector
structure Int64ArraySlice: MONO_ARRAY_SLICE
			   where type array = Int64Array.array
			   where type elem = Int64.int
			   where type slice = Int64ArraySlice.slice
			   where type vector = Int64Vector.vector
			   where type vector_slice = Int64VectorSlice.slice
structure Int64Vector: MONO_VECTOR
		       where type elem = Int64.int
		       where type vector = Int64Vector.vector
structure Int64VectorSlice: MONO_VECTOR_SLICE
			    where type elem = Int64.int
			    where type slice = Int64VectorSlice.slice
			    where type vector = Int64Vector.vector
structure Int7: INTEGER
		where type int = Int7.int
structure Int8: INTEGER
		where type int = Int8.int
structure Int8Array: MONO_ARRAY
		     where type array = Int8Array.array
		     where type elem = Int8.int
		     where type vector = Int8Vector.vector
structure Int8Array2: MONO_ARRAY2
		      where type array = Int8Array2.array
		      where type elem = Int8.int
		      where type vector = Int8Vector.vector
structure Int8ArraySlice: MONO_ARRAY_SLICE
			  where type array = Int8Array.array
			  where type elem = Int8.int
			  where type slice = Int8ArraySlice.slice
			  where type vector = Int8Vector.vector
			  where type vector_slice = Int8VectorSlice.slice
structure Int8Vector: MONO_VECTOR
		      where type elem = Int8.int
		      where type vector = Int8Vector.vector
structure Int8VectorSlice: MONO_VECTOR_SLICE
			   where type elem = Int8.int
			   where type slice = Int8VectorSlice.slice
			   where type vector = Int8Vector.vector
structure Int9: INTEGER
		where type int = Int9.int
structure IntArray: MONO_ARRAY
		    where type array = IntArray.array
		    where type elem = int
		    where type vector = IntVector.vector
structure IntArray2: MONO_ARRAY2
		     where type array = IntArray2.array
		     where type elem = int
		     where type vector = IntVector.vector
structure IntArraySlice: MONO_ARRAY_SLICE
			 where type array = IntArray.array
			 where type elem = int
			 where type slice = IntArraySlice.slice
			 where type vector = IntVector.vector
			 where type vector_slice = IntVectorSlice.slice
structure IntInf: INT_INF
		  where type int = IntInf.int
structure IntVector: MONO_VECTOR
		     where type elem = int
		     where type vector = IntVector.vector
structure IntVectorSlice: MONO_VECTOR_SLICE
			  where type elem = int
			  where type slice = IntVectorSlice.slice
			  where type vector = IntVector.vector
structure LargeInt: INTEGER
		    where type int = IntInf.int
structure LargeIntArray: MONO_ARRAY
			 where type array = LargeIntArray.array
			 where type elem = IntInf.int
			 where type vector = LargeIntVector.vector
structure LargeIntArray2: MONO_ARRAY2
			  where type array = LargeIntArray2.array
			  where type elem = IntInf.int
			  where type vector = LargeIntVector.vector
structure LargeIntArraySlice: MONO_ARRAY_SLICE
			      where type array = LargeIntArray.array
			      where type elem = IntInf.int
			      where type slice = LargeIntArraySlice.slice
			      where type vector = LargeIntVector.vector
			      where type vector_slice = LargeIntVectorSlice.slice
structure LargeIntVector: MONO_VECTOR
			  where type elem = IntInf.int
			  where type vector = LargeIntVector.vector
structure LargeIntVectorSlice: MONO_VECTOR_SLICE
			       where type elem = IntInf.int
			       where type slice = LargeIntVectorSlice.slice
			       where type vector = LargeIntVector.vector
structure LargeReal: REAL
		     where type real = real
structure LargeRealArray: MONO_ARRAY
			  where type array = LargeRealArray.array
			  where type elem = real
			  where type vector = LargeRealVector.vector
structure LargeRealArray2: MONO_ARRAY2
			   where type array = LargeRealArray2.array
			   where type elem = real
			   where type vector = LargeRealVector.vector
structure LargeRealArraySlice: MONO_ARRAY_SLICE
			       where type array = LargeRealArray.array
			       where type elem = real
			       where type slice = LargeRealArraySlice.slice
			       where type vector = LargeRealVector.vector
			       where type vector_slice = LargeRealVectorSlice.slice
structure LargeRealVector: MONO_VECTOR
			   where type elem = real
			   where type vector = LargeRealVector.vector
structure LargeRealVectorSlice: MONO_VECTOR_SLICE
				where type elem = real
				where type slice = LargeRealVectorSlice.slice
				where type vector = LargeRealVector.vector
structure LargeWord: WORD
		     where type word = Word64.word
structure LargeWordArray: MONO_ARRAY
			  where type array = LargeWordArray.array
			  where type elem = Word64.word
			  where type vector = LargeWordVector.vector
structure LargeWordArray2: MONO_ARRAY2
			   where type array = LargeWordArray2.array
			   where type elem = Word64.word
			   where type vector = LargeWordVector.vector
structure LargeWordArraySlice: MONO_ARRAY_SLICE
			       where type array = LargeWordArray.array
			       where type elem = Word64.word
			       where type slice = LargeWordArraySlice.slice
			       where type vector = LargeWordVector.vector
			       where type vector_slice = LargeWordVectorSlice.slice
structure LargeWordVector: MONO_VECTOR
			   where type elem = Word64.word
			   where type vector = LargeWordVector.vector
structure LargeWordVectorSlice: MONO_VECTOR_SLICE
				where type elem = Word64.word
				where type slice = LargeWordVectorSlice.slice
				where type vector = LargeWordVector.vector
structure List: LIST
structure ListPair: LIST_PAIR
structure MLton: MLTON
		 where type BinIO.instream = BinIO.instream
		 where type BinIO.outstream = BinIO.outstream
		 where type CharArray.elem = MLton.CharArray.elem
		 where type CharArray.t = MLton.CharArray.t
		 where type CharVector.elem = MLton.CharVector.elem
		 where type CharVector.t = MLton.CharVector.t
		 where type 'a Cont.t = 'a MLton.Cont.t
		 where type 'a Finalizable.t = 'a MLton.Finalizable.t
		 where type IntInf.BigWord.word = MLton.IntInf.BigWord.word
		 where type IntInf.SmallInt.int = MLton.IntInf.SmallInt.int
		 where type IntInf.rep = MLton.IntInf.rep
		 where type IntInf.t = IntInf.int
		 where type Itimer.t = MLton.Itimer.t
		 where type LargeReal.t = real
		 where type LargeWord.t = Word64.word
		 where type 'a PCML.Mailbox.mbox = 'a MLton.PCML.Mailbox.mbox
		 where type 'a PCML.Multicast.mchan = 'a MLton.PCML.Multicast.mchan
		 where type 'a PCML.Multicast.port = 'a MLton.PCML.Multicast.port
		 where type PCML.MutexLock.mutexlock = MLton.PCML.MutexLock.mutexlock
		 where type PCML.NonBlockingIO.NBTextIO.StreamIO.instream = MLton.PCML.NonBlockingIO.NBTextIO.StreamIO.instream
		 where type PCML.NonBlockingIO.NBTextIO.StreamIO.out_pos = MLton.PCML.NonBlockingIO.NBTextIO.StreamIO.out_pos
		 where type PCML.NonBlockingIO.NBTextIO.StreamIO.outstream = MLton.PCML.NonBlockingIO.NBTextIO.StreamIO.outstream
		 where type PCML.NonBlockingIO.NBTextIO.instream = MLton.PCML.NonBlockingIO.NBTextIO.instream
		 where type PCML.NonBlockingIO.NBTextIO.outstream = MLton.PCML.NonBlockingIO.NBTextIO.outstream
		 where type 'a PCML.SyncVar.ivar = 'a MLton.PCML.SyncVar.ivar
		 where type 'a PCML.SyncVar.mvar = 'a MLton.PCML.SyncVar.mvar
		 where type 'a PCML.Threadlet.asyncChan = 'a MLton.PCML.Threadlet.asyncChan
		 where type 'a PCML.chan = 'a MLton.PCML.chan
		 where type 'a PCML.event = 'a MLton.PCML.event
		 where type PCML.thread_id = MLton.PCML.thread_id
		 where type PCML.thread_state = MLton.PCML.thread_state
		 where type Platform.Arch.t = MLton.Platform.Arch.t
		 where type Platform.Format.t = MLton.Platform.Format.t
		 where type Platform.OS.t = MLton.Platform.OS.t
		 where type Pointer.t = MLton.Pointer.t
		 where type ProcEnv.gid = Posix.FileSys.gid
		 where type ('a, 'b) Process.Child.t = ('a, 'b) MLton.Process.Child.t
		 where type ('a, 'b) Process.Param.t = ('a, 'b) MLton.Process.Param.t
		 where type Process.any = MLton.Process.any
		 where type Process.chain = MLton.Process.chain
		 where type Process.input = MLton.Process.input
		 where type Process.none = MLton.Process.none
		 where type Process.output = MLton.Process.output
		 where type Process.pid = Posix.IO.pid
		 where type ('a, 'b, 'c) Process.t = ('a, 'b, 'c) MLton.Process.t
		 where type Profile.Data.t = MLton.Profile.Data.t
		 where type Real.t = real
		 where type Real32.t = Real32.real
		 where type Real64.t = real
		 where type Rlimit.RLim.t = MLton.Rlimit.RLim.t
		 where type Rlimit.t = MLton.Rlimit.t
		 where type Signal.Handler.t = MLton.Signal.Handler.t
		 where type Signal.Mask.t = MLton.Signal.Mask.t
		 where type Signal.signal = Unix.signal
		 where type Socket.t = MLton.Socket.t
		 where type Syslog.facility = MLton.Syslog.facility
		 where type Syslog.loglevel = MLton.Syslog.loglevel
		 where type Syslog.openflag = MLton.Syslog.openflag
		 where type TextIO.instream = TextIO.instream
		 where type TextIO.outstream = TextIO.outstream
		 where type Thread.AtomicState.t = MLton.Thread.AtomicState.t
		 where type Thread.Runnable.t = MLton.Thread.Runnable.t
		 where type 'a Thread.t = 'a MLton.Thread.t
		 where type 'a Weak.t = 'a MLton.Weak.t
		 where type Word.t = word
		 where type Word16.t = Word16.word
		 where type Word32.t = word
		 where type Word64.t = Word64.word
		 where type Word8.t = Word8.word
		 where type Word8Array.elem = MLton.Word8Array.elem
		 where type Word8Array.t = Word8.word array
		 where type Word8Vector.elem = MLton.Word8Vector.elem
		 where type Word8Vector.t = Word8.word vector
		 where type World.status = MLton.World.status
structure Math: MATH
		where type real = real
structure NetHostDB: NET_HOST_DB
		     where type addr_family = NetHostDB.addr_family
		     where type entry = NetHostDB.entry
		     where type in_addr = Word8.word vector
structure NetProtDB: NET_PROT_DB
		     where type entry = NetProtDB.entry
structure NetServDB: NET_SERV_DB
		     where type entry = NetServDB.entry
structure OS: OS
	      where type FileSys.access_mode = OS.FileSys.access_mode
	      where type FileSys.dirstream = OS.FileSys.dirstream
	      where type FileSys.file_id = OS.FileSys.file_id
	      where type IO.iodesc = OS.IO.iodesc
	      where type IO.iodesc_kind = OS.IO.iodesc_kind
	      where type IO.poll_desc = OS.IO.poll_desc
	      where type IO.poll_info = OS.IO.poll_info
	      where type Process.status = OS.Process.status
	      where type syserror = OS.syserror
structure Option: OPTION
		  where type 'a option = 'a option
structure PackReal32Big: PACK_REAL
			 where type real = Real32.real
structure PackReal32Little: PACK_REAL
			    where type real = Real32.real
structure PackReal64Big: PACK_REAL
			 where type real = real
structure PackReal64Little: PACK_REAL
			    where type real = real
structure PackRealBig: PACK_REAL
		       where type real = real
structure PackRealLittle: PACK_REAL
			  where type real = real
structure PackWord16Big: PACK_WORD
structure PackWord16Little: PACK_WORD
structure PackWord32Big: PACK_WORD
structure PackWord32Little: PACK_WORD
structure PackWord64Big: PACK_WORD
structure PackWord64Little: PACK_WORD
structure Position: INTEGER
		    where type int = Int64.int
structure Posix: POSIX
		 where type Error.syserror = OS.syserror
		 where type FileSys.O.flags = Posix.FileSys.O.flags
		 where type FileSys.S.mode = Posix.FileSys.S.flags
		 where type FileSys.ST.stat = Posix.FileSys.ST.stat
		 where type FileSys.access_mode = OS.FileSys.access_mode
		 where type FileSys.dev = Posix.FileSys.dev
		 where type FileSys.dirstream = OS.FileSys.dirstream
		 where type FileSys.file_desc = Posix.FileSys.file_desc
		 where type FileSys.gid = Posix.FileSys.gid
		 where type FileSys.ino = Posix.FileSys.ino
		 where type FileSys.open_mode = Posix.FileSys.open_mode
		 where type FileSys.uid = Posix.FileSys.uid
		 where type IO.FD.flags = Posix.IO.FD.flags
		 where type IO.FLock.flock = Posix.IO.FLock.flock
		 where type IO.O.flags = Posix.IO.O.flags
		 where type IO.lock_type = Posix.IO.lock_type
		 where type IO.pid = Posix.IO.pid
		 where type IO.whence = Posix.IO.whence
		 where type Process.W.flags = Posix.Process.W.flags
		 where type Process.exit_status = Unix.exit_status
		 where type Process.killpid_arg = Posix.Process.killpid_arg
		 where type Process.signal = Unix.signal
		 where type Process.waitpid_arg = Posix.Process.waitpid_arg
		 where type SysDB.Group.group = Posix.SysDB.Group.group
		 where type SysDB.Passwd.passwd = Posix.SysDB.Passwd.passwd
		 where type TTY.C.flags = Posix.TTY.C.flags
		 where type TTY.I.flags = Posix.TTY.I.flags
		 where type TTY.L.flags = Posix.TTY.L.flags
		 where type TTY.O.flags = Posix.TTY.O.flags
		 where type TTY.TC.flow_action = Posix.TTY.TC.flow_action
		 where type TTY.TC.queue_sel = Posix.TTY.TC.queue_sel
		 where type TTY.TC.set_action = Posix.TTY.TC.set_action
		 where type TTY.V.cc = Posix.TTY.V.cc
		 where type TTY.speed = Posix.TTY.speed
		 where type TTY.termios = Posix.TTY.termios
structure Real: REAL
		where type real = real
structure Real32: REAL
		  where type real = Real32.real
structure Real32Array: MONO_ARRAY
		       where type array = Real32Array.array
		       where type elem = Real32.real
		       where type vector = Real32Vector.vector
structure Real32Array2: MONO_ARRAY2
			where type array = Real32Array2.array
			where type elem = Real32.real
			where type vector = Real32Vector.vector
structure Real32ArraySlice: MONO_ARRAY_SLICE
			    where type array = Real32Array.array
			    where type elem = Real32.real
			    where type slice = Real32ArraySlice.slice
			    where type vector = Real32Vector.vector
			    where type vector_slice = Real32VectorSlice.slice
structure Real32Vector: MONO_VECTOR
			where type elem = Real32.real
			where type vector = Real32Vector.vector
structure Real32VectorSlice: MONO_VECTOR_SLICE
			     where type elem = Real32.real
			     where type slice = Real32VectorSlice.slice
			     where type vector = Real32Vector.vector
structure Real64: REAL
		  where type real = real
structure Real64Array: MONO_ARRAY
		       where type array = Real64Array.array
		       where type elem = real
		       where type vector = Real64Vector.vector
structure Real64Array2: MONO_ARRAY2
			where type array = Real64Array2.array
			where type elem = real
			where type vector = Real64Vector.vector
structure Real64ArraySlice: MONO_ARRAY_SLICE
			    where type array = Real64Array.array
			    where type elem = real
			    where type slice = Real64ArraySlice.slice
			    where type vector = Real64Vector.vector
			    where type vector_slice = Real64VectorSlice.slice
structure Real64Vector: MONO_VECTOR
			where type elem = real
			where type vector = Real64Vector.vector
structure Real64VectorSlice: MONO_VECTOR_SLICE
			     where type elem = real
			     where type slice = Real64VectorSlice.slice
			     where type vector = Real64Vector.vector
structure RealArray: MONO_ARRAY
		     where type array = RealArray.array
		     where type elem = real
		     where type vector = RealVector.vector
structure RealArray2: MONO_ARRAY2
		      where type array = RealArray2.array
		      where type elem = real
		      where type vector = RealVector.vector
structure RealArraySlice: MONO_ARRAY_SLICE
			  where type array = RealArray.array
			  where type elem = real
			  where type slice = RealArraySlice.slice
			  where type vector = RealVector.vector
			  where type vector_slice = RealVectorSlice.slice
structure RealVector: MONO_VECTOR
		      where type elem = real
		      where type vector = RealVector.vector
structure RealVectorSlice: MONO_VECTOR_SLICE
			   where type elem = real
			   where type slice = RealVectorSlice.slice
			   where type vector = RealVector.vector
structure SMLofNJ: SML_OF_NJ
		   where type 'a Cont.cont = 'a SMLofNJ.Cont.cont
		   where type SysInfo.os_kind = SMLofNJ.SysInfo.os_kind
structure Socket: SOCKET
		  where type SOCK.sock_type = Socket.SOCK.sock_type
		  where type active = Socket.active
		  where type dgram = Socket.dgram
		  where type passive = Socket.passive
		  where type shutdown_mode = Socket.shutdown_mode
		  where type ('a, 'b) sock = ('a, 'b) Socket.sock
		  where type 'a sock_addr = 'a Socket.sock_addr
		  where type sock_desc = Socket.sock_desc
		  where type 'a stream = 'a Socket.stream
structure String: STRING
		  where type char = char
		  where type string = string
structure StringCvt: STRING_CVT
		     where type cs = StringCvt.cs
		     where type radix = StringCvt.radix
		     where type realfmt = StringCvt.realfmt
structure Substring: SUBSTRING
		     where type char = char
		     where type string = string
		     where type substring = char VectorSlice.slice
structure SysWord: WORD
		   where type word = Word64.word
structure Text: TEXT
		where type Char.char = char
		where type Char.string = string
		where type CharArray.array = CharArray.array
		where type CharArraySlice.slice = CharArraySlice.slice
		where type CharArraySlice.vector_slice = char VectorSlice.slice
		where type Substring.substring = char VectorSlice.slice
structure TextIO: TEXT_IO
		  where type StreamIO.instream = TextIO.StreamIO.instream
		  where type StreamIO.out_pos = TextIO.StreamIO.out_pos
		  where type StreamIO.outstream = TextIO.StreamIO.outstream
		  where type instream = TextIO.instream
		  where type outstream = TextIO.outstream
structure TextPrimIO: PRIM_IO
		      where type array = CharArray.array
		      where type array_slice = CharArraySlice.slice
		      where type elem = char
		      where type pos = Int64.int
		      where type reader = TextPrimIO.reader
		      where type vector = string
		      where type vector_slice = char VectorSlice.slice
		      where type writer = TextPrimIO.writer
structure Time: TIME
		where type time = Time.time
structure Timer: TIMER
		 where type cpu_timer = Timer.cpu_timer
		 where type real_timer = Timer.real_timer
structure Unix: UNIX
		where type exit_status = Unix.exit_status
		where type ('a, 'b) proc = ('a, 'b) Unix.proc
		where type signal = Unix.signal
structure UnixSock: UNIX_SOCK
		    where type unix = UnixSock.unix
structure Unsafe: UNSAFE
		  where type BoolArray.array = BoolArray.array
		  where type BoolArray.elem = bool
		  where type BoolVector.elem = bool
		  where type BoolVector.vector = BoolVector.vector
		  where type CharArray.array = CharArray.array
		  where type CharArray.elem = char
		  where type CharVector.elem = char
		  where type CharVector.vector = string
		  where type Int16Array.array = Int16Array.array
		  where type Int16Array.elem = Int16.int
		  where type Int16Vector.elem = Int16.int
		  where type Int16Vector.vector = Int16Vector.vector
		  where type Int32Array.array = Int32Array.array
		  where type Int32Array.elem = int
		  where type Int32Vector.elem = int
		  where type Int32Vector.vector = Int32Vector.vector
		  where type Int64Array.array = Int64Array.array
		  where type Int64Array.elem = Int64.int
		  where type Int64Vector.elem = Int64.int
		  where type Int64Vector.vector = Int64Vector.vector
		  where type Int8Array.array = Int8Array.array
		  where type Int8Array.elem = Int8.int
		  where type Int8Vector.elem = Int8.int
		  where type Int8Vector.vector = Int8Vector.vector
		  where type IntArray.array = IntArray.array
		  where type IntArray.elem = int
		  where type IntInfArray.array = Unsafe.IntInfArray.array
		  where type IntInfArray.elem = Unsafe.IntInfArray.elem
		  where type IntInfVector.elem = Unsafe.IntInfVector.elem
		  where type IntInfVector.vector = Unsafe.IntInfVector.vector
		  where type IntVector.elem = int
		  where type IntVector.vector = IntVector.vector
		  where type LargeIntArray.array = LargeIntArray.array
		  where type LargeIntArray.elem = IntInf.int
		  where type LargeIntVector.elem = IntInf.int
		  where type LargeIntVector.vector = LargeIntVector.vector
		  where type LargeRealArray.array = LargeRealArray.array
		  where type LargeRealArray.elem = real
		  where type LargeRealVector.elem = real
		  where type LargeRealVector.vector = LargeRealVector.vector
		  where type LargeWordArray.array = LargeWordArray.array
		  where type LargeWordArray.elem = Word64.word
		  where type LargeWordVector.elem = Word64.word
		  where type LargeWordVector.vector = LargeWordVector.vector
		  where type Real32Array.array = Real32Array.array
		  where type Real32Array.elem = Real32.real
		  where type Real32Vector.elem = Real32.real
		  where type Real32Vector.vector = Real32Vector.vector
		  where type Real64Array.array = Real64Array.array
		  where type Real64Array.elem = real
		  where type Real64Vector.elem = real
		  where type Real64Vector.vector = Real64Vector.vector
		  where type RealArray.array = RealArray.array
		  where type RealArray.elem = real
		  where type RealVector.elem = real
		  where type RealVector.vector = RealVector.vector
		  where type Word16Array.array = Word16Array.array
		  where type Word16Array.elem = Word16.word
		  where type Word16Vector.elem = Word16.word
		  where type Word16Vector.vector = Word16Vector.vector
		  where type Word32Array.array = Word32Array.array
		  where type Word32Array.elem = word
		  where type Word32Vector.elem = word
		  where type Word32Vector.vector = Word32Vector.vector
		  where type Word64Array.array = Word64Array.array
		  where type Word64Array.elem = Word64.word
		  where type Word64Vector.elem = Word64.word
		  where type Word64Vector.vector = Word64Vector.vector
		  where type Word8Array.array = Word8.word array
		  where type Word8Array.elem = Word8.word
		  where type Word8Vector.elem = Word8.word
		  where type Word8Vector.vector = Word8.word vector
		  where type WordArray.array = WordArray.array
		  where type WordArray.elem = word
		  where type WordVector.elem = word
		  where type WordVector.vector = WordVector.vector
structure Vector: VECTOR
structure VectorSlice: VECTOR_SLICE
		       where type 'a slice = 'a VectorSlice.slice
structure WideChar: CHAR
		    where type char = WideSubstring.char
		    where type string = WideSubstring.char vector
structure WideCharArray: MONO_ARRAY
			 where type array = WideCharArraySlice.array
			 where type elem = WideSubstring.char
			 where type vector = WideSubstring.char vector
structure WideCharArray2: MONO_ARRAY2
			  where type array = WideCharArray2.array
			  where type elem = WideSubstring.char
			  where type vector = WideSubstring.char vector
structure WideCharArraySlice: MONO_ARRAY_SLICE
			      where type array = WideCharArraySlice.array
			      where type elem = WideSubstring.char
			      where type slice = WideCharArraySlice.slice
			      where type vector = WideSubstring.char vector
			      where type vector_slice = WideSubstring.substring
structure WideCharVector: MONO_VECTOR
			  where type elem = WideSubstring.char
			  where type vector = WideSubstring.char vector
structure WideCharVectorSlice: MONO_VECTOR_SLICE
			       where type elem = WideSubstring.char
			       where type slice = WideSubstring.substring
			       where type vector = WideSubstring.char vector
structure WideString: STRING
		      where type char = WideSubstring.char
		      where type string = WideSubstring.char vector
structure WideSubstring: SUBSTRING
			 where type char = WideSubstring.char
			 where type string = WideSubstring.char vector
			 where type substring = WideSubstring.substring
structure WideText: TEXT
		    where type Char.char = WideSubstring.char
		    where type Char.string = WideSubstring.char vector
		    where type CharArray.array = WideCharArraySlice.array
		    where type CharArraySlice.slice = WideCharArraySlice.slice
		    where type CharArraySlice.vector_slice = WideSubstring.substring
		    where type Substring.substring = WideSubstring.substring
structure Word: WORD
		where type word = word
structure Word1: WORD
		 where type word = Word1.word
structure Word10: WORD
		  where type word = Word10.word
structure Word11: WORD
		  where type word = Word11.word
structure Word12: WORD
		  where type word = Word12.word
structure Word13: WORD
		  where type word = Word13.word
structure Word14: WORD
		  where type word = Word14.word
structure Word15: WORD
		  where type word = Word15.word
structure Word16: WORD
		  where type word = Word16.word
structure Word16Array: MONO_ARRAY
		       where type array = Word16Array.array
		       where type elem = Word16.word
		       where type vector = Word16Vector.vector
structure Word16Array2: MONO_ARRAY2
			where type array = Word16Array2.array
			where type elem = Word16.word
			where type vector = Word16Vector.vector
structure Word16ArraySlice: MONO_ARRAY_SLICE
			    where type array = Word16Array.array
			    where type elem = Word16.word
			    where type slice = Word16ArraySlice.slice
			    where type vector = Word16Vector.vector
			    where type vector_slice = Word16VectorSlice.slice
structure Word16Vector: MONO_VECTOR
			where type elem = Word16.word
			where type vector = Word16Vector.vector
structure Word16VectorSlice: MONO_VECTOR_SLICE
			     where type elem = Word16.word
			     where type slice = Word16VectorSlice.slice
			     where type vector = Word16Vector.vector
structure Word17: WORD
		  where type word = Word17.word
structure Word18: WORD
		  where type word = Word18.word
structure Word19: WORD
		  where type word = Word19.word
structure Word2: WORD
		 where type word = Word2.word
structure Word20: WORD
		  where type word = Word20.word
structure Word21: WORD
		  where type word = Word21.word
structure Word22: WORD
		  where type word = Word22.word
structure Word23: WORD
		  where type word = Word23.word
structure Word24: WORD
		  where type word = Word24.word
structure Word25: WORD
		  where type word = Word25.word
structure Word26: WORD
		  where type word = Word26.word
structure Word27: WORD
		  where type word = Word27.word
structure Word28: WORD
		  where type word = Word28.word
structure Word29: WORD
		  where type word = Word29.word
structure Word3: WORD
		 where type word = Word3.word
structure Word30: WORD
		  where type word = Word30.word
structure Word31: WORD
		  where type word = Word31.word
structure Word32: WORD
		  where type word = word
structure Word32Array: MONO_ARRAY
		       where type array = Word32Array.array
		       where type elem = word
		       where type vector = Word32Vector.vector
structure Word32Array2: MONO_ARRAY2
			where type array = Word32Array2.array
			where type elem = word
			where type vector = Word32Vector.vector
structure Word32ArraySlice: MONO_ARRAY_SLICE
			    where type array = Word32Array.array
			    where type elem = word
			    where type slice = Word32ArraySlice.slice
			    where type vector = Word32Vector.vector
			    where type vector_slice = Word32VectorSlice.slice
structure Word32Vector: MONO_VECTOR
			where type elem = word
			where type vector = Word32Vector.vector
structure Word32VectorSlice: MONO_VECTOR_SLICE
			     where type elem = word
			     where type slice = Word32VectorSlice.slice
			     where type vector = Word32Vector.vector
structure Word4: WORD
		 where type word = Word4.word
structure Word5: WORD
		 where type word = Word5.word
structure Word6: WORD
		 where type word = Word6.word
structure Word64: WORD
		  where type word = Word64.word
structure Word64Array: MONO_ARRAY
		       where type array = Word64Array.array
		       where type elem = Word64.word
		       where type vector = Word64Vector.vector
structure Word64Array2: MONO_ARRAY2
			where type array = Word64Array2.array
			where type elem = Word64.word
			where type vector = Word64Vector.vector
structure Word64ArraySlice: MONO_ARRAY_SLICE
			    where type array = Word64Array.array
			    where type elem = Word64.word
			    where type slice = Word64ArraySlice.slice
			    where type vector = Word64Vector.vector
			    where type vector_slice = Word64VectorSlice.slice
structure Word64Vector: MONO_VECTOR
			where type elem = Word64.word
			where type vector = Word64Vector.vector
structure Word64VectorSlice: MONO_VECTOR_SLICE
			     where type elem = Word64.word
			     where type slice = Word64VectorSlice.slice
			     where type vector = Word64Vector.vector
structure Word7: WORD
		 where type word = Word7.word
structure Word8: WORD
		 where type word = Word8.word
structure Word8Array: MONO_ARRAY
		      where type array = Word8.word array
		      where type elem = Word8.word
		      where type vector = Word8.word vector
structure Word8Array2: MONO_ARRAY2
		       where type array = Word8Array2.array
		       where type elem = Word8.word
		       where type vector = Word8.word vector
structure Word8ArraySlice: MONO_ARRAY_SLICE
			   where type array = Word8.word array
			   where type elem = Word8.word
			   where type slice = Word8.word ArraySlice.slice
			   where type vector = Word8.word vector
			   where type vector_slice = Word8.word VectorSlice.slice
structure Word8Vector: MONO_VECTOR
		       where type elem = Word8.word
		       where type vector = Word8.word vector
structure Word8VectorSlice: MONO_VECTOR_SLICE
			    where type elem = Word8.word
			    where type slice = Word8.word VectorSlice.slice
			    where type vector = Word8.word vector
structure Word9: WORD
		 where type word = Word9.word
structure WordArray: MONO_ARRAY
		     where type array = WordArray.array
		     where type elem = word
		     where type vector = WordVector.vector
structure WordArray2: MONO_ARRAY2
		      where type array = WordArray2.array
		      where type elem = word
		      where type vector = WordVector.vector
structure WordArraySlice: MONO_ARRAY_SLICE
			  where type array = WordArray.array
			  where type elem = word
			  where type slice = WordArraySlice.slice
			  where type vector = WordVector.vector
			  where type vector_slice = WordVectorSlice.slice
structure WordVector: MONO_VECTOR
		      where type elem = word
		      where type vector = WordVector.vector
structure WordVectorSlice: MONO_VECTOR_SLICE
			   where type elem = word
			   where type slice = WordVectorSlice.slice
			   where type vector = WordVector.vector
functor ImperativeIO (S: sig
			    structure Array: MONO_ARRAY
					     where type array = S.Array.array
					     where type elem = S.Array.elem
					     where type vector = S.Array.vector
			    structure StreamIO: STREAM_IO
						where type elem = S.Array.elem
						where type instream = S.StreamIO.instream
						where type out_pos = S.StreamIO.out_pos
						where type outstream = S.StreamIO.outstream
						where type pos = S.StreamIO.pos
						where type reader = S.StreamIO.reader
						where type vector = S.StreamIO.vector
						where type writer = S.StreamIO.writer
			    structure Vector: MONO_VECTOR
					      where type elem = S.Array.elem
					      where type vector = S.Array.vector
			 end)
   : IMPERATIVE_IO
     where type StreamIO.instream = S.StreamIO.instream
     where type StreamIO.out_pos = S.StreamIO.out_pos
     where type StreamIO.outstream = S.StreamIO.outstream
     where type StreamIO.pos = S.StreamIO.pos
     where type StreamIO.reader = S.StreamIO.reader
     where type StreamIO.writer = S.StreamIO.writer
     where type elem = S.Array.elem
     where type instream = ImperativeIO.instream
     where type outstream = ImperativeIO.outstream
     where type vector = S.StreamIO.vector
functor PrimIO (S: sig
		      eqtype pos = S.pos
		      val compare: S.pos * S.pos -> order
		      val someElem: S.Array.elem
		      structure Array: MONO_ARRAY
				       where type array = S.Array.array
				       where type elem = S.Array.elem
				       where type vector = S.Array.vector
		      structure ArraySlice: MONO_ARRAY_SLICE
					    where type array = S.Array.array
					    where type elem = S.Array.elem
					    where type slice = S.ArraySlice.slice
					    where type vector = S.Array.vector
					    where type vector_slice = S.ArraySlice.vector_slice
		      structure Vector: MONO_VECTOR
					where type elem = S.Array.elem
					where type vector = S.Array.vector
		      structure VectorSlice: MONO_VECTOR_SLICE
					     where type elem = S.Array.elem
					     where type slice = S.ArraySlice.vector_slice
					     where type vector = S.Array.vector
		   end)
   : PRIM_IO
     where type array = S.Array.array
     where type array_slice = S.ArraySlice.slice
     where type elem = S.Array.elem
     where type pos = S.pos
     where type reader = PrimIO.reader
     where type vector = S.Array.vector
     where type vector_slice = S.ArraySlice.vector_slice
     where type writer = PrimIO.writer
functor StreamIO (S: sig
			val someElem: S.Array.elem
			structure Array: MONO_ARRAY
					 where type array = S.Array.array
					 where type elem = S.Array.elem
					 where type vector = S.Array.vector
			structure ArraySlice: MONO_ARRAY_SLICE
					      where type array = S.Array.array
					      where type elem = S.Array.elem
					      where type slice = S.ArraySlice.slice
					      where type vector = S.Array.vector
					      where type vector_slice = S.ArraySlice.vector_slice
			structure PrimIO: PRIM_IO
					  where type array = S.Array.array
					  where type array_slice = S.ArraySlice.slice
					  where type elem = S.Array.elem
					  where type pos = S.PrimIO.pos
					  where type reader = S.PrimIO.reader
					  where type vector = S.Array.vector
					  where type vector_slice = S.ArraySlice.vector_slice
					  where type writer = S.PrimIO.writer
			structure Vector: MONO_VECTOR
					  where type elem = S.Array.elem
					  where type vector = S.Array.vector
			structure VectorSlice: MONO_VECTOR_SLICE
					       where type elem = S.Array.elem
					       where type slice = S.ArraySlice.vector_slice
					       where type vector = S.Array.vector
		     end)
   : STREAM_IO
     where type elem = S.Array.elem
     where type instream = StreamIOExtra.instream
     where type out_pos = StreamIOExtra.out_pos
     where type outstream = StreamIOExtra.outstream
     where type pos = S.PrimIO.pos
     where type reader = S.PrimIO.reader
     where type vector = S.Array.vector
     where type writer = S.PrimIO.writer
signature ARRAY = 
   sig
      eqtype 'a array = 'a array
      eqtype 'a vector = 'a vector
      val all: ('a -> bool) -> 'a array -> bool
      val app: ('a -> unit) -> 'a array -> unit
      val appi: (int * 'a -> unit) -> 'a array -> unit
      val array: int * 'a -> 'a array
      val collate: ('a * 'a -> order) -> 'a array * 'a array -> order
      val copy: {di: int, dst: 'a array, src: 'a array} -> unit
      val copyVec: {di: int, dst: 'a array, src: 'a vector} -> unit
      val exists: ('a -> bool) -> 'a array -> bool
      val find: ('a -> bool) -> 'a array -> 'a option
      val findi: (int * 'a -> bool) -> 'a array -> (int * 'a) option
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
      val fromList: 'a list -> 'a array
      val length: 'a array -> int
      val maxLen: int
      val modify: ('a -> 'a) -> 'a array -> unit
      val modifyi: (int * 'a -> 'a) -> 'a array -> unit
      val sub: 'a array * int -> 'a
      val tabulate: int * (int -> 'a) -> 'a array
      val toList: 'a array -> 'a list
      val update: 'a array * int * 'a -> unit
      val vector: 'a array -> 'a vector
   end
signature ARRAY2 = 
   sig
      eqtype 'a array = 'a ?.array
      eqtype 'a region = {base: 'a ?.array,
			  col: int,
			  ncols: int option,
			  nrows: int option,
			  row: int}
      datatype traversal = RowMajor | ColMajor
      val app: ?.traversal -> ('a -> unit) -> 'a ?.array -> unit
      val appi: ?.traversal
		-> (int * int * 'a -> unit)
		   -> {base: 'a ?.array,
		       col: int,
		       ncols: int option,
		       nrows: int option,
		       row: int}
		      -> unit
      val array: int * int * 'a -> 'a ?.array
      val column: 'a ?.array * int -> 'a vector
      val copy: {dst: 'a ?.array,
		 dst_col: int,
		 dst_row: int,
		 src: {base: 'a ?.array,
		       col: int,
		       ncols: int option,
		       nrows: int option,
		       row: int}}
		-> unit
      val dimensions: 'a ?.array -> int * int
      val fold: ?.traversal -> ('a * 'b -> 'b) -> 'b -> 'a ?.array -> 'b
      val foldi: ?.traversal
		 -> (int * int * 'a * 'b -> 'b)
		    -> 'b
		       -> {base: 'a ?.array,
			   col: int,
			   ncols: int option,
			   nrows: int option,
			   row: int}
			  -> 'b
      val fromList: 'a list list -> 'a ?.array
      val modify: ?.traversal -> ('a -> 'a) -> 'a ?.array -> unit
      val modifyi: ?.traversal
		   -> (int * int * 'a -> 'a)
		      -> {base: 'a ?.array,
			  col: int,
			  ncols: int option,
			  nrows: int option,
			  row: int}
			 -> unit
      val nCols: 'a ?.array -> int
      val nRows: 'a ?.array -> int
      val row: 'a ?.array * int -> 'a vector
      val sub: 'a ?.array * int * int -> 'a
      val tabulate: ?.traversal -> int * int * (int * int -> 'a) -> 'a ?.array
      val update: 'a ?.array * int * int * 'a -> unit
   end
signature ARRAY_SLICE = 
   sig
      type 'a slice = 'a ?.slice
      val all: ('a -> bool) -> 'a ?.slice -> bool
      val app: ('a -> unit) -> 'a ?.slice -> unit
      val appi: (int * 'a -> unit) -> 'a ?.slice -> unit
      val base: 'a ?.slice -> 'a array * int * int
      val collate: ('a * 'a -> order) -> 'a ?.slice * 'a ?.slice -> order
      val copy: {di: int, dst: 'a array, src: 'a ?.slice} -> unit
      val copyVec: {di: int, dst: 'a array, src: 'a VectorSlice.slice} -> unit
      val exists: ('a -> bool) -> 'a ?.slice -> bool
      val find: ('a -> bool) -> 'a ?.slice -> 'a option
      val findi: (int * 'a -> bool) -> 'a ?.slice -> (int * 'a) option
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val full: 'a array -> 'a ?.slice
      val getItem: 'a ?.slice -> ('a * 'a ?.slice) option
      val isEmpty: 'a ?.slice -> bool
      val length: 'a ?.slice -> int
      val modify: ('a -> 'a) -> 'a ?.slice -> unit
      val modifyi: (int * 'a -> 'a) -> 'a ?.slice -> unit
      val slice: 'a array * int * int option -> 'a ?.slice
      val sub: 'a ?.slice * int -> 'a
      val subslice: 'a ?.slice * int * int option -> 'a ?.slice
      val toList: 'a ?.slice -> 'a list
      val update: 'a ?.slice * int * 'a -> unit
      val vector: 'a ?.slice -> 'a vector
   end
signature BIN_IO = 
   sig
      eqtype elem = Word8.word
      type instream = ?.instream
      type outstream = ?.outstream
      eqtype vector = Word8.word vector
      val canInput: ?.instream * int -> int option
      val closeIn: ?.instream -> unit
      val closeOut: ?.outstream -> unit
      val endOfStream: ?.instream -> bool
      val flushOut: ?.outstream -> unit
      val getInstream: ?.instream -> ?.StreamIO.instream
      val getOutstream: ?.outstream -> ?.StreamIO.outstream
      val getPosOut: ?.outstream -> ?.StreamIO.out_pos
      val input: ?.instream -> Word8.word vector
      val input1: ?.instream -> Word8.word option
      val inputAll: ?.instream -> Word8.word vector
      val inputN: ?.instream * int -> Word8.word vector
      val lookahead: ?.instream -> Word8.word option
      val mkInstream: ?.StreamIO.instream -> ?.instream
      val mkOutstream: ?.StreamIO.outstream -> ?.outstream
      val openAppend: string -> ?.outstream
      val openIn: string -> ?.instream
      val openOut: string -> ?.outstream
      val output: ?.outstream * Word8.word vector -> unit
      val output1: ?.outstream * Word8.word -> unit
      val setInstream: ?.instream * ?.StreamIO.instream -> unit
      val setOutstream: ?.outstream * ?.StreamIO.outstream -> unit
      val setPosOut: ?.outstream * ?.StreamIO.out_pos -> unit
      structure StreamIO: STREAM_IO
			  where type elem = Word8.word
			  where type instream = ?.StreamIO.instream
			  where type out_pos = ?.StreamIO.out_pos
			  where type outstream = ?.StreamIO.outstream
			  where type pos = ?.StreamIO.pos
			  where type reader = ?.StreamIO.reader
			  where type vector = Word8.word vector
			  where type writer = ?.StreamIO.writer
   end
signature BIT_FLAGS = 
   sig
      eqtype flags = ?.flags
      val all: ?.flags
      val allSet: ?.flags * ?.flags -> bool
      val anySet: ?.flags * ?.flags -> bool
      val clear: ?.flags * ?.flags -> ?.flags
      val flags: ?.flags list -> ?.flags
      val fromWord: Word64.word -> ?.flags
      val intersect: ?.flags list -> ?.flags
      val toWord: ?.flags -> Word64.word
   end
signature BOOL = 
   sig
      datatype bool = false | true
      val fromString: string -> bool option
      val not: bool -> bool
      val scan: ('a -> (char * 'a) option) -> 'a -> (bool * 'a) option
      val toString: bool -> string
   end
signature BYTE = 
   sig
      val byteToChar: Word8.word -> char
      val bytesToString: Word8.word vector -> string
      val charToByte: char -> Word8.word
      val packString: Word8.word array * int * char VectorSlice.slice -> unit
      val stringToBytes: string -> Word8.word vector
      val unpackString: Word8.word ArraySlice.slice -> string
      val unpackStringVec: Word8.word VectorSlice.slice -> string
   end
signature CHAR = 
   sig
      eqtype char = ?.char
      eqtype string = ?.string
      val < : ?.char * ?.char -> bool
      val <= : ?.char * ?.char -> bool
      val > : ?.char * ?.char -> bool
      val >= : ?.char * ?.char -> bool
      val chr: int -> ?.char
      val compare: ?.char * ?.char -> order
      val contains: ?.string -> ?.char -> bool
      val fromCString: string -> ?.char option
      val fromString: string -> ?.char option
      val isAlpha: ?.char -> bool
      val isAlphaNum: ?.char -> bool
      val isAscii: ?.char -> bool
      val isCntrl: ?.char -> bool
      val isDigit: ?.char -> bool
      val isGraph: ?.char -> bool
      val isHexDigit: ?.char -> bool
      val isLower: ?.char -> bool
      val isPrint: ?.char -> bool
      val isPunct: ?.char -> bool
      val isSpace: ?.char -> bool
      val isUpper: ?.char -> bool
      val maxChar: ?.char
      val maxOrd: int
      val minChar: ?.char
      val notContains: ?.string -> ?.char -> bool
      val ord: ?.char -> int
      val pred: ?.char -> ?.char
      val scan: ('a -> (char * 'a) option) -> 'a -> (?.char * 'a) option
      val succ: ?.char -> ?.char
      val toCString: ?.char -> string
      val toLower: ?.char -> ?.char
      val toString: ?.char -> string
      val toUpper: ?.char -> ?.char
   end
signature COMMAND_LINE = 
   sig
      val arguments: unit -> string list
      val name: unit -> string
   end
signature DATE = 
   sig
      type date = ?.date
      datatype month = Jan
		     | Feb
		     | Mar
		     | Apr
		     | May
		     | Jun
		     | Jul
		     | Aug
		     | Sep
		     | Oct
		     | Nov
		     | Dec
      datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
      exception Date_0
      val compare: ?.date * ?.date -> order
      val date: {day: int,
		 hour: int,
		 minute: int,
		 month: ?.month,
		 offset: Time.time option,
		 second: int,
		 year: int}
		-> ?.date
      val day: ?.date -> int
      val fmt: string -> ?.date -> string
      val fromString: string -> ?.date option
      val fromTimeLocal: Time.time -> ?.date
      val fromTimeUniv: Time.time -> ?.date
      val hour: ?.date -> int
      val isDst: ?.date -> bool option
      val localOffset: unit -> Time.time
      val minute: ?.date -> int
      val month: ?.date -> ?.month
      val offset: ?.date -> Time.time option
      val scan: ('a -> (char * 'a) option) -> 'a -> (?.date * 'a) option
      val second: ?.date -> int
      val toString: ?.date -> string
      val toTime: ?.date -> Time.time
      val weekDay: ?.date -> ?.weekday
      val year: ?.date -> int
      val yearDay: ?.date -> int
   end
signature GENERAL = 
   sig
      type exn = ?.exn
      datatype order = LESS | EQUAL | GREATER
      eqtype unit = ?.unit
      val ! : 'a ref -> 'a
      val := : 'a ref * 'a -> ?.unit
      exception Bind_0
      exception Chr_0
      exception Div_0
      exception Domain_0
      exception Fail_0 of string
      exception Match_0
      exception Overflow_0
      exception Size_0
      exception Span_0
      exception Subscript_0
      val before: 'a * ?.unit -> 'a
      val exnMessage: ?.exn -> string
      val exnName: ?.exn -> string
      val ignore: 'a -> ?.unit
      val o: ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   end
signature GENERIC_SOCK = 
   sig
      val socket: NetHostDB.addr_family * Socket.SOCK.sock_type
		  -> ('a, 'b) Socket.sock
      val socket': NetHostDB.addr_family * Socket.SOCK.sock_type * int
		   -> ('a, 'b) Socket.sock
      val socketPair: NetHostDB.addr_family * Socket.SOCK.sock_type
		      -> ('a, 'b) Socket.sock * ('a, 'b) Socket.sock
      val socketPair': NetHostDB.addr_family * Socket.SOCK.sock_type * int
		       -> ('a, 'b) Socket.sock * ('a, 'b) Socket.sock
   end
signature IEEE_REAL = 
   sig
      eqtype decimal_approx = {class: ?.float_class,
			       digits: int list,
			       exp: int,
			       sign: bool}
      datatype float_class = NAN | INF | ZERO | NORMAL | SUBNORMAL
      datatype real_order = LESS | EQUAL | GREATER | UNORDERED
      datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
      exception Unordered_0
      val fromString: string
		      -> {class: ?.float_class,
			  digits: int list,
			  exp: int,
			  sign: bool} option
      val getRoundingMode: unit -> ?.rounding_mode
      val scan: ('a -> (char * 'a) option)
		-> 'a
		   -> ({class: ?.float_class,
			digits: int list,
			exp: int,
			sign: bool}
		       * 'a) option
      val setRoundingMode: ?.rounding_mode -> unit
      val toString: {class: ?.float_class,
		     digits: int list,
		     exp: int,
		     sign: bool}
		    -> string
   end
signature IMPERATIVE_IO = 
   sig
      type elem = ?.elem
      type instream = ?.instream
      type outstream = ?.outstream
      type vector = ?.vector
      val canInput: ?.instream * int -> int option
      val closeIn: ?.instream -> unit
      val closeOut: ?.outstream -> unit
      val endOfStream: ?.instream -> bool
      val flushOut: ?.outstream -> unit
      val getInstream: ?.instream -> ?.StreamIO.instream
      val getOutstream: ?.outstream -> ?.StreamIO.outstream
      val getPosOut: ?.outstream -> ?.StreamIO.out_pos
      val input: ?.instream -> ?.vector
      val input1: ?.instream -> ?.elem option
      val inputAll: ?.instream -> ?.vector
      val inputN: ?.instream * int -> ?.vector
      val lookahead: ?.instream -> ?.elem option
      val mkInstream: ?.StreamIO.instream -> ?.instream
      val mkOutstream: ?.StreamIO.outstream -> ?.outstream
      val output: ?.outstream * ?.vector -> unit
      val output1: ?.outstream * ?.elem -> unit
      val setInstream: ?.instream * ?.StreamIO.instream -> unit
      val setOutstream: ?.outstream * ?.StreamIO.outstream -> unit
      val setPosOut: ?.outstream * ?.StreamIO.out_pos -> unit
      structure StreamIO: STREAM_IO
			  where type elem = ?.elem
			  where type instream = ?.StreamIO.instream
			  where type out_pos = ?.StreamIO.out_pos
			  where type outstream = ?.StreamIO.outstream
			  where type pos = ?.StreamIO.pos
			  where type reader = ?.StreamIO.reader
			  where type vector = ?.vector
			  where type writer = ?.StreamIO.writer
   end
signature INET_SOCK = 
   sig
      type dgram_sock = (?.inet, Socket.dgram) Socket.sock
      type inet = ?.inet
      type 'a sock = (?.inet, 'a) Socket.sock
      type sock_addr = ?.inet Socket.sock_addr
      type 'a stream_sock = (?.inet, 'a Socket.stream) Socket.sock
      val any: int -> ?.inet Socket.sock_addr
      val fromAddr: ?.inet Socket.sock_addr -> Word8.word vector * int
      val inetAF: NetHostDB.addr_family
      val toAddr: Word8.word vector * int -> ?.inet Socket.sock_addr
      structure TCP:
	 sig
	    val getNODELAY: (?.inet, 'a Socket.stream) Socket.sock -> bool
	    val setNODELAY: (?.inet, 'a Socket.stream) Socket.sock * bool
			    -> unit
	    val socket: unit -> (?.inet, 'a Socket.stream) Socket.sock
	    val socket': int -> (?.inet, 'a Socket.stream) Socket.sock
	 end
      structure UDP:
	 sig
	    val socket: unit -> (?.inet, Socket.dgram) Socket.sock
	    val socket': int -> (?.inet, Socket.dgram) Socket.sock
	 end
   end
signature INTEGER = 
   sig
      eqtype int = ?.int
      val * : ?.int * ?.int -> ?.int
      val + : ?.int * ?.int -> ?.int
      val - : ?.int * ?.int -> ?.int
      val < : ?.int * ?.int -> bool
      val <= : ?.int * ?.int -> bool
      val > : ?.int * ?.int -> bool
      val >= : ?.int * ?.int -> bool
      val abs: ?.int -> ?.int
      val compare: ?.int * ?.int -> order
      val div: ?.int * ?.int -> ?.int
      val fmt: StringCvt.radix -> ?.int -> string
      val fromInt: int -> ?.int
      val fromLarge: IntInf.int -> ?.int
      val fromString: string -> ?.int option
      val max: ?.int * ?.int -> ?.int
      val maxInt: ?.int option
      val min: ?.int * ?.int -> ?.int
      val minInt: ?.int option
      val mod: ?.int * ?.int -> ?.int
      val precision: int option
      val quot: ?.int * ?.int -> ?.int
      val rem: ?.int * ?.int -> ?.int
      val sameSign: ?.int * ?.int -> bool
      val scan: StringCvt.radix
		-> ('a -> (char * 'a) option) -> 'a -> (?.int * 'a) option
      val sign: ?.int -> int
      val toInt: ?.int -> int
      val toLarge: ?.int -> IntInf.int
      val toString: ?.int -> string
      val ~ : ?.int -> ?.int
   end
signature INT_INF = 
   sig
      eqtype int = ?.int
      val * : ?.int * ?.int -> ?.int
      val + : ?.int * ?.int -> ?.int
      val - : ?.int * ?.int -> ?.int
      val < : ?.int * ?.int -> bool
      val << : ?.int * word -> ?.int
      val <= : ?.int * ?.int -> bool
      val > : ?.int * ?.int -> bool
      val >= : ?.int * ?.int -> bool
      val abs: ?.int -> ?.int
      val andb: ?.int * ?.int -> ?.int
      val compare: ?.int * ?.int -> order
      val div: ?.int * ?.int -> ?.int
      val divMod: ?.int * ?.int -> ?.int * ?.int
      val fmt: StringCvt.radix -> ?.int -> string
      val fromInt: int -> ?.int
      val fromLarge: IntInf.int -> ?.int
      val fromString: string -> ?.int option
      val log2: ?.int -> int
      val max: ?.int * ?.int -> ?.int
      val maxInt: ?.int option
      val min: ?.int * ?.int -> ?.int
      val minInt: ?.int option
      val mod: ?.int * ?.int -> ?.int
      val notb: ?.int -> ?.int
      val orb: ?.int * ?.int -> ?.int
      val pow: ?.int * int -> ?.int
      val precision: int option
      val quot: ?.int * ?.int -> ?.int
      val quotRem: ?.int * ?.int -> ?.int * ?.int
      val rem: ?.int * ?.int -> ?.int
      val sameSign: ?.int * ?.int -> bool
      val scan: StringCvt.radix
		-> ('a -> (char * 'a) option) -> 'a -> (?.int * 'a) option
      val sign: ?.int -> int
      val toInt: ?.int -> int
      val toLarge: ?.int -> IntInf.int
      val toString: ?.int -> string
      val xorb: ?.int * ?.int -> ?.int
      val ~ : ?.int -> ?.int
      val ~>> : ?.int * word -> ?.int
   end
signature IO = 
   sig
      datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
      exception BlockingNotSupported_0
      exception ClosedStream_0
      exception Io_0 of {cause: exn, function: string, name: string}
      exception NonblockingNotSupported_0
      exception RandomAccessNotSupported_0
   end
signature LIST = 
   sig
      datatype 'a list = nil | :: of 'a * 'a list
      val @ : 'a list * 'a list -> 'a list
      exception Empty_0
      val all: ('a -> bool) -> 'a list -> bool
      val app: ('a -> unit) -> 'a list -> unit
      val collate: ('a * 'a -> order) -> 'a list * 'a list -> order
      val concat: 'a list list -> 'a list
      val drop: 'a list * int -> 'a list
      val exists: ('a -> bool) -> 'a list -> bool
      val filter: ('a -> bool) -> 'a list -> 'a list
      val find: ('a -> bool) -> 'a list -> 'a option
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
      val getItem: 'a list -> ('a * 'a list) option
      val hd: 'a list -> 'a
      val last: 'a list -> 'a
      val length: 'a list -> int
      val map: ('a -> 'b) -> 'a list -> 'b list
      val mapPartial: ('a -> 'b option) -> 'a list -> 'b list
      val nth: 'a list * int -> 'a
      val null: 'a list -> bool
      val partition: ('a -> bool) -> 'a list -> 'a list * 'a list
      val rev: 'a list -> 'a list
      val revAppend: 'a list * 'a list -> 'a list
      val tabulate: int * (int -> 'a) -> 'a list
      val take: 'a list * int -> 'a list
      val tl: 'a list -> 'a list
   end
signature LIST_PAIR = 
   sig
      exception UnequalLengths_0
      val all: ('a * 'b -> bool) -> 'a list * 'b list -> bool
      val allEq: ('a * 'b -> bool) -> 'a list * 'b list -> bool
      val app: ('a * 'b -> unit) -> 'a list * 'b list -> unit
      val appEq: ('a * 'b -> unit) -> 'a list * 'b list -> unit
      val exists: ('a * 'b -> bool) -> 'a list * 'b list -> bool
      val foldl: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
      val foldlEq: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
      val foldr: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
      val foldrEq: ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
      val map: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
      val mapEq: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
      val unzip: ('a * 'b) list -> 'a list * 'b list
      val zip: 'a list * 'b list -> ('a * 'b) list
      val zipEq: 'a list * 'b list -> ('a * 'b) list
   end
signature MATH = 
   sig
      type real = ?.real
      val acos: ?.real -> ?.real
      val asin: ?.real -> ?.real
      val atan: ?.real -> ?.real
      val atan2: ?.real * ?.real -> ?.real
      val cos: ?.real -> ?.real
      val cosh: ?.real -> ?.real
      val e: ?.real
      val exp: ?.real -> ?.real
      val ln: ?.real -> ?.real
      val log10: ?.real -> ?.real
      val pi: ?.real
      val pow: ?.real * ?.real -> ?.real
      val sin: ?.real -> ?.real
      val sinh: ?.real -> ?.real
      val sqrt: ?.real -> ?.real
      val tan: ?.real -> ?.real
      val tanh: ?.real -> ?.real
   end
signature MLTON = 
   sig
      val debug: bool
      val eq: 'a * 'a -> bool
      val equal: 'a * 'a -> bool
      val hash: 'a -> word
      val isMLton: bool
      val safe: bool
      val share: 'a -> unit
      val shareAll: unit -> unit
      val size: 'a -> int
      structure Array: MLTON_ARRAY
      structure BinIO: MLTON_TEXT_IO
		       where type instream = ?.BinIO.instream
		       where type outstream = ?.BinIO.outstream
      structure CharArray: MLTON_MONO_ARRAY
			   where type elem = ?.CharArray.elem
			   where type t = ?.CharArray.t
      structure CharVector: MLTON_MONO_VECTOR
			    where type elem = ?.CharVector.elem
			    where type t = ?.CharVector.t
      structure Cont: MLTON_CONT
		      where type 'a t = 'a ?.Cont.t
      structure Exn: MLTON_EXN
      structure Finalizable: MLTON_FINALIZABLE
			     where type 'a t = 'a ?.Finalizable.t
      structure GC: MLTON_GC
      structure IntInf: MLTON_INT_INF
			where type BigWord.word = ?.IntInf.BigWord.word
			where type SmallInt.int = ?.IntInf.SmallInt.int
			where type rep = ?.IntInf.rep
			where type t = ?.IntInf.t
      structure Itimer: MLTON_ITIMER
			where type t = ?.Itimer.t
      structure LargeReal: MLTON_REAL
			   where type t = ?.LargeReal.t
      structure LargeWord: MLTON_WORD
			   where type t = ?.LargeWord.t
      structure PCML: MLTON_PCML
		      where type 'a Mailbox.mbox = 'a ?.PCML.Mailbox.mbox
		      where type 'a Multicast.mchan = 'a ?.PCML.Multicast.mchan
		      where type 'a Multicast.port = 'a ?.PCML.Multicast.port
		      where type MutexLock.mutexlock = ?.PCML.MutexLock.mutexlock
		      where type NonBlockingIO.NBTextIO.StreamIO.instream = ?.PCML.NonBlockingIO.NBTextIO.StreamIO.instream
		      where type NonBlockingIO.NBTextIO.StreamIO.out_pos = ?.PCML.NonBlockingIO.NBTextIO.StreamIO.out_pos
		      where type NonBlockingIO.NBTextIO.StreamIO.outstream = ?.PCML.NonBlockingIO.NBTextIO.StreamIO.outstream
		      where type NonBlockingIO.NBTextIO.instream = ?.PCML.NonBlockingIO.NBTextIO.instream
		      where type NonBlockingIO.NBTextIO.outstream = ?.PCML.NonBlockingIO.NBTextIO.outstream
		      where type 'a SyncVar.ivar = 'a ?.PCML.SyncVar.ivar
		      where type 'a SyncVar.mvar = 'a ?.PCML.SyncVar.mvar
		      where type 'a Threadlet.asyncChan = 'a ?.PCML.Threadlet.asyncChan
		      where type 'a chan = 'a ?.PCML.chan
		      where type 'a event = 'a ?.PCML.event
		      where type thread_id = ?.PCML.thread_id
		      where type thread_state = ?.PCML.thread_state
      structure Platform: MLTON_PLATFORM
			  where type Arch.t = ?.Platform.Arch.t
			  where type Format.t = ?.Platform.Format.t
			  where type OS.t = ?.Platform.OS.t
      structure Pointer: MLTON_POINTER
			 where type t = ?.Pointer.t
      structure ProcEnv: MLTON_PROC_ENV
			 where type gid = ?.ProcEnv.gid
      structure Process: MLTON_PROCESS
			 where type ('a, 'b) Child.t = ('a, 'b) ?.Process.Child.t
			 where type ('a, 'b) Param.t = ('a, 'b) ?.Process.Param.t
			 where type any = ?.Process.any
			 where type chain = ?.Process.chain
			 where type input = ?.Process.input
			 where type none = ?.Process.none
			 where type output = ?.Process.output
			 where type pid = ?.Process.pid
			 where type ('a, 'b, 'c) t = ('a, 'b, 'c) ?.Process.t
      structure Profile: MLTON_PROFILE
			 where type Data.t = ?.Profile.Data.t
      structure Random: MLTON_RANDOM
      structure Real: MLTON_REAL
		      where type t = ?.Real.t
      structure Real32:
	 sig
	    type t = ?.Real32.t
	    val castFromWord: word -> ?.Real32.t
	    val castToWord: ?.Real32.t -> word
	    val fromLargeWord: Word64.word -> ?.Real32.t
	    val fromWord: word -> ?.Real32.t
	    val toLargeWord: IEEEReal.rounding_mode -> ?.Real32.t -> Word64.word
	    val toWord: IEEEReal.rounding_mode -> ?.Real32.t -> word
	 end
      structure Real64:
	 sig
	    type t = ?.Real64.t
	    val castFromWord: Word64.word -> real
	    val castToWord: real -> Word64.word
	    val fromLargeWord: Word64.word -> ?.Real64.t
	    val fromWord: word -> ?.Real64.t
	    val toLargeWord: IEEEReal.rounding_mode -> ?.Real64.t -> Word64.word
	    val toWord: IEEEReal.rounding_mode -> ?.Real64.t -> word
	 end
      structure Rlimit: MLTON_RLIMIT
			where type RLim.t = ?.Rlimit.RLim.t
			where type t = ?.Rlimit.t
      structure RunPCML: MLTON_RUN_PCML
      structure Rusage: MLTON_RUSAGE
      structure Signal: MLTON_SIGNAL
			where type Handler.t = ?.Signal.Handler.t
			where type Mask.t = ?.Signal.Mask.t
			where type signal = ?.Signal.signal
      structure Socket: MLTON_SOCKET
			where type t = ?.Socket.t
      structure Syslog: MLTON_SYSLOG
			where type facility = ?.Syslog.facility
			where type loglevel = ?.Syslog.loglevel
			where type openflag = ?.Syslog.openflag
      structure TextIO: MLTON_TEXT_IO
			where type instream = ?.TextIO.instream
			where type outstream = ?.TextIO.outstream
      structure Thread: MLTON_THREAD
			where type AtomicState.t = ?.Thread.AtomicState.t
			where type Runnable.t = ?.Thread.Runnable.t
			where type 'a t = 'a ?.Thread.t
      structure Vector: MLTON_VECTOR
      structure Weak: MLTON_WEAK
		      where type 'a t = 'a ?.Weak.t
      structure Word: MLTON_WORD
		      where type t = ?.Word.t
      structure Word16: MLTON_WORD
			where type t = ?.Word16.t
      structure Word32: MLTON_WORD
			where type t = ?.Word32.t
      structure Word64: MLTON_WORD
			where type t = ?.Word64.t
      structure Word8: MLTON_WORD
		       where type t = ?.Word8.t
      structure Word8Array: MLTON_MONO_ARRAY
			    where type elem = ?.Word8Array.elem
			    where type t = ?.Word8Array.t
      structure Word8Vector: MLTON_MONO_VECTOR
			     where type elem = ?.Word8Vector.elem
			     where type t = ?.Word8Vector.t
      structure World: MLTON_WORLD
		       where type status = ?.World.status
   end
signature MLTON_ARRAY = 
   sig
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b array * 'a
   end
signature MLTON_BIN_IO = 
   sig
      type instream = ?.instream
      type outstream = ?.outstream
      val inFd: ?.instream -> Posix.FileSys.file_desc
      val mkstemp: string -> string * ?.outstream
      val mkstemps: {prefix: string, suffix: string} -> string * ?.outstream
      val newIn: Posix.FileSys.file_desc * string -> ?.instream
      val newOut: Posix.FileSys.file_desc * string -> ?.outstream
      val outFd: ?.outstream -> Posix.FileSys.file_desc
      val tempPrefix: string -> string
   end
signature MLTON_CONT = 
   sig
      type 'a t = 'a ?.t
      val callcc: ('a ?.t -> 'a) -> 'a
      val isolate: ('a -> unit) -> 'a ?.t
      val prepend: 'a ?.t * ('b -> 'a) -> 'b ?.t
      val throw: 'a ?.t * 'a -> 'b
      val throw': 'a ?.t * (unit -> 'a) -> 'b
   end
signature MLTON_EXN = 
   sig
      val addExnMessager: (exn -> string option) -> unit
      val defaultTopLevelHandler: exn -> 'a
      val getTopLevelHandler: unit -> exn -> unit
      val history: exn -> string list
      val setTopLevelHandler: (exn -> unit) -> unit
      val topLevelHandler: exn -> 'a
   end
signature MLTON_FINALIZABLE = 
   sig
      type 'a t = 'a ?.t
      val addFinalizer: 'a ?.t * ('a -> unit) -> unit
      val finalizeBefore: 'a ?.t * 'b ?.t -> unit
      val new: 'a -> 'a ?.t
      val touch: 'a ?.t -> unit
      val withValue: 'a ?.t * ('a -> 'b) -> 'b
   end
signature MLTON_GC = 
   sig
      val collect: unit -> unit
      val pack: unit -> unit
      val setMessages: bool -> unit
      val setSummary: bool -> unit
      val unpack: unit -> unit
      structure Statistics:
	 sig
	    val bytesAllocated: unit -> IntInf.int
	    val lastBytesLive: unit -> IntInf.int
	    val maxBytesLive: unit -> IntInf.int
	    val numCopyingGCs: unit -> IntInf.int
	    val numMarkCompactGCs: unit -> IntInf.int
	    val numMinorGCs: unit -> IntInf.int
	 end
   end
signature MLTON_INT_INF = 
   sig
      datatype rep = Big of ?.BigWord.word vector | Small of ?.SmallInt.int
      type t = ?.t
      val areSmall: ?.t * ?.t -> bool
      val gcd: ?.t * ?.t -> ?.t
      val isSmall: ?.t -> bool
      val rep: ?.t -> ?.rep
      structure BigWord: WORD
			 where type word = ?.BigWord.word
      structure SmallInt: INTEGER
			  where type int = ?.SmallInt.int
   end
signature MLTON_IO = 
   sig
      type instream = ?.instream
      type outstream = ?.outstream
      val inFd: ?.instream -> Posix.FileSys.file_desc
      val mkstemp: string -> string * ?.outstream
      val mkstemps: {prefix: string, suffix: string} -> string * ?.outstream
      val newIn: Posix.FileSys.file_desc * string -> ?.instream
      val newOut: Posix.FileSys.file_desc * string -> ?.outstream
      val outFd: ?.outstream -> Posix.FileSys.file_desc
      val tempPrefix: string -> string
   end
signature MLTON_ITIMER = 
   sig
      datatype t = Prof | Real | Virtual
      val set: ?.t * {interval: Time.time, value: Time.time} -> unit
      val signal: ?.t -> Unix.signal
   end
signature MLTON_MONO_ARRAY = 
   sig
      type elem = ?.elem
      type t = ?.t
      val fromPoly: ?.elem array -> ?.t
      val toPoly: ?.t -> ?.elem array
   end
signature MLTON_MONO_VECTOR = 
   sig
      type elem = ?.elem
      type t = ?.t
      val fromPoly: ?.elem vector -> ?.t
      val toPoly: ?.t -> ?.elem vector
   end
signature MLTON_PCML = 
   sig
      type 'a chan = 'a ?.chan
      type 'a event = 'a ?.event
      type thread_id = ?.thread_id
      type thread_state = ?.thread_state
      val alwaysEvt: 'a -> 'a MLton.PCML.Multicast.event
      val atTimeEvt: Time.time -> unit MLton.PCML.Multicast.event
      val banner: string
      val channel: unit -> 'a ?.chan
      val choose: 'a MLton.PCML.Multicast.event list
		  -> 'a MLton.PCML.Multicast.event
      val compareTid: ?.thread_id * ?.thread_id -> order
      val disablePreemption: unit -> unit
      val enablePreemption: unit -> unit
      val exit: unit -> 'a
      val getTid: unit -> ?.thread_id
      val guard: (unit -> 'a MLton.PCML.Multicast.event)
		 -> 'a MLton.PCML.Multicast.event
      val hashTid: ?.thread_id -> word
      val joinEvt: ?.thread_id -> unit MLton.PCML.Multicast.event
      val never: 'a MLton.PCML.Multicast.event
      val newThreadFlag: unit -> {getFn: unit -> bool, setFn: bool -> unit}
      val newThreadProp: (unit -> 'a)
			 -> {clrFn: unit -> unit,
			     getFn: unit -> 'a,
			     peekFn: unit -> 'a option,
			     setFn: 'a -> unit}
      val recv: 'a ?.chan -> 'a
      val recvEvt: 'a ?.chan -> 'a MLton.PCML.Multicast.event
      val recvPoll: 'a ?.chan -> 'a option
      val sameChannel: 'a ?.chan * 'a ?.chan -> bool
      val sameTid: ?.thread_id * ?.thread_id -> bool
      val select: 'a MLton.PCML.Multicast.event list -> 'a
      val send: 'a ?.chan * 'a -> unit
      val sendEvt: 'a ?.chan * 'a -> unit MLton.PCML.Multicast.event
      val sendPoll: 'a ?.chan * 'a -> bool
      val spawn: (unit -> unit) -> ?.thread_id
      val spawnOnCurProc: (unit -> unit) -> ?.thread_id
      val spawnOnProc: (unit -> unit) * int -> ?.thread_id
      val sync: 'a MLton.PCML.Multicast.event -> 'a
      val tidToInt: ?.thread_id -> int
      val tidToString: ?.thread_id -> string
      val timeOutEvt: Time.time -> unit MLton.PCML.Multicast.event
      val version: {date: string, system: string, version_id: int list}
      val withNack: (unit MLton.PCML.Multicast.event
		     -> 'a MLton.PCML.Multicast.event)
		    -> 'a MLton.PCML.Multicast.event
      val wrap: 'a MLton.PCML.Multicast.event * ('a -> 'b)
		-> 'b MLton.PCML.Multicast.event
      val wrapHandler: 'a MLton.PCML.Multicast.event * (exn -> 'a)
		       -> 'a MLton.PCML.Multicast.event
      val yield: unit -> unit
      structure Aux:
	 sig
	    val numThreadsLive: unit -> int
	    val numberOfProcessors: int
	    val processorNumber: unit -> int
	    val tidMsg: unit -> string
	 end
      structure Mailbox:
	 sig
	    type 'a mbox = 'a ?.Mailbox.mbox
	    val mailbox: unit -> 'a ?.Mailbox.mbox
	    val recv: 'a ?.Mailbox.mbox -> 'a
	    val recvEvt: 'a ?.Mailbox.mbox -> 'a MLton.PCML.Multicast.event
	    val recvPoll: 'a ?.Mailbox.mbox -> 'a option
	    val sameMailbox: 'a ?.Mailbox.mbox * 'a ?.Mailbox.mbox -> bool
	    val send: 'a ?.Mailbox.mbox * 'a -> unit
	 end
      structure Multicast:
	 sig
	    type 'a event = 'a MLton.PCML.Multicast.event
	    type 'a mchan = 'a ?.Multicast.mchan
	    type 'a port = 'a ?.Multicast.port
	    val copy: 'a ?.Multicast.port -> 'a ?.Multicast.port
	    val mChannel: unit -> 'a ?.Multicast.mchan
	    val multicast: 'a ?.Multicast.mchan * 'a -> unit
	    val port: 'a ?.Multicast.mchan -> 'a ?.Multicast.port
	    val recv: 'a ?.Multicast.port -> 'a
	    val recvEvt: 'a ?.Multicast.port -> 'a MLton.PCML.Multicast.event
	 end
      structure MutexLock:
	 sig
	    type mutexlock = ?.MutexLock.mutexlock
	    val fetchAndAdd: int ref * int -> int
	    val getLock: ?.MutexLock.mutexlock -> unit
	    val initLock: unit -> ?.MutexLock.mutexlock
	    val releaseLock: ?.MutexLock.mutexlock -> unit
	 end
      structure NonBlockingIO:
	 sig
	    structure NBTextIO:
	       sig
		  eqtype elem = char
		  type instream = ?.NonBlockingIO.NBTextIO.instream
		  type outstream = ?.NonBlockingIO.NBTextIO.outstream
		  eqtype vector = string
		  val canInput: ?.NonBlockingIO.NBTextIO.instream * int
				-> int option
		  val closeIn: ?.NonBlockingIO.NBTextIO.instream -> unit
		  val closeOut: ?.NonBlockingIO.NBTextIO.outstream -> unit
		  val endOfStream: ?.NonBlockingIO.NBTextIO.instream -> bool
		  val flushOut: ?.NonBlockingIO.NBTextIO.outstream -> unit
		  val getInstream: ?.NonBlockingIO.NBTextIO.instream
				   -> ?.NonBlockingIO.NBTextIO.StreamIO.instream
		  val getOutstream: ?.NonBlockingIO.NBTextIO.outstream
				    -> ?.NonBlockingIO.NBTextIO.StreamIO.outstream
		  val getPosOut: ?.NonBlockingIO.NBTextIO.outstream
				 -> ?.NonBlockingIO.NBTextIO.StreamIO.out_pos
		  val input: ?.NonBlockingIO.NBTextIO.instream -> string
		  val input1: ?.NonBlockingIO.NBTextIO.instream -> char option
		  val inputAll: ?.NonBlockingIO.NBTextIO.instream -> string
		  val inputLine: ?.NonBlockingIO.NBTextIO.instream
				 -> string option
		  val inputN: ?.NonBlockingIO.NBTextIO.instream * int -> string
		  val lookahead: ?.NonBlockingIO.NBTextIO.instream
				 -> char option
		  val mkInstream: ?.NonBlockingIO.NBTextIO.StreamIO.instream
				  -> ?.NonBlockingIO.NBTextIO.instream
		  val mkOutstream: ?.NonBlockingIO.NBTextIO.StreamIO.outstream
				   -> ?.NonBlockingIO.NBTextIO.outstream
		  val mkTextIOProcessor: unit -> unit
		  val openAppend: string -> ?.NonBlockingIO.NBTextIO.outstream
		  val openIn: string -> ?.NonBlockingIO.NBTextIO.instream
		  val openOut: string -> ?.NonBlockingIO.NBTextIO.outstream
		  val openString: string -> ?.NonBlockingIO.NBTextIO.instream
		  val output: ?.NonBlockingIO.NBTextIO.outstream * string
			      -> unit
		  val output1: ?.NonBlockingIO.NBTextIO.outstream * char -> unit
		  val outputSubstr: ?.NonBlockingIO.NBTextIO.outstream
				    * char VectorSlice.slice
				    -> unit
		  val print: string -> unit
		  val scanStream: ((?.NonBlockingIO.NBTextIO.StreamIO.instream
				    -> (char
					* ?.NonBlockingIO.NBTextIO.StreamIO.instream) option)
				   -> ?.NonBlockingIO.NBTextIO.StreamIO.instream
				      -> ('a
					  * ?.NonBlockingIO.NBTextIO.StreamIO.instream) option)
				  -> ?.NonBlockingIO.NBTextIO.instream
				     -> 'a option
		  val setInstream: ?.NonBlockingIO.NBTextIO.instream
				   * ?.NonBlockingIO.NBTextIO.StreamIO.instream
				   -> unit
		  val setOutstream: ?.NonBlockingIO.NBTextIO.outstream
				    * ?.NonBlockingIO.NBTextIO.StreamIO.outstream
				    -> unit
		  val setPosOut: ?.NonBlockingIO.NBTextIO.outstream
				 * ?.NonBlockingIO.NBTextIO.StreamIO.out_pos
				 -> unit
		  val stdErr: ?.NonBlockingIO.NBTextIO.outstream
		  val stdIn: ?.NonBlockingIO.NBTextIO.instream
		  val stdOut: ?.NonBlockingIO.NBTextIO.outstream
		  structure StreamIO: TEXT_STREAM_IO
				      where type instream = ?.NonBlockingIO.NBTextIO.StreamIO.instream
				      where type out_pos = ?.NonBlockingIO.NBTextIO.StreamIO.out_pos
				      where type outstream = ?.NonBlockingIO.NBTextIO.StreamIO.outstream
				      where type pos = Int64.int
				      where type reader = TextPrimIO.reader
				      where type writer = TextPrimIO.writer
	       end
	 end
      structure SimpleRPC:
	 sig
	    type 'a event = 'a MLton.PCML.Multicast.event
	    val mkRPC: ('a -> 'b)
		       -> {call: 'a -> 'b,
			   entryEvt: unit MLton.PCML.Multicast.event}
	    val mkRPC_In: ('a * 'b -> 'c)
			  -> {call: 'a -> 'c,
			      entryEvt: 'b -> unit MLton.PCML.Multicast.event}
	    val mkRPC_InOut: ('a * 'b -> 'c * 'd)
			     -> {call: 'a -> 'c,
				 entryEvt: 'b -> 'd MLton.PCML.Multicast.event}
	    val mkRPC_Out: ('a -> 'b * 'c)
			   -> {call: 'a -> 'b,
			       entryEvt: 'c MLton.PCML.Multicast.event}
	 end
      structure SyncVar:
	 sig
	    type 'a ivar = 'a ?.SyncVar.ivar
	    type 'a mvar = 'a ?.SyncVar.mvar
	    exception Put_0
	    val iGet: 'a ?.SyncVar.ivar -> 'a
	    val iGetEvt: 'a ?.SyncVar.ivar -> 'a MLton.PCML.Multicast.event
	    val iGetPoll: 'a ?.SyncVar.ivar -> 'a option
	    val iPut: 'a ?.SyncVar.ivar * 'a -> unit
	    val iVar: unit -> 'a ?.SyncVar.ivar
	    val mGet: 'a ?.SyncVar.mvar -> 'a
	    val mGetEvt: 'a ?.SyncVar.mvar -> 'a MLton.PCML.Multicast.event
	    val mGetPoll: 'a ?.SyncVar.mvar -> 'a option
	    val mPut: 'a ?.SyncVar.mvar * 'a -> unit
	    val mSwap: 'a ?.SyncVar.mvar * 'a -> 'a
	    val mSwapEvt: 'a ?.SyncVar.mvar * 'a
			  -> 'a MLton.PCML.Multicast.event
	    val mTake: 'a ?.SyncVar.mvar -> 'a
	    val mTakeEvt: 'a ?.SyncVar.mvar -> 'a MLton.PCML.Multicast.event
	    val mTakePoll: 'a ?.SyncVar.mvar -> 'a option
	    val mVar: unit -> 'a ?.SyncVar.mvar
	    val mVarInit: 'a -> 'a ?.SyncVar.mvar
	    val sameIVar: 'a ?.SyncVar.ivar * 'a ?.SyncVar.ivar -> bool
	    val sameMVar: 'a ?.SyncVar.mvar * 'a ?.SyncVar.mvar -> bool
	 end
      structure Threadlet:
	 sig
	    type 'a asyncChan = 'a ?.Threadlet.asyncChan
	    val aRecv: 'a ?.Threadlet.asyncChan -> 'a
	    val aSend: 'a ?.Threadlet.asyncChan * 'a -> unit
	    val async: (unit -> unit) -> unit
	    val dontInline: (unit -> 'a) -> 'a
	    val newAChan: unit -> 'a ?.Threadlet.asyncChan
	    val printFrames: unit -> unit
	 end
   end
signature MLTON_PLATFORM = 
   sig
      structure Arch:
	 sig
	    datatype t = Alpha
		       | AMD64
		       | ARM
		       | HPPA
		       | IA64
		       | m68k
		       | MIPS
		       | PowerPC
		       | S390
		       | Sparc
		       | X86
	    val fromString: string -> ?.Arch.t option
	    val host: ?.Arch.t
	    val toString: ?.Arch.t -> string
	 end
      structure Format:
	 sig
	    datatype t = Archive | Executable | LibArchive | Library
	    val fromString: string -> ?.Format.t option
	    val host: ?.Format.t
	    val toString: ?.Format.t -> string
	 end
      structure OS:
	 sig
	    datatype t = AIX
		       | Cygwin
		       | Darwin
		       | FreeBSD
		       | HPUX
		       | Linux
		       | MinGW
		       | NetBSD
		       | OpenBSD
		       | Solaris
	    val fromString: string -> ?.OS.t option
	    val host: ?.OS.t
	    val toString: ?.OS.t -> string
	 end
   end
signature MLTON_POINTER = 
   sig
      eqtype t = ?.t
      val add: ?.t * word -> ?.t
      val compare: ?.t * ?.t -> order
      val diff: ?.t * ?.t -> word
      val getInt16: ?.t * int -> Int16.int
      val getInt32: ?.t * int -> int
      val getInt64: ?.t * int -> Int64.int
      val getInt8: ?.t * int -> Int8.int
      val getPointer: ?.t * int -> ?.t
      val getReal32: ?.t * int -> Real32.real
      val getReal64: ?.t * int -> real
      val getWord16: ?.t * int -> Word16.word
      val getWord32: ?.t * int -> word
      val getWord64: ?.t * int -> Word64.word
      val getWord8: ?.t * int -> Word8.word
      val null: ?.t
      val setInt16: ?.t * int * Int16.int -> unit
      val setInt32: ?.t * int * int -> unit
      val setInt64: ?.t * int * Int64.int -> unit
      val setInt8: ?.t * int * Int8.int -> unit
      val setPointer: ?.t * int * ?.t -> unit
      val setReal32: ?.t * int * Real32.real -> unit
      val setReal64: ?.t * int * real -> unit
      val setWord16: ?.t * int * Word16.word -> unit
      val setWord32: ?.t * int * word -> unit
      val setWord64: ?.t * int * Word64.word -> unit
      val setWord8: ?.t * int * Word8.word -> unit
      val sizeofPointer: word
      val sub: ?.t * word -> ?.t
   end
signature MLTON_PROCESS = 
   sig
      type any = ?.any
      type chain = ?.chain
      type input = ?.input
      type none = ?.none
      type output = ?.output
      type pid = ?.pid
      type ('a, 'b, 'c) t = ('a, 'b, 'c) ?.t
      exception DoublyRedirected_0
      exception MisuseOfForget_0
      val create: {args: string list,
		   env: string list option,
		   path: string,
		   stderr: ('a, ?.output) ?.Param.t,
		   stdin: ('b, ?.input) ?.Param.t,
		   stdout: ('c, ?.output) ?.Param.t}
		  -> ('b, 'c, 'a) ?.t
      val getStderr: ('a, 'b, 'c) ?.t -> ('c, ?.input) ?.Child.t
      val getStdin: ('a, 'b, 'c) ?.t -> ('a, ?.output) ?.Child.t
      val getStdout: ('a, 'b, 'c) ?.t -> ('b, ?.input) ?.Child.t
      val kill: ('a, 'b, 'c) ?.t * Unix.signal -> unit
      val reap: ('a, 'b, 'c) ?.t -> PosixProcess.exit_status
      val spawn: {args: string list, path: string} -> ?.pid
      val spawne: {args: string list, env: string list, path: string} -> ?.pid
      val spawnp: {args: string list, file: string} -> ?.pid
      structure Child:
	 sig
	    type ('a, 'b) t = ('a, 'b) ?.Child.t
	    val binIn: (BinIO.instream, ?.input) ?.Child.t -> BinIO.instream
	    val binOut: (BinIO.outstream, ?.output) ?.Child.t -> BinIO.outstream
	    val fd: (Posix.FileSys.file_desc, 'a) ?.Child.t
		    -> Posix.FileSys.file_desc
	    val remember: (?.any, 'a) ?.Child.t -> ('b, 'a) ?.Child.t
	    val textIn: (TextIO.instream, ?.input) ?.Child.t -> TextIO.instream
	    val textOut: (TextIO.outstream, ?.output) ?.Child.t
			 -> TextIO.outstream
	 end
      structure Param:
	 sig
	    type ('a, 'b) t = ('a, 'b) ?.Param.t
	    val child: (?.chain, 'a) ?.Child.t -> (?.none, 'a) ?.Param.t
	    val fd: Posix.FileSys.file_desc -> (?.none, 'a) ?.Param.t
	    val file: string -> (?.none, 'a) ?.Param.t
	    val forget: ('a, 'b) ?.Param.t -> (?.any, 'b) ?.Param.t
	    val null: (?.none, 'a) ?.Param.t
	    val pipe: ('a, 'b) ?.Param.t
	    val self: (?.none, 'a) ?.Param.t
	 end
   end
signature MLTON_PROC_ENV = 
   sig
      type gid = ?.gid
      val setenv: {name: string, value: string} -> unit
      val setgroups: ?.gid list -> unit
   end
signature MLTON_PROFILE = 
   sig
      val isOn: bool
      val withData: ?.Data.t * (unit -> 'a) -> 'a
      structure Data:
	 sig
	    type t = ?.Data.t
	    val equals: ?.Data.t * ?.Data.t -> bool
	    val free: ?.Data.t -> unit
	    val malloc: unit -> ?.Data.t
	    val write: ?.Data.t * string -> unit
	 end
   end
signature MLTON_RANDOM = 
   sig
      val alphaNumChar: unit -> char
      val alphaNumString: int -> string
      val rand: unit -> word
      val seed: unit -> word option
      val srand: word -> unit
      val useed: unit -> word option
   end
signature MLTON_REAL = 
   sig
      type t = ?.t
      val fromLargeWord: Word64.word -> ?.t
      val fromWord: word -> ?.t
      val toLargeWord: IEEEReal.rounding_mode -> ?.t -> Word64.word
      val toWord: IEEEReal.rounding_mode -> ?.t -> word
   end
signature MLTON_RLIMIT = 
   sig
      type t = ?.t
      val coreFileSize: ?.t
      val cpuTime: ?.t
      val dataSize: ?.t
      val fileSize: ?.t
      val get: ?.t -> {hard: ?.RLim.t, soft: ?.RLim.t}
      val infinity: ?.RLim.t
      val lockedInMemorySize: ?.t
      val numFiles: ?.t
      val numProcesses: ?.t
      val residentSetSize: ?.t
      val set: ?.t * {hard: ?.RLim.t, soft: ?.RLim.t} -> unit
      val stackSize: ?.t
      val virtualMemorySize: ?.t
      structure RLim:
	 sig
	    type t = ?.RLim.t
	    val castFromSysWord: Word64.word -> ?.RLim.t
	    val castToSysWord: ?.RLim.t -> Word64.word
	 end
   end
signature MLTON_RUN_PCML = 
   sig
      val doit: (unit -> unit) * Time.time option -> OS.Process.status
      val isRunning: unit -> bool
      val shutdown: OS.Process.status -> 'a
   end
signature MLTON_RUSAGE = 
   sig
      eqtype t = {stime: Time.time, utime: Time.time}
      val measureGC: bool -> unit
      val rusage: unit
		  -> {children: {stime: Time.time, utime: Time.time},
		      gc: {stime: Time.time, utime: Time.time},
		      self: {stime: Time.time, utime: Time.time}}
   end
signature MLTON_SIGNAL = 
   sig
      type signal = ?.signal
      type t = ?.signal
      val getHandler: ?.signal -> ?.Handler.t
      val handled: unit -> ?.Mask.t
      val prof: ?.signal
      val restart: bool ref
      val setHandler: ?.signal * ?.Handler.t -> unit
      val suspend: ?.Mask.t -> unit
      val vtalrm: ?.signal
      structure Handler:
	 sig
	    type t = ?.Handler.t
	    val default: ?.Handler.t
	    val handler: (MLton.Thread.Runnable.t -> MLton.Thread.Runnable.t)
			 -> ?.Handler.t
	    val ignore: ?.Handler.t
	    val isDefault: ?.Handler.t -> bool
	    val isIgnore: ?.Handler.t -> bool
	    val simple: (unit -> unit) -> ?.Handler.t
	 end
      structure Mask:
	 sig
	    type t = ?.Mask.t
	    val all: ?.Mask.t
	    val allBut: ?.signal list -> ?.Mask.t
	    val block: ?.Mask.t -> unit
	    val getBlocked: unit -> ?.Mask.t
	    val isMember: ?.Mask.t * ?.signal -> bool
	    val none: ?.Mask.t
	    val setBlocked: ?.Mask.t -> unit
	    val some: ?.signal list -> ?.Mask.t
	    val unblock: ?.Mask.t -> unit
	 end
   end
signature MLTON_SOCKET = 
   sig
      type t = ?.t
      val accept: ?.t
		  -> Word8.word vector
		     * int
		     * TextIO.instream
		     * TextIO.outstream
      val connect: string * int -> TextIO.instream * TextIO.outstream
      val fdToSock: Posix.FileSys.file_desc -> ('a, 'b) Socket.sock
      val listen: unit -> int * ?.t
      val listenAt: int -> ?.t
      val shutdownRead: TextIO.instream -> unit
      val shutdownWrite: TextIO.outstream -> unit
      structure Address:
	 sig
	    eqtype t = Word8.word vector
	    val toVector: Word8.word vector -> Word8.word vector
	 end
      structure Ctl:
	 sig
	    val getERROR: ('a, 'b) Socket.sock
			  -> (string * MkAbsRepEq.t option) option
	 end
      structure Host:
	 sig
	    eqtype t = {name: string}
	    val getByAddress: Word8.word vector -> {name: string} option
	    val getByName: string -> {name: string} option
	 end
      structure Port:
	 sig
	    eqtype t = int
	 end
   end
signature MLTON_SYSLOG = 
   sig
      type facility = ?.facility
      type loglevel = ?.loglevel
      type openflag = ?.openflag
      val ALERT: ?.loglevel
      val AUTHPRIV: ?.facility
      val CONS: ?.openflag
      val CRIT: ?.loglevel
      val CRON: ?.facility
      val DAEMON: ?.facility
      val DEBUG: ?.loglevel
      val EMERG: ?.loglevel
      val ERR: ?.loglevel
      val INFO: ?.loglevel
      val KERN: ?.facility
      val LOCAL0: ?.facility
      val LOCAL1: ?.facility
      val LOCAL2: ?.facility
      val LOCAL3: ?.facility
      val LOCAL4: ?.facility
      val LOCAL5: ?.facility
      val LOCAL6: ?.facility
      val LOCAL7: ?.facility
      val LPR: ?.facility
      val MAIL: ?.facility
      val NDELAY: ?.openflag
      val NEWS: ?.facility
      val NOTICE: ?.loglevel
      val NOWAIT: ?.openflag
      val ODELAY: ?.openflag
      val PERROR: ?.openflag
      val PID: ?.openflag
      val SYSLOG: ?.facility
      val USER: ?.facility
      val UUCP: ?.facility
      val WARNING: ?.loglevel
      val closelog: unit -> unit
      val log: ?.loglevel * string -> unit
      val openlog: string * ?.openflag list * ?.facility -> unit
   end
signature MLTON_TEXT_IO = 
   sig
      type instream = ?.instream
      type outstream = ?.outstream
      val inFd: ?.instream -> Posix.FileSys.file_desc
      val mkstemp: string -> string * ?.outstream
      val mkstemps: {prefix: string, suffix: string} -> string * ?.outstream
      val newIn: Posix.FileSys.file_desc * string -> ?.instream
      val newOut: Posix.FileSys.file_desc * string -> ?.outstream
      val outFd: ?.outstream -> Posix.FileSys.file_desc
      val tempPrefix: string -> string
   end
signature MLTON_THREAD = 
   sig
      type 'a t = 'a ?.t
      val atomicBegin: unit -> unit
      val atomicEnd: unit -> unit
      val atomicState: unit -> ?.AtomicState.t
      val atomicSwitch: ('a ?.t -> ?.Runnable.t) -> 'a
      val atomically: (unit -> 'a) -> 'a
      val new: ('a -> unit) -> 'a ?.t
      val prepare: 'a ?.t * 'a -> ?.Runnable.t
      val prepend: 'a ?.t * ('b -> 'a) -> 'b ?.t
      val switch: ('a ?.t -> ?.Runnable.t) -> 'a
      structure AtomicState:
	 sig
	    datatype t = NonAtomic | Atomic of int
	 end
      structure Runnable:
	 sig
	    type t = ?.Runnable.t
	 end
   end
signature MLTON_VECTOR = 
   sig
      val create: int
		  -> {done: unit -> 'a vector,
		      sub: int -> 'a,
		      update: int * 'a -> unit}
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b vector * 'a
   end
signature MLTON_WEAK = 
   sig
      type 'a t = 'a ?.t
      val get: 'a ?.t -> 'a option
      val new: 'a -> 'a ?.t
   end
signature MLTON_WORD = 
   sig
      type t = ?.t
      val bswap: ?.t -> ?.t
      val rol: ?.t * word -> ?.t
      val ror: ?.t * word -> ?.t
   end
signature MLTON_WORLD = 
   sig
      datatype status = Clone | Original
      val load: string -> 'a
      val save: string -> ?.status
      val saveThread: string * MLton.Thread.Runnable.t -> unit
   end
signature MONO_ARRAY = 
   sig
      eqtype array = ?.array
      type elem = ?.elem
      type vector = ?.vector
      val all: (?.elem -> bool) -> ?.array -> bool
      val app: (?.elem -> unit) -> ?.array -> unit
      val appi: (int * ?.elem -> unit) -> ?.array -> unit
      val array: int * ?.elem -> ?.array
      val collate: (?.elem * ?.elem -> order) -> ?.array * ?.array -> order
      val copy: {di: int, dst: ?.array, src: ?.array} -> unit
      val copyVec: {di: int, dst: ?.array, src: ?.vector} -> unit
      val exists: (?.elem -> bool) -> ?.array -> bool
      val find: (?.elem -> bool) -> ?.array -> ?.elem option
      val findi: (int * ?.elem -> bool) -> ?.array -> (int * ?.elem) option
      val foldl: (?.elem * 'a -> 'a) -> 'a -> ?.array -> 'a
      val foldli: (int * ?.elem * 'a -> 'a) -> 'a -> ?.array -> 'a
      val foldr: (?.elem * 'a -> 'a) -> 'a -> ?.array -> 'a
      val foldri: (int * ?.elem * 'a -> 'a) -> 'a -> ?.array -> 'a
      val fromList: ?.elem list -> ?.array
      val length: ?.array -> int
      val maxLen: int
      val modify: (?.elem -> ?.elem) -> ?.array -> unit
      val modifyi: (int * ?.elem -> ?.elem) -> ?.array -> unit
      val sub: ?.array * int -> ?.elem
      val tabulate: int * (int -> ?.elem) -> ?.array
      val update: ?.array * int * ?.elem -> unit
      val vector: ?.array -> ?.vector
   end
signature MONO_ARRAY2 = 
   sig
      eqtype array = ?.array
      type elem = ?.elem
      eqtype region = {base: ?.array,
		       col: int,
		       ncols: int option,
		       nrows: int option,
		       row: int}
      datatype traversal = RowMajor | ColMajor
      type vector = ?.vector
      val app: Word64Array2.traversal -> (?.elem -> unit) -> ?.array -> unit
      val appi: Word64Array2.traversal
		-> (int * int * ?.elem -> unit)
		   -> {base: ?.array,
		       col: int,
		       ncols: int option,
		       nrows: int option,
		       row: int}
		      -> unit
      val array: int * int * ?.elem -> ?.array
      val column: ?.array * int -> ?.vector
      val copy: {dst: ?.array,
		 dst_col: int,
		 dst_row: int,
		 src: {base: ?.array,
		       col: int,
		       ncols: int option,
		       nrows: int option,
		       row: int}}
		-> unit
      val dimensions: ?.array -> int * int
      val fold: Word64Array2.traversal
		-> (?.elem * 'a -> 'a) -> 'a -> ?.array -> 'a
      val foldi: Word64Array2.traversal
		 -> (int * int * ?.elem * 'a -> 'a)
		    -> 'a
		       -> {base: ?.array,
			   col: int,
			   ncols: int option,
			   nrows: int option,
			   row: int}
			  -> 'a
      val fromList: ?.elem list list -> ?.array
      val modify: Word64Array2.traversal
		  -> (?.elem -> ?.elem) -> ?.array -> unit
      val modifyi: Word64Array2.traversal
		   -> (int * int * ?.elem -> ?.elem)
		      -> {base: ?.array,
			  col: int,
			  ncols: int option,
			  nrows: int option,
			  row: int}
			 -> unit
      val nCols: ?.array -> int
      val nRows: ?.array -> int
      val row: ?.array * int -> ?.vector
      val sub: ?.array * int * int -> ?.elem
      val tabulate: Word64Array2.traversal
		    -> int * int * (int * int -> ?.elem) -> ?.array
      val update: ?.array * int * int * ?.elem -> unit
   end
signature MONO_ARRAY_SLICE = 
   sig
      type array = ?.array
      type elem = ?.elem
      type slice = ?.slice
      type vector = ?.vector
      type vector_slice = ?.vector_slice
      val all: (?.elem -> bool) -> ?.slice -> bool
      val app: (?.elem -> unit) -> ?.slice -> unit
      val appi: (int * ?.elem -> unit) -> ?.slice -> unit
      val base: ?.slice -> ?.array * int * int
      val collate: (?.elem * ?.elem -> order) -> ?.slice * ?.slice -> order
      val copy: {di: int, dst: ?.array, src: ?.slice} -> unit
      val copyVec: {di: int, dst: ?.array, src: ?.vector_slice} -> unit
      val exists: (?.elem -> bool) -> ?.slice -> bool
      val find: (?.elem -> bool) -> ?.slice -> ?.elem option
      val findi: (int * ?.elem -> bool) -> ?.slice -> (int * ?.elem) option
      val foldl: (?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val foldli: (int * ?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val foldr: (?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val foldri: (int * ?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val full: ?.array -> ?.slice
      val getItem: ?.slice -> (?.elem * ?.slice) option
      val isEmpty: ?.slice -> bool
      val length: ?.slice -> int
      val modify: (?.elem -> ?.elem) -> ?.slice -> unit
      val modifyi: (int * ?.elem -> ?.elem) -> ?.slice -> unit
      val slice: ?.array * int * int option -> ?.slice
      val sub: ?.slice * int -> ?.elem
      val subslice: ?.slice * int * int option -> ?.slice
      val update: ?.slice * int * ?.elem -> unit
      val vector: ?.slice -> ?.vector
   end
signature MONO_VECTOR = 
   sig
      type elem = ?.elem
      type vector = ?.vector
      val all: (?.elem -> bool) -> ?.vector -> bool
      val app: (?.elem -> unit) -> ?.vector -> unit
      val appi: (int * ?.elem -> unit) -> ?.vector -> unit
      val collate: (?.elem * ?.elem -> order) -> ?.vector * ?.vector -> order
      val concat: ?.vector list -> ?.vector
      val exists: (?.elem -> bool) -> ?.vector -> bool
      val find: (?.elem -> bool) -> ?.vector -> ?.elem option
      val findi: (int * ?.elem -> bool) -> ?.vector -> (int * ?.elem) option
      val foldl: (?.elem * 'a -> 'a) -> 'a -> ?.vector -> 'a
      val foldli: (int * ?.elem * 'a -> 'a) -> 'a -> ?.vector -> 'a
      val foldr: (?.elem * 'a -> 'a) -> 'a -> ?.vector -> 'a
      val foldri: (int * ?.elem * 'a -> 'a) -> 'a -> ?.vector -> 'a
      val fromList: ?.elem list -> ?.vector
      val length: ?.vector -> int
      val map: (?.elem -> ?.elem) -> ?.vector -> ?.vector
      val mapi: (int * ?.elem -> ?.elem) -> ?.vector -> ?.vector
      val maxLen: int
      val sub: ?.vector * int -> ?.elem
      val tabulate: int * (int -> ?.elem) -> ?.vector
      val update: ?.vector * int * ?.elem -> ?.vector
   end
signature MONO_VECTOR_SLICE = 
   sig
      type elem = ?.elem
      type slice = ?.slice
      type vector = ?.vector
      val all: (?.elem -> bool) -> ?.slice -> bool
      val app: (?.elem -> unit) -> ?.slice -> unit
      val appi: (int * ?.elem -> unit) -> ?.slice -> unit
      val base: ?.slice -> ?.vector * int * int
      val collate: (?.elem * ?.elem -> order) -> ?.slice * ?.slice -> order
      val concat: ?.slice list -> ?.vector
      val exists: (?.elem -> bool) -> ?.slice -> bool
      val find: (?.elem -> bool) -> ?.slice -> ?.elem option
      val findi: (int * ?.elem -> bool) -> ?.slice -> (int * ?.elem) option
      val foldl: (?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val foldli: (int * ?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val foldr: (?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val foldri: (int * ?.elem * 'a -> 'a) -> 'a -> ?.slice -> 'a
      val full: ?.vector -> ?.slice
      val getItem: ?.slice -> (?.elem * ?.slice) option
      val isEmpty: ?.slice -> bool
      val length: ?.slice -> int
      val map: (?.elem -> ?.elem) -> ?.slice -> ?.vector
      val mapi: (int * ?.elem -> ?.elem) -> ?.slice -> ?.vector
      val slice: ?.vector * int * int option -> ?.slice
      val sub: ?.slice * int -> ?.elem
      val subslice: ?.slice * int * int option -> ?.slice
      val vector: ?.slice -> ?.vector
   end
signature NET_HOST_DB = 
   sig
      eqtype addr_family = ?.addr_family
      type entry = ?.entry
      eqtype in_addr = ?.in_addr
      val addr: ?.entry -> ?.in_addr
      val addrType: ?.entry -> ?.addr_family
      val addrs: ?.entry -> ?.in_addr list
      val aliases: ?.entry -> string list
      val fromString: string -> ?.in_addr option
      val getByAddr: ?.in_addr -> ?.entry option
      val getByName: string -> ?.entry option
      val getHostName: unit -> string
      val name: ?.entry -> string
      val scan: ('a -> (char * 'a) option) -> 'a -> (?.in_addr * 'a) option
      val toString: ?.in_addr -> string
   end
signature NET_PROT_DB = 
   sig
      type entry = ?.entry
      val aliases: ?.entry -> string list
      val getByName: string -> ?.entry option
      val getByNumber: int -> ?.entry option
      val name: ?.entry -> string
      val protocol: ?.entry -> int
   end
signature NET_SERV_DB = 
   sig
      type entry = ?.entry
      val aliases: ?.entry -> string list
      val getByName: string * string option -> ?.entry option
      val getByPort: int * string option -> ?.entry option
      val name: ?.entry -> string
      val port: ?.entry -> int
      val protocol: ?.entry -> string
   end
signature OPTION = 
   sig
      datatype 'a option = NONE | SOME of 'a
      exception Option_0
      val app: ('a -> unit) -> 'a ?.option -> unit
      val compose: ('a -> 'b) * ('c -> 'a ?.option) -> 'c -> 'b ?.option
      val composePartial: ('a -> 'b ?.option) * ('c -> 'a ?.option)
			  -> 'c -> 'b ?.option
      val filter: ('a -> bool) -> 'a -> 'a ?.option
      val getOpt: 'a ?.option * 'a -> 'a
      val isSome: 'a ?.option -> bool
      val join: 'a ?.option ?.option -> 'a ?.option
      val map: ('a -> 'b) -> 'a ?.option -> 'b ?.option
      val mapPartial: ('a -> 'b ?.option) -> 'a ?.option -> 'b ?.option
      val valOf: 'a ?.option -> 'a
   end
signature OS = 
   sig
      eqtype syserror = ?.syserror
      exception SysErr_0 of string * ?.syserror option
      val errorMsg: ?.syserror -> string
      val errorName: ?.syserror -> string
      val syserror: string -> ?.syserror option
      structure FileSys: OS_FILE_SYS
			 where type access_mode = ?.FileSys.access_mode
			 where type dirstream = ?.FileSys.dirstream
			 where type file_id = ?.FileSys.file_id
      structure IO: OS_IO
		    where type iodesc = ?.IO.iodesc
		    where type iodesc_kind = ?.IO.iodesc_kind
		    where type poll_desc = ?.IO.poll_desc
		    where type poll_info = ?.IO.poll_info
      structure Path: OS_PATH
      structure Process: OS_PROCESS
			 where type status = ?.Process.status
   end
signature OS_FILE_SYS = 
   sig
      datatype access_mode = A_READ | A_WRITE | A_EXEC
      type dirstream = ?.dirstream
      eqtype file_id = ?.file_id
      val access: string * ?.access_mode list -> bool
      val chDir: string -> unit
      val closeDir: ?.dirstream -> unit
      val compare: ?.file_id * ?.file_id -> order
      val fileId: string -> ?.file_id
      val fileSize: string -> Int64.int
      val fullPath: string -> string
      val getDir: unit -> string
      val hash: ?.file_id -> word
      val isDir: string -> bool
      val isLink: string -> bool
      val mkDir: string -> unit
      val modTime: string -> Time.time
      val openDir: string -> ?.dirstream
      val readDir: ?.dirstream -> string option
      val readLink: string -> string
      val realPath: string -> string
      val remove: string -> unit
      val rename: {new: string, old: string} -> unit
      val rewindDir: ?.dirstream -> unit
      val rmDir: string -> unit
      val setTime: string * Time.time option -> unit
      val tmpName: unit -> string
   end
signature OS_IO = 
   sig
      eqtype iodesc = ?.iodesc
      eqtype iodesc_kind = ?.iodesc_kind
      eqtype poll_desc = ?.poll_desc
      type poll_info = ?.poll_info
      exception Poll_0
      val compare: ?.iodesc * ?.iodesc -> order
      val hash: ?.iodesc -> word
      val infoToPollDesc: ?.poll_info -> ?.poll_desc
      val isIn: ?.poll_info -> bool
      val isOut: ?.poll_info -> bool
      val isPri: ?.poll_info -> bool
      val kind: ?.iodesc -> ?.iodesc_kind
      val poll: ?.poll_desc list * Time.time option -> ?.poll_info list
      val pollDesc: ?.iodesc -> ?.poll_desc option
      val pollIn: ?.poll_desc -> ?.poll_desc
      val pollOut: ?.poll_desc -> ?.poll_desc
      val pollPri: ?.poll_desc -> ?.poll_desc
      val pollToIODesc: ?.poll_desc -> ?.iodesc
      structure Kind:
	 sig
	    val device: ?.iodesc_kind
	    val dir: ?.iodesc_kind
	    val file: ?.iodesc_kind
	    val pipe: ?.iodesc_kind
	    val socket: ?.iodesc_kind
	    val symlink: ?.iodesc_kind
	    val tty: ?.iodesc_kind
	 end
   end
signature OS_PATH = 
   sig
      exception InvalidArc_0
      exception Path_0
      val base: string -> string
      val concat: string * string -> string
      val currentArc: string
      val dir: string -> string
      val ext: string -> string option
      val file: string -> string
      val fromString: string -> {arcs: string list, isAbs: bool, vol: string}
      val fromUnixPath: string -> string
      val getParent: string -> string
      val getVolume: string -> string
      val isAbsolute: string -> bool
      val isCanonical: string -> bool
      val isRelative: string -> bool
      val isRoot: string -> bool
      val joinBaseExt: {base: string, ext: string option} -> string
      val joinDirFile: {dir: string, file: string} -> string
      val mkAbsolute: {path: string, relativeTo: string} -> string
      val mkCanonical: string -> string
      val mkRelative: {path: string, relativeTo: string} -> string
      val parentArc: string
      val splitBaseExt: string -> {base: string, ext: string option}
      val splitDirFile: string -> {dir: string, file: string}
      val toString: {arcs: string list, isAbs: bool, vol: string} -> string
      val toUnixPath: string -> string
      val validVolume: {isAbs: bool, vol: string} -> bool
   end
signature OS_PROCESS = 
   sig
      type status = ?.status
      val atExit: (unit -> unit) -> unit
      val exit: ?.status -> 'a
      val failure: ?.status
      val getEnv: string -> string option
      val isSuccess: ?.status -> bool
      val sleep: Time.time -> unit
      val success: ?.status
      val system: string -> ?.status
      val terminate: ?.status -> 'a
   end
signature PACK_REAL = 
   sig
      type real = ?.real
      val bytesPerElem: int
      val fromBytes: Word8.word vector -> ?.real
      val isBigEndian: bool
      val subArr: Word8.word array * int -> ?.real
      val subVec: Word8.word vector * int -> ?.real
      val toBytes: ?.real -> Word8.word vector
      val update: Word8.word array * int * ?.real -> unit
   end
signature PACK_WORD = 
   sig
      val bytesPerElem: int
      val isBigEndian: bool
      val subArr: Word8.word array * int -> Word64.word
      val subArrX: Word8.word array * int -> Word64.word
      val subVec: Word8.word vector * int -> Word64.word
      val subVecX: Word8.word vector * int -> Word64.word
      val update: Word8.word array * int * Word64.word -> unit
   end
signature POSIX = 
   sig
      structure Error: POSIX_ERROR
		       where type syserror = ?.Error.syserror
      structure FileSys: POSIX_FILE_SYS
			 where type O.flags = ?.FileSys.O.flags
			 where type S.mode = ?.FileSys.S.mode
			 where type ST.stat = ?.FileSys.ST.stat
			 where type access_mode = ?.FileSys.access_mode
			 where type dev = ?.FileSys.dev
			 where type dirstream = ?.FileSys.dirstream
			 where type file_desc = ?.FileSys.file_desc
			 where type gid = ?.FileSys.gid
			 where type ino = ?.FileSys.ino
			 where type open_mode = ?.FileSys.open_mode
			 where type uid = ?.FileSys.uid
      structure IO: POSIX_IO
		    where type FD.flags = ?.IO.FD.flags
		    where type FLock.flock = ?.IO.FLock.flock
		    where type O.flags = ?.IO.O.flags
		    where type file_desc = ?.FileSys.file_desc
		    where type lock_type = ?.IO.lock_type
		    where type open_mode = ?.FileSys.open_mode
		    where type pid = ?.IO.pid
		    where type whence = ?.IO.whence
      structure ProcEnv: POSIX_PROC_ENV
			 where type file_desc = ?.FileSys.file_desc
			 where type gid = ?.FileSys.gid
			 where type pid = ?.IO.pid
			 where type uid = ?.FileSys.uid
      structure Process: POSIX_PROCESS
			 where type W.flags = ?.Process.W.flags
			 where type exit_status = ?.Process.exit_status
			 where type killpid_arg = ?.Process.killpid_arg
			 where type pid = ?.IO.pid
			 where type signal = ?.Process.signal
			 where type waitpid_arg = ?.Process.waitpid_arg
      structure Signal: POSIX_SIGNAL
			where type signal = ?.Process.signal
      structure SysDB: POSIX_SYS_DB
		       where type Group.group = ?.SysDB.Group.group
		       where type Passwd.passwd = ?.SysDB.Passwd.passwd
		       where type gid = ?.FileSys.gid
		       where type uid = ?.FileSys.uid
      structure TTY: POSIX_TTY
		     where type C.flags = ?.TTY.C.flags
		     where type I.flags = ?.TTY.I.flags
		     where type L.flags = ?.TTY.L.flags
		     where type O.flags = ?.TTY.O.flags
		     where type TC.flow_action = ?.TTY.TC.flow_action
		     where type TC.queue_sel = ?.TTY.TC.queue_sel
		     where type TC.set_action = ?.TTY.TC.set_action
		     where type V.cc = ?.TTY.V.cc
		     where type file_desc = ?.FileSys.file_desc
		     where type pid = ?.IO.pid
		     where type speed = ?.TTY.speed
		     where type termios = ?.TTY.termios
   end
signature POSIX_ERROR = 
   sig
      eqtype syserror = ?.syserror
      val acces: ?.syserror
      val again: ?.syserror
      val badf: ?.syserror
      val badmsg: ?.syserror
      val busy: ?.syserror
      val canceled: ?.syserror
      val child: ?.syserror
      val deadlk: ?.syserror
      val dom: ?.syserror
      val errorMsg: ?.syserror -> string
      val errorName: ?.syserror -> string
      val exist: ?.syserror
      val fault: ?.syserror
      val fbig: ?.syserror
      val fromWord: Word64.word -> ?.syserror
      val inprogress: ?.syserror
      val intr: ?.syserror
      val inval: ?.syserror
      val io: ?.syserror
      val isdir: ?.syserror
      val loop: ?.syserror
      val mfile: ?.syserror
      val mlink: ?.syserror
      val msgsize: ?.syserror
      val nametoolong: ?.syserror
      val nfile: ?.syserror
      val nodev: ?.syserror
      val noent: ?.syserror
      val noexec: ?.syserror
      val nolck: ?.syserror
      val nomem: ?.syserror
      val nospc: ?.syserror
      val nosys: ?.syserror
      val notdir: ?.syserror
      val notempty: ?.syserror
      val notsup: ?.syserror
      val notty: ?.syserror
      val nxio: ?.syserror
      val perm: ?.syserror
      val pipe: ?.syserror
      val range: ?.syserror
      val rofs: ?.syserror
      val spipe: ?.syserror
      val srch: ?.syserror
      val syserror: string -> ?.syserror option
      val toWord: ?.syserror -> Word64.word
      val toobig: ?.syserror
      val xdev: ?.syserror
   end
signature POSIX_FILE_SYS = 
   sig
      datatype access_mode = A_READ | A_WRITE | A_EXEC
      eqtype dev = ?.dev
      type dirstream = ?.dirstream
      eqtype file_desc = ?.file_desc
      eqtype gid = ?.gid
      eqtype ino = ?.ino
      datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
      eqtype uid = ?.uid
      val access: string * ?.access_mode list -> bool
      val chdir: string -> unit
      val chmod: string * ?.S.mode -> unit
      val chown: string * ?.uid * ?.gid -> unit
      val closedir: ?.dirstream -> unit
      val creat: string * ?.S.mode -> ?.file_desc
      val createf: string * ?.open_mode * ?.O.flags * ?.S.mode -> ?.file_desc
      val devToWord: ?.dev -> Word64.word
      val fchmod: ?.file_desc * ?.S.mode -> unit
      val fchown: ?.file_desc * ?.uid * ?.gid -> unit
      val fdToIOD: ?.file_desc -> OS.IO.iodesc
      val fdToWord: ?.file_desc -> Word64.word
      val fpathconf: ?.file_desc * string -> Word64.word option
      val fstat: ?.file_desc -> ?.ST.stat
      val ftruncate: ?.file_desc * Int64.int -> unit
      val getcwd: unit -> string
      val inoToWord: ?.ino -> Word64.word
      val iodToFD: OS.IO.iodesc -> ?.file_desc option
      val link: {new: string, old: string} -> unit
      val lstat: string -> ?.ST.stat
      val mkdir: string * ?.S.mode -> unit
      val mkfifo: string * ?.S.mode -> unit
      val opendir: string -> ?.dirstream
      val openf: string * ?.open_mode * ?.O.flags -> ?.file_desc
      val pathconf: string * string -> Word64.word option
      val readdir: ?.dirstream -> string option
      val readlink: string -> string
      val rename: {new: string, old: string} -> unit
      val rewinddir: ?.dirstream -> unit
      val rmdir: string -> unit
      val stat: string -> ?.ST.stat
      val stderr: ?.file_desc
      val stdin: ?.file_desc
      val stdout: ?.file_desc
      val symlink: {new: string, old: string} -> unit
      val umask: ?.S.mode -> ?.S.mode
      val unlink: string -> unit
      val utime: string * {actime: Time.time, modtime: Time.time} option -> unit
      val wordToDev: Word64.word -> ?.dev
      val wordToFD: Word64.word -> ?.file_desc
      val wordToIno: Word64.word -> ?.ino
      structure O:
	 sig
	    eqtype flags = ?.O.flags
	    val all: ?.O.flags
	    val allSet: ?.O.flags * ?.O.flags -> bool
	    val anySet: ?.O.flags * ?.O.flags -> bool
	    val append: ?.O.flags
	    val clear: ?.O.flags * ?.O.flags -> ?.O.flags
	    val excl: ?.O.flags
	    val flags: ?.O.flags list -> ?.O.flags
	    val fromWord: Word64.word -> ?.O.flags
	    val intersect: ?.O.flags list -> ?.O.flags
	    val noctty: ?.O.flags
	    val nonblock: ?.O.flags
	    val sync: ?.O.flags
	    val toWord: ?.O.flags -> Word64.word
	    val trunc: ?.O.flags
	 end
      structure S:
	 sig
	    eqtype flags = ?.S.mode
	    eqtype mode = ?.S.mode
	    val all: ?.S.mode
	    val allSet: ?.S.mode * ?.S.mode -> bool
	    val anySet: ?.S.mode * ?.S.mode -> bool
	    val clear: ?.S.mode * ?.S.mode -> ?.S.mode
	    val flags: ?.S.mode list -> ?.S.mode
	    val fromWord: Word64.word -> ?.S.mode
	    val intersect: ?.S.mode list -> ?.S.mode
	    val irgrp: ?.S.mode
	    val iroth: ?.S.mode
	    val irusr: ?.S.mode
	    val irwxg: ?.S.mode
	    val irwxo: ?.S.mode
	    val irwxu: ?.S.mode
	    val isgid: ?.S.mode
	    val isuid: ?.S.mode
	    val iwgrp: ?.S.mode
	    val iwoth: ?.S.mode
	    val iwusr: ?.S.mode
	    val ixgrp: ?.S.mode
	    val ixoth: ?.S.mode
	    val ixusr: ?.S.mode
	    val toWord: ?.S.mode -> Word64.word
	 end
      structure ST:
	 sig
	    type stat = ?.ST.stat
	    val atime: ?.ST.stat -> Time.time
	    val ctime: ?.ST.stat -> Time.time
	    val dev: ?.ST.stat -> ?.dev
	    val gid: ?.ST.stat -> ?.gid
	    val ino: ?.ST.stat -> ?.ino
	    val isBlk: ?.ST.stat -> bool
	    val isChr: ?.ST.stat -> bool
	    val isDir: ?.ST.stat -> bool
	    val isFIFO: ?.ST.stat -> bool
	    val isLink: ?.ST.stat -> bool
	    val isReg: ?.ST.stat -> bool
	    val isSock: ?.ST.stat -> bool
	    val mode: ?.ST.stat -> ?.S.mode
	    val mtime: ?.ST.stat -> Time.time
	    val nlink: ?.ST.stat -> int
	    val size: ?.ST.stat -> Int64.int
	    val uid: ?.ST.stat -> ?.uid
	 end
   end
signature POSIX_IO = 
   sig
      eqtype file_desc = ?.file_desc
      datatype lock_type = F_RDLCK | F_WRLCK | F_UNLCK
      datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
      eqtype pid = ?.pid
      datatype whence = SEEK_SET | SEEK_CUR | SEEK_END
      val close: ?.file_desc -> unit
      val dup: ?.file_desc -> ?.file_desc
      val dup2: {new: ?.file_desc, old: ?.file_desc} -> unit
      val dupfd: {base: ?.file_desc, old: ?.file_desc} -> ?.file_desc
      val fsync: ?.file_desc -> unit
      val getfd: ?.file_desc -> ?.FD.flags
      val getfl: ?.file_desc -> ?.O.flags * ?.open_mode
      val getlk: ?.file_desc * ?.FLock.flock -> ?.FLock.flock
      val lseek: ?.file_desc * Int64.int * ?.whence -> Int64.int
      val mkBinReader: {fd: ?.file_desc, initBlkMode: bool, name: string}
		       -> BinPrimIO.reader
      val mkBinWriter: {appendMode: bool,
			chunkSize: int,
			fd: ?.file_desc,
			initBlkMode: bool,
			name: string}
		       -> BinPrimIO.writer
      val mkTextReader: {fd: ?.file_desc, initBlkMode: bool, name: string}
			-> TextPrimIO.reader
      val mkTextWriter: {appendMode: bool,
			 chunkSize: int,
			 fd: ?.file_desc,
			 initBlkMode: bool,
			 name: string}
			-> TextPrimIO.writer
      val pipe: unit -> {infd: ?.file_desc, outfd: ?.file_desc}
      val readArr: ?.file_desc * Word8.word ArraySlice.slice -> int
      val readVec: ?.file_desc * int -> Word8.word vector
      val setfd: ?.file_desc * ?.FD.flags -> unit
      val setfl: ?.file_desc * ?.O.flags -> unit
      val setlk: ?.file_desc * ?.FLock.flock -> ?.FLock.flock
      val setlkw: ?.file_desc * ?.FLock.flock -> ?.FLock.flock
      val writeArr: ?.file_desc * Word8.word ArraySlice.slice -> int
      val writeVec: ?.file_desc * Word8.word VectorSlice.slice -> int
      structure FD:
	 sig
	    eqtype flags = ?.FD.flags
	    val all: ?.FD.flags
	    val allSet: ?.FD.flags * ?.FD.flags -> bool
	    val anySet: ?.FD.flags * ?.FD.flags -> bool
	    val clear: ?.FD.flags * ?.FD.flags -> ?.FD.flags
	    val cloexec: ?.FD.flags
	    val flags: ?.FD.flags list -> ?.FD.flags
	    val fromWord: Word64.word -> ?.FD.flags
	    val intersect: ?.FD.flags list -> ?.FD.flags
	    val toWord: ?.FD.flags -> Word64.word
	 end
      structure FLock:
	 sig
	    type flock = ?.FLock.flock
	    val flock: {len: Int64.int,
			ltype: ?.lock_type,
			pid: ?.pid option,
			start: Int64.int,
			whence: ?.whence}
		       -> ?.FLock.flock
	    val len: ?.FLock.flock -> Int64.int
	    val ltype: ?.FLock.flock -> ?.lock_type
	    val pid: ?.FLock.flock -> ?.pid option
	    val start: ?.FLock.flock -> Int64.int
	    val whence: ?.FLock.flock -> ?.whence
	 end
      structure O:
	 sig
	    eqtype flags = ?.O.flags
	    val all: ?.O.flags
	    val allSet: ?.O.flags * ?.O.flags -> bool
	    val anySet: ?.O.flags * ?.O.flags -> bool
	    val append: ?.O.flags
	    val clear: ?.O.flags * ?.O.flags -> ?.O.flags
	    val flags: ?.O.flags list -> ?.O.flags
	    val fromWord: Word64.word -> ?.O.flags
	    val intersect: ?.O.flags list -> ?.O.flags
	    val nonblock: ?.O.flags
	    val sync: ?.O.flags
	    val toWord: ?.O.flags -> Word64.word
	 end
   end
signature POSIX_PROCESS = 
   sig
      datatype exit_status = W_EXITED
			   | W_EXITSTATUS of Word8.word
			   | W_SIGNALED of ?.signal
			   | W_STOPPED of ?.signal
      datatype killpid_arg = K_PROC of ?.pid | K_SAME_GROUP | K_GROUP of ?.pid
      eqtype pid = ?.pid
      eqtype signal = ?.signal
      datatype waitpid_arg = W_ANY_CHILD
			   | W_CHILD of ?.pid | W_SAME_GROUP | W_GROUP of ?.pid
      val alarm: Time.time -> Time.time
      val exec: string * string list -> 'a
      val exece: string * string list * string list -> 'a
      val execp: string * string list -> 'a
      val exit: Word8.word -> 'a
      val fork: unit -> ?.pid option
      val fromStatus: OS.Process.status -> ?.exit_status
      val kill: ?.killpid_arg * ?.signal -> unit
      val pause: unit -> unit
      val pidToWord: ?.pid -> Word64.word
      val raiseSig: ?.signal -> unit
      val sleep: Time.time -> Time.time
      val wait: unit -> ?.pid * ?.exit_status
      val waitpid: ?.waitpid_arg * ?.W.flags list -> ?.pid * ?.exit_status
      val waitpid_nh: ?.waitpid_arg * ?.W.flags list
		      -> (?.pid * ?.exit_status) option
      val wordToPid: Word64.word -> ?.pid
      structure W:
	 sig
	    eqtype flags = ?.W.flags
	    val all: ?.W.flags
	    val allSet: ?.W.flags * ?.W.flags -> bool
	    val anySet: ?.W.flags * ?.W.flags -> bool
	    val clear: ?.W.flags * ?.W.flags -> ?.W.flags
	    val flags: ?.W.flags list -> ?.W.flags
	    val fromWord: Word64.word -> ?.W.flags
	    val intersect: ?.W.flags list -> ?.W.flags
	    val toWord: ?.W.flags -> Word64.word
	    val untraced: ?.W.flags
	 end
   end
signature POSIX_PROC_ENV = 
   sig
      eqtype file_desc = ?.file_desc
      eqtype gid = ?.gid
      eqtype pid = ?.pid
      eqtype uid = ?.uid
      val ctermid: unit -> string
      val environ: unit -> string list
      val getegid: unit -> ?.gid
      val getenv: string -> string option
      val geteuid: unit -> ?.uid
      val getgid: unit -> ?.gid
      val getgroups: unit -> ?.gid list
      val getlogin: unit -> string
      val getpgrp: unit -> ?.pid
      val getpid: unit -> ?.pid
      val getppid: unit -> ?.pid
      val getuid: unit -> ?.uid
      val gidToWord: ?.gid -> Word64.word
      val isatty: ?.file_desc -> bool
      val setgid: ?.gid -> unit
      val setpgid: {pgid: ?.pid option, pid: ?.pid option} -> unit
      val setsid: unit -> ?.pid
      val setuid: ?.uid -> unit
      val sysconf: string -> Word64.word
      val time: unit -> Time.time
      val times: unit
		 -> {cstime: Time.time,
		     cutime: Time.time,
		     elapsed: Time.time,
		     stime: Time.time,
		     utime: Time.time}
      val ttyname: ?.file_desc -> string
      val uidToWord: ?.uid -> Word64.word
      val uname: unit -> (string * string) list
      val wordToGid: Word64.word -> ?.gid
      val wordToUid: Word64.word -> ?.uid
   end
signature POSIX_SIGNAL = 
   sig
      eqtype signal = ?.signal
      val abrt: ?.signal
      val alrm: ?.signal
      val bus: ?.signal
      val chld: ?.signal
      val cont: ?.signal
      val fpe: ?.signal
      val fromWord: Word64.word -> ?.signal
      val hup: ?.signal
      val ill: ?.signal
      val int: ?.signal
      val kill: ?.signal
      val pipe: ?.signal
      val quit: ?.signal
      val segv: ?.signal
      val stop: ?.signal
      val term: ?.signal
      val toWord: ?.signal -> Word64.word
      val tstp: ?.signal
      val ttin: ?.signal
      val ttou: ?.signal
      val usr1: ?.signal
      val usr2: ?.signal
   end
signature POSIX_SYS_DB = 
   sig
      eqtype gid = ?.gid
      eqtype uid = ?.uid
      val getgrgid: ?.gid -> ?.Group.group
      val getgrnam: string -> ?.Group.group
      val getpwnam: string -> ?.Passwd.passwd
      val getpwuid: ?.uid -> ?.Passwd.passwd
      structure Group:
	 sig
	    type group = ?.Group.group
	    val gid: ?.Group.group -> ?.gid
	    val members: ?.Group.group -> string list
	    val name: ?.Group.group -> string
	 end
      structure Passwd:
	 sig
	    type passwd = ?.Passwd.passwd
	    val gid: ?.Passwd.passwd -> ?.gid
	    val home: ?.Passwd.passwd -> string
	    val name: ?.Passwd.passwd -> string
	    val shell: ?.Passwd.passwd -> string
	    val uid: ?.Passwd.passwd -> ?.uid
	 end
   end
signature POSIX_TTY = 
   sig
      eqtype file_desc = ?.file_desc
      eqtype pid = ?.pid
      eqtype speed = ?.speed
      type termios = ?.termios
      val b0: ?.speed
      val b110: ?.speed
      val b1200: ?.speed
      val b134: ?.speed
      val b150: ?.speed
      val b1800: ?.speed
      val b19200: ?.speed
      val b200: ?.speed
      val b2400: ?.speed
      val b300: ?.speed
      val b38400: ?.speed
      val b4800: ?.speed
      val b50: ?.speed
      val b600: ?.speed
      val b75: ?.speed
      val b9600: ?.speed
      val compareSpeed: ?.speed * ?.speed -> order
      val fieldsOf: ?.termios
		    -> {cc: ?.V.cc,
			cflag: ?.C.flags,
			iflag: ?.I.flags,
			ispeed: ?.speed,
			lflag: ?.L.flags,
			oflag: ?.O.flags,
			ospeed: ?.speed}
      val getcc: ?.termios -> ?.V.cc
      val getcflag: ?.termios -> ?.C.flags
      val getiflag: ?.termios -> ?.I.flags
      val getlflag: ?.termios -> ?.L.flags
      val getoflag: ?.termios -> ?.O.flags
      val speedToWord: ?.speed -> Word64.word
      val termios: {cc: ?.V.cc,
		    cflag: ?.C.flags,
		    iflag: ?.I.flags,
		    ispeed: ?.speed,
		    lflag: ?.L.flags,
		    oflag: ?.O.flags,
		    ospeed: ?.speed}
		   -> ?.termios
      val wordToSpeed: Word64.word -> ?.speed
      structure C:
	 sig
	    eqtype flags = ?.C.flags
	    val all: ?.C.flags
	    val allSet: ?.C.flags * ?.C.flags -> bool
	    val anySet: ?.C.flags * ?.C.flags -> bool
	    val clear: ?.C.flags * ?.C.flags -> ?.C.flags
	    val clocal: ?.C.flags
	    val cread: ?.C.flags
	    val cs5: ?.C.flags
	    val cs6: ?.C.flags
	    val cs7: ?.C.flags
	    val cs8: ?.C.flags
	    val csize: ?.C.flags
	    val cstopb: ?.C.flags
	    val flags: ?.C.flags list -> ?.C.flags
	    val fromWord: Word64.word -> ?.C.flags
	    val hupcl: ?.C.flags
	    val intersect: ?.C.flags list -> ?.C.flags
	    val parenb: ?.C.flags
	    val parodd: ?.C.flags
	    val toWord: ?.C.flags -> Word64.word
	 end
      structure CF:
	 sig
	    val getispeed: ?.termios -> ?.speed
	    val getospeed: ?.termios -> ?.speed
	    val setispeed: ?.termios * ?.speed -> ?.termios
	    val setospeed: ?.termios * ?.speed -> ?.termios
	 end
      structure I:
	 sig
	    eqtype flags = ?.I.flags
	    val all: ?.I.flags
	    val allSet: ?.I.flags * ?.I.flags -> bool
	    val anySet: ?.I.flags * ?.I.flags -> bool
	    val brkint: ?.I.flags
	    val clear: ?.I.flags * ?.I.flags -> ?.I.flags
	    val flags: ?.I.flags list -> ?.I.flags
	    val fromWord: Word64.word -> ?.I.flags
	    val icrnl: ?.I.flags
	    val ignbrk: ?.I.flags
	    val igncr: ?.I.flags
	    val ignpar: ?.I.flags
	    val inlcr: ?.I.flags
	    val inpck: ?.I.flags
	    val intersect: ?.I.flags list -> ?.I.flags
	    val istrip: ?.I.flags
	    val ixoff: ?.I.flags
	    val ixon: ?.I.flags
	    val parmrk: ?.I.flags
	    val toWord: ?.I.flags -> Word64.word
	 end
      structure L:
	 sig
	    eqtype flags = ?.L.flags
	    val all: ?.L.flags
	    val allSet: ?.L.flags * ?.L.flags -> bool
	    val anySet: ?.L.flags * ?.L.flags -> bool
	    val clear: ?.L.flags * ?.L.flags -> ?.L.flags
	    val echo: ?.L.flags
	    val echoe: ?.L.flags
	    val echok: ?.L.flags
	    val echonl: ?.L.flags
	    val flags: ?.L.flags list -> ?.L.flags
	    val fromWord: Word64.word -> ?.L.flags
	    val icanon: ?.L.flags
	    val iexten: ?.L.flags
	    val intersect: ?.L.flags list -> ?.L.flags
	    val isig: ?.L.flags
	    val noflsh: ?.L.flags
	    val toWord: ?.L.flags -> Word64.word
	    val tostop: ?.L.flags
	 end
      structure O:
	 sig
	    eqtype flags = ?.O.flags
	    val all: ?.O.flags
	    val allSet: ?.O.flags * ?.O.flags -> bool
	    val anySet: ?.O.flags * ?.O.flags -> bool
	    val clear: ?.O.flags * ?.O.flags -> ?.O.flags
	    val flags: ?.O.flags list -> ?.O.flags
	    val fromWord: Word64.word -> ?.O.flags
	    val intersect: ?.O.flags list -> ?.O.flags
	    val opost: ?.O.flags
	    val toWord: ?.O.flags -> Word64.word
	 end
      structure TC:
	 sig
	    eqtype flow_action = ?.TC.flow_action
	    eqtype queue_sel = ?.TC.queue_sel
	    eqtype set_action = ?.TC.set_action
	    val drain: ?.file_desc -> unit
	    val flow: ?.file_desc * ?.TC.flow_action -> unit
	    val flush: ?.file_desc * ?.TC.queue_sel -> unit
	    val getattr: ?.file_desc -> ?.termios
	    val getpgrp: ?.file_desc -> ?.pid
	    val iflush: ?.TC.queue_sel
	    val ioff: ?.TC.flow_action
	    val ioflush: ?.TC.queue_sel
	    val ion: ?.TC.flow_action
	    val oflush: ?.TC.queue_sel
	    val ooff: ?.TC.flow_action
	    val oon: ?.TC.flow_action
	    val sadrain: ?.TC.set_action
	    val saflush: ?.TC.set_action
	    val sanow: ?.TC.set_action
	    val sendbreak: ?.file_desc * int -> unit
	    val setattr: ?.file_desc * ?.TC.set_action * ?.termios -> unit
	    val setpgrp: ?.file_desc * ?.pid -> unit
	 end
      structure V:
	 sig
	    type cc = ?.V.cc
	    val cc: (int * char) list -> ?.V.cc
	    val eof: int
	    val eol: int
	    val erase: int
	    val intr: int
	    val kill: int
	    val min: int
	    val nccs: int
	    val quit: int
	    val start: int
	    val stop: int
	    val sub: ?.V.cc * int -> char
	    val susp: int
	    val time: int
	    val update: ?.V.cc * (int * char) list -> ?.V.cc
	 end
   end
signature PRIM_IO = 
   sig
      type array = ?.array
      type array_slice = ?.array_slice
      type elem = ?.elem
      eqtype pos = ?.pos
      datatype reader = RD of {avail: unit -> int option,
			       block: (unit -> unit) option,
			       canInput: (unit -> bool) option,
			       chunkSize: int,
			       close: unit -> unit,
			       endPos: (unit -> ?.pos) option,
			       getPos: (unit -> ?.pos) option,
			       ioDesc: OS.IO.iodesc option,
			       name: string,
			       readArr: (?.array_slice -> int) option,
			       readArrNB: (?.array_slice -> int option) option,
			       readVec: (int -> ?.vector) option,
			       readVecNB: (int -> ?.vector option) option,
			       setPos: (?.pos -> unit) option,
			       verifyPos: (unit -> ?.pos) option}
      type vector = ?.vector
      type vector_slice = ?.vector_slice
      datatype writer = WR of {block: (unit -> unit) option,
			       canOutput: (unit -> bool) option,
			       chunkSize: int,
			       close: unit -> unit,
			       endPos: (unit -> ?.pos) option,
			       getPos: (unit -> ?.pos) option,
			       ioDesc: OS.IO.iodesc option,
			       name: string,
			       setPos: (?.pos -> unit) option,
			       verifyPos: (unit -> ?.pos) option,
			       writeArr: (?.array_slice -> int) option,
			       writeArrNB: (?.array_slice -> int option) option,
			       writeVec: (?.vector_slice -> int) option,
			       writeVecNB: (?.vector_slice -> int option) option}
      val augmentReader: ?.reader -> ?.reader
      val augmentWriter: ?.writer -> ?.writer
      val compare: ?.pos * ?.pos -> order
      val nullRd: unit -> ?.reader
      val nullWr: unit -> ?.writer
      val openVector: ?.vector -> ?.reader
   end
signature REAL = 
   sig
      type real = ?.real
      val != : ?.real * ?.real -> bool
      val * : ?.real * ?.real -> ?.real
      val *+ : ?.real * ?.real * ?.real -> ?.real
      val *- : ?.real * ?.real * ?.real -> ?.real
      val + : ?.real * ?.real -> ?.real
      val - : ?.real * ?.real -> ?.real
      val / : ?.real * ?.real -> ?.real
      val < : ?.real * ?.real -> bool
      val <= : ?.real * ?.real -> bool
      val == : ?.real * ?.real -> bool
      val > : ?.real * ?.real -> bool
      val >= : ?.real * ?.real -> bool
      val ?= : ?.real * ?.real -> bool
      val abs: ?.real -> ?.real
      val ceil: ?.real -> int
      val checkFloat: ?.real -> ?.real
      val class: ?.real -> IEEEReal.float_class
      val compare: ?.real * ?.real -> order
      val compareReal: ?.real * ?.real -> IEEEReal.real_order
      val copySign: ?.real * ?.real -> ?.real
      val floor: ?.real -> int
      val fmt: StringCvt.realfmt -> ?.real -> string
      val fromDecimal: {class: IEEEReal.float_class,
			digits: int list,
			exp: int,
			sign: bool}
		       -> ?.real option
      val fromInt: int -> ?.real
      val fromLarge: IEEEReal.rounding_mode -> real -> ?.real
      val fromLargeInt: IntInf.int -> ?.real
      val fromManExp: {exp: int, man: ?.real} -> ?.real
      val fromString: string -> ?.real option
      val isFinite: ?.real -> bool
      val isNan: ?.real -> bool
      val isNormal: ?.real -> bool
      val max: ?.real * ?.real -> ?.real
      val maxFinite: ?.real
      val min: ?.real * ?.real -> ?.real
      val minNormalPos: ?.real
      val minPos: ?.real
      val negInf: ?.real
      val nextAfter: ?.real * ?.real -> ?.real
      val posInf: ?.real
      val precision: int
      val radix: int
      val realCeil: ?.real -> ?.real
      val realFloor: ?.real -> ?.real
      val realMod: ?.real -> ?.real
      val realRound: ?.real -> ?.real
      val realTrunc: ?.real -> ?.real
      val rem: ?.real * ?.real -> ?.real
      val round: ?.real -> int
      val sameSign: ?.real * ?.real -> bool
      val scan: ('a -> (char * 'a) option) -> 'a -> (?.real * 'a) option
      val sign: ?.real -> int
      val signBit: ?.real -> bool
      val split: ?.real -> {frac: ?.real, whole: ?.real}
      val toDecimal: ?.real
		     -> {class: IEEEReal.float_class,
			 digits: int list,
			 exp: int,
			 sign: bool}
      val toInt: IEEEReal.rounding_mode -> ?.real -> int
      val toLarge: ?.real -> real
      val toLargeInt: IEEEReal.rounding_mode -> ?.real -> IntInf.int
      val toManExp: ?.real -> {exp: int, man: ?.real}
      val toString: ?.real -> string
      val trunc: ?.real -> int
      val unordered: ?.real * ?.real -> bool
      val ~ : ?.real -> ?.real
      structure Math: MATH
		      where type real = ?.real
   end
signature SML_OF_NJ = 
   sig
      val exnHistory: exn -> string list
      val exportFn: string * (string * string list -> OS.Process.status) -> unit
      val exportML: string -> bool
      val getAllArgs: unit -> string list
      val getArgs: unit -> string list
      val getCmdName: unit -> string
      structure Cont:
	 sig
	    type 'a cont = 'a ?.Cont.cont
	    val callcc: ('a ?.Cont.cont -> 'a) -> 'a
	    val isolate: ('a -> unit) -> 'a ?.Cont.cont
	    val throw: 'a ?.Cont.cont -> 'a -> 'b
	 end
      structure SysInfo:
	 sig
	    datatype os_kind = BEOS | MACOS | OS2 | UNIX | WIN32
	    exception UNKNOWN_0
	    val getHostArch: unit -> string
	    val getOSKind: unit -> ?.SysInfo.os_kind
	    val getOSName: unit -> string
	 end
   end
signature SOCKET = 
   sig
      type active = ?.active
      type dgram = ?.dgram
      eqtype in_flags = {oob: bool, peek: bool}
      eqtype out_flags = {don't_route: bool, oob: bool}
      type passive = ?.passive
      datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
      type ('a, 'b) sock = ('a, 'b) ?.sock
      type 'a sock_addr = 'a ?.sock_addr
      type sock_desc = ?.sock_desc
      type 'a stream = 'a ?.stream
      val accept: ('a, ?.passive ?.stream) ?.sock
		  -> ('a, ?.active ?.stream) ?.sock * 'a ?.sock_addr
      val acceptNB: ('a, ?.passive ?.stream) ?.sock
		    -> (('a, ?.active ?.stream) ?.sock * 'a ?.sock_addr) option
      val bind: ('a, 'b) ?.sock * 'a ?.sock_addr -> unit
      val close: ('a, 'b) ?.sock -> unit
      val connect: ('a, 'b) ?.sock * 'a ?.sock_addr -> unit
      val connectNB: ('a, 'b) ?.sock * 'a ?.sock_addr -> bool
      val familyOfAddr: 'a ?.sock_addr -> NetHostDB.addr_family
      val ioDesc: ('a, 'b) ?.sock -> OS.IO.iodesc
      val listen: ('a, ?.passive ?.stream) ?.sock * int -> unit
      val recvArr: ('a, ?.active ?.stream) ?.sock * Word8.word ArraySlice.slice
		   -> int
      val recvArr': ('a, ?.active ?.stream) ?.sock
		    * Word8.word ArraySlice.slice
		    * {oob: bool, peek: bool}
		    -> int
      val recvArrFrom: ('a, ?.dgram) ?.sock * Word8.word ArraySlice.slice
		       -> int * 'a ?.sock_addr
      val recvArrFrom': ('a, ?.dgram) ?.sock
			* Word8.word ArraySlice.slice
			* {oob: bool, peek: bool}
			-> int * 'a ?.sock_addr
      val recvArrFromNB: ('a, ?.dgram) ?.sock * Word8.word ArraySlice.slice
			 -> (int * 'a ?.sock_addr) option
      val recvArrFromNB': ('a, ?.dgram) ?.sock
			  * Word8.word ArraySlice.slice
			  * {oob: bool, peek: bool}
			  -> (int * 'a ?.sock_addr) option
      val recvArrNB: ('a, ?.active ?.stream) ?.sock
		     * Word8.word ArraySlice.slice
		     -> int option
      val recvArrNB': ('a, ?.active ?.stream) ?.sock
		      * Word8.word ArraySlice.slice
		      * {oob: bool, peek: bool}
		      -> int option
      val recvVec: ('a, ?.active ?.stream) ?.sock * int -> Word8.word vector
      val recvVec': ('a, ?.active ?.stream) ?.sock
		    * int
		    * {oob: bool, peek: bool}
		    -> Word8.word vector
      val recvVecFrom: ('a, ?.dgram) ?.sock * int
		       -> Word8.word vector * 'a ?.sock_addr
      val recvVecFrom': ('a, ?.dgram) ?.sock * int * {oob: bool, peek: bool}
			-> Word8.word vector * 'a ?.sock_addr
      val recvVecFromNB: ('a, ?.dgram) ?.sock * int
			 -> (Word8.word vector * 'a ?.sock_addr) option
      val recvVecFromNB': ('a, ?.dgram) ?.sock * int * {oob: bool, peek: bool}
			  -> (Word8.word vector * 'a ?.sock_addr) option
      val recvVecNB: ('a, ?.active ?.stream) ?.sock * int
		     -> Word8.word vector option
      val recvVecNB': ('a, ?.active ?.stream) ?.sock
		      * int
		      * {oob: bool, peek: bool}
		      -> Word8.word vector option
      val sameAddr: 'a ?.sock_addr * 'a ?.sock_addr -> bool
      val sameDesc: ?.sock_desc * ?.sock_desc -> bool
      val select: {exs: ?.sock_desc list,
		   rds: ?.sock_desc list,
		   timeout: Time.time option,
		   wrs: ?.sock_desc list}
		  -> {exs: ?.sock_desc list,
		      rds: ?.sock_desc list,
		      wrs: ?.sock_desc list}
      val sendArr: ('a, ?.active ?.stream) ?.sock * Word8.word ArraySlice.slice
		   -> int
      val sendArr': ('a, ?.active ?.stream) ?.sock
		    * Word8.word ArraySlice.slice
		    * {don't_route: bool, oob: bool}
		    -> int
      val sendArrNB: ('a, ?.active ?.stream) ?.sock
		     * Word8.word ArraySlice.slice
		     -> int option
      val sendArrNB': ('a, ?.active ?.stream) ?.sock
		      * Word8.word ArraySlice.slice
		      * {don't_route: bool, oob: bool}
		      -> int option
      val sendArrTo: ('a, ?.dgram) ?.sock
		     * 'a ?.sock_addr
		     * Word8.word ArraySlice.slice
		     -> unit
      val sendArrTo': ('a, ?.dgram) ?.sock
		      * 'a ?.sock_addr
		      * Word8.word ArraySlice.slice
		      * {don't_route: bool, oob: bool}
		      -> unit
      val sendArrToNB: ('a, ?.dgram) ?.sock
		       * 'a ?.sock_addr
		       * Word8.word ArraySlice.slice
		       -> bool
      val sendArrToNB': ('a, ?.dgram) ?.sock
			* 'a ?.sock_addr
			* Word8.word ArraySlice.slice
			* {don't_route: bool, oob: bool}
			-> bool
      val sendVec: ('a, ?.active ?.stream) ?.sock * Word8.word VectorSlice.slice
		   -> int
      val sendVec': ('a, ?.active ?.stream) ?.sock
		    * Word8.word VectorSlice.slice
		    * {don't_route: bool, oob: bool}
		    -> int
      val sendVecNB: ('a, ?.active ?.stream) ?.sock
		     * Word8.word VectorSlice.slice
		     -> int option
      val sendVecNB': ('a, ?.active ?.stream) ?.sock
		      * Word8.word VectorSlice.slice
		      * {don't_route: bool, oob: bool}
		      -> int option
      val sendVecTo: ('a, ?.dgram) ?.sock
		     * 'a ?.sock_addr
		     * Word8.word VectorSlice.slice
		     -> unit
      val sendVecTo': ('a, ?.dgram) ?.sock
		      * 'a ?.sock_addr
		      * Word8.word VectorSlice.slice
		      * {don't_route: bool, oob: bool}
		      -> unit
      val sendVecToNB: ('a, ?.dgram) ?.sock
		       * 'a ?.sock_addr
		       * Word8.word VectorSlice.slice
		       -> bool
      val sendVecToNB': ('a, ?.dgram) ?.sock
			* 'a ?.sock_addr
			* Word8.word VectorSlice.slice
			* {don't_route: bool, oob: bool}
			-> bool
      val shutdown: ('a, 'b ?.stream) ?.sock * ?.shutdown_mode -> unit
      val sockDesc: ('a, 'b) ?.sock -> ?.sock_desc
      structure AF:
	 sig
	    eqtype addr_family = NetHostDB.addr_family
	    val fromString: string -> NetHostDB.addr_family option
	    val list: unit -> (string * NetHostDB.addr_family) list
	    val toString: NetHostDB.addr_family -> string
	 end
      structure Ctl:
	 sig
	    val getATMARK: ('a, ?.active ?.stream) ?.sock -> bool
	    val getBROADCAST: ('a, 'b) ?.sock -> bool
	    val getDEBUG: ('a, 'b) ?.sock -> bool
	    val getDONTROUTE: ('a, 'b) ?.sock -> bool
	    val getERROR: ('a, 'b) ?.sock -> bool
	    val getKEEPALIVE: ('a, 'b) ?.sock -> bool
	    val getLINGER: ('a, 'b) ?.sock -> Time.time option
	    val getNREAD: ('a, 'b) ?.sock -> int
	    val getOOBINLINE: ('a, 'b) ?.sock -> bool
	    val getPeerName: ('a, 'b) ?.sock -> 'a ?.sock_addr
	    val getRCVBUF: ('a, 'b) ?.sock -> int
	    val getREUSEADDR: ('a, 'b) ?.sock -> bool
	    val getSNDBUF: ('a, 'b) ?.sock -> int
	    val getSockName: ('a, 'b) ?.sock -> 'a ?.sock_addr
	    val getTYPE: ('a, 'b) ?.sock -> ?.SOCK.sock_type
	    val setBROADCAST: ('a, 'b) ?.sock * bool -> unit
	    val setDEBUG: ('a, 'b) ?.sock * bool -> unit
	    val setDONTROUTE: ('a, 'b) ?.sock * bool -> unit
	    val setKEEPALIVE: ('a, 'b) ?.sock * bool -> unit
	    val setLINGER: ('a, 'b) ?.sock * Time.time option -> unit
	    val setOOBINLINE: ('a, 'b) ?.sock * bool -> unit
	    val setRCVBUF: ('a, 'b) ?.sock * int -> unit
	    val setREUSEADDR: ('a, 'b) ?.sock * bool -> unit
	    val setSNDBUF: ('a, 'b) ?.sock * int -> unit
	 end
      structure SOCK:
	 sig
	    eqtype sock_type = ?.SOCK.sock_type
	    val dgram: ?.SOCK.sock_type
	    val fromString: string -> ?.SOCK.sock_type option
	    val list: unit -> (string * ?.SOCK.sock_type) list
	    val stream: ?.SOCK.sock_type
	    val toString: ?.SOCK.sock_type -> string
	 end
   end
signature STREAM_IO = 
   sig
      type elem = ?.elem
      type instream = ?.instream
      type out_pos = ?.out_pos
      type outstream = ?.outstream
      type pos = ?.pos
      type reader = ?.reader
      type vector = ?.vector
      type writer = ?.writer
      val canInput: ?.instream * int -> int option
      val closeIn: ?.instream -> unit
      val closeOut: ?.outstream -> unit
      val endOfStream: ?.instream -> bool
      val filePosIn: ?.instream -> ?.pos
      val filePosOut: ?.out_pos -> ?.pos
      val flushOut: ?.outstream -> unit
      val getBufferMode: ?.outstream -> IO.buffer_mode
      val getPosOut: ?.outstream -> ?.out_pos
      val getReader: ?.instream -> ?.reader * ?.vector
      val getWriter: ?.outstream -> ?.writer * IO.buffer_mode
      val input: ?.instream -> ?.vector * ?.instream
      val input1: ?.instream -> (?.elem * ?.instream) option
      val inputAll: ?.instream -> ?.vector * ?.instream
      val inputN: ?.instream * int -> ?.vector * ?.instream
      val mkInstream: ?.reader * ?.vector -> ?.instream
      val mkOutstream: ?.writer * IO.buffer_mode -> ?.outstream
      val output: ?.outstream * ?.vector -> unit
      val output1: ?.outstream * ?.elem -> unit
      val setBufferMode: ?.outstream * IO.buffer_mode -> unit
      val setPosOut: ?.out_pos -> ?.outstream
   end
signature STRING = 
   sig
      eqtype char = ?.char
      eqtype string = ?.string
      val < : ?.string * ?.string -> bool
      val <= : ?.string * ?.string -> bool
      val > : ?.string * ?.string -> bool
      val >= : ?.string * ?.string -> bool
      val ^ : ?.string * ?.string -> ?.string
      val collate: (?.char * ?.char -> order) -> ?.string * ?.string -> order
      val compare: ?.string * ?.string -> order
      val concat: ?.string list -> ?.string
      val concatWith: ?.string -> ?.string list -> ?.string
      val explode: ?.string -> ?.char list
      val extract: ?.string * int * int option -> ?.string
      val fields: (?.char -> bool) -> ?.string -> ?.string list
      val fromCString: string -> ?.string option
      val fromString: string -> ?.string option
      val implode: ?.char list -> ?.string
      val isPrefix: ?.string -> ?.string -> bool
      val isSubstring: ?.string -> ?.string -> bool
      val isSuffix: ?.string -> ?.string -> bool
      val map: (?.char -> ?.char) -> ?.string -> ?.string
      val maxSize: int
      val scan: ('a -> (char * 'a) option) -> 'a -> (?.string * 'a) option
      val size: ?.string -> int
      val str: ?.char -> ?.string
      val sub: ?.string * int -> ?.char
      val substring: ?.string * int * int -> ?.string
      val toCString: ?.string -> string
      val toString: ?.string -> string
      val tokens: (?.char -> bool) -> ?.string -> ?.string list
      val translate: (?.char -> ?.string) -> ?.string -> ?.string
   end
signature STRING_CVT = 
   sig
      type cs = ?.cs
      datatype radix = BIN | OCT | DEC | HEX
      type ('a, 'b) reader = 'b -> ('a * 'b) option
      datatype realfmt = SCI of int option
		       | FIX of int option | GEN of int option | EXACT
      val dropl: (char -> bool) -> ('a -> (char * 'a) option) -> 'a -> 'a
      val padLeft: char -> int -> string -> string
      val padRight: char -> int -> string -> string
      val scanString: ((?.cs -> (char * ?.cs) option)
		       -> ?.cs -> ('a * ?.cs) option)
		      -> string -> 'a option
      val skipWS: ('a -> (char * 'a) option) -> 'a -> 'a
      val splitl: (char -> bool)
		  -> ('a -> (char * 'a) option) -> 'a -> string * 'a
      val takel: (char -> bool) -> ('a -> (char * 'a) option) -> 'a -> string
   end
signature SUBSTRING = 
   sig
      eqtype char = ?.char
      eqtype string = ?.string
      type substring = ?.substring
      val app: (?.char -> unit) -> ?.substring -> unit
      val base: ?.substring -> ?.string * int * int
      val collate: (?.char * ?.char -> order)
		   -> ?.substring * ?.substring -> order
      val compare: ?.substring * ?.substring -> order
      val concat: ?.substring list -> ?.string
      val concatWith: ?.string -> ?.substring list -> ?.string
      val dropl: (?.char -> bool) -> ?.substring -> ?.substring
      val dropr: (?.char -> bool) -> ?.substring -> ?.substring
      val explode: ?.substring -> ?.char list
      val extract: ?.string * int * int option -> ?.substring
      val fields: (?.char -> bool) -> ?.substring -> ?.substring list
      val first: ?.substring -> ?.char option
      val foldl: (?.char * 'a -> 'a) -> 'a -> ?.substring -> 'a
      val foldr: (?.char * 'a -> 'a) -> 'a -> ?.substring -> 'a
      val full: ?.string -> ?.substring
      val getc: ?.substring -> (?.char * ?.substring) option
      val isEmpty: ?.substring -> bool
      val isPrefix: ?.string -> ?.substring -> bool
      val isSubstring: ?.string -> ?.substring -> bool
      val isSuffix: ?.string -> ?.substring -> bool
      val position: ?.string -> ?.substring -> ?.substring * ?.substring
      val size: ?.substring -> int
      val slice: ?.substring * int * int option -> ?.substring
      val span: ?.substring * ?.substring -> ?.substring
      val splitAt: ?.substring * int -> ?.substring * ?.substring
      val splitl: (?.char -> bool) -> ?.substring -> ?.substring * ?.substring
      val splitr: (?.char -> bool) -> ?.substring -> ?.substring * ?.substring
      val string: ?.substring -> ?.string
      val sub: ?.substring * int -> ?.char
      val substring: ?.string * int * int -> ?.substring
      val takel: (?.char -> bool) -> ?.substring -> ?.substring
      val taker: (?.char -> bool) -> ?.substring -> ?.substring
      val tokens: (?.char -> bool) -> ?.substring -> ?.substring list
      val translate: (?.char -> ?.string) -> ?.substring -> ?.string
      val triml: int -> ?.substring -> ?.substring
      val trimr: int -> ?.substring -> ?.substring
   end
signature TEXT = 
   sig
      structure Char: CHAR
		      where type char = ?.Char.char
		      where type string = ?.Char.string
      structure CharArray: MONO_ARRAY
			   where type array = ?.CharArray.array
			   where type elem = ?.Char.char
			   where type vector = ?.Char.string
      structure CharArraySlice: MONO_ARRAY_SLICE
				where type array = ?.CharArray.array
				where type elem = ?.Char.char
				where type slice = ?.CharArraySlice.slice
				where type vector = ?.Char.string
				where type vector_slice = ?.CharArraySlice.vector_slice
      structure CharVector: MONO_VECTOR
			    where type elem = ?.Char.char
			    where type vector = ?.Char.string
      structure CharVectorSlice: MONO_VECTOR_SLICE
				 where type elem = ?.Char.char
				 where type slice = ?.CharArraySlice.vector_slice
				 where type vector = ?.Char.string
      structure String: STRING
			where type char = ?.Char.char
			where type string = ?.Char.string
      structure Substring: SUBSTRING
			   where type char = ?.Char.char
			   where type string = ?.Char.string
			   where type substring = ?.Substring.substring
   end
signature TEXT_IO = 
   sig
      eqtype elem = char
      type instream = ?.instream
      type outstream = ?.outstream
      eqtype vector = string
      val canInput: ?.instream * int -> int option
      val closeIn: ?.instream -> unit
      val closeOut: ?.outstream -> unit
      val endOfStream: ?.instream -> bool
      val flushOut: ?.outstream -> unit
      val getInstream: ?.instream -> ?.StreamIO.instream
      val getOutstream: ?.outstream -> ?.StreamIO.outstream
      val getPosOut: ?.outstream -> ?.StreamIO.out_pos
      val input: ?.instream -> string
      val input1: ?.instream -> char option
      val inputAll: ?.instream -> string
      val inputLine: ?.instream -> string option
      val inputN: ?.instream * int -> string
      val lookahead: ?.instream -> char option
      val mkInstream: ?.StreamIO.instream -> ?.instream
      val mkOutstream: ?.StreamIO.outstream -> ?.outstream
      val openAppend: string -> ?.outstream
      val openIn: string -> ?.instream
      val openOut: string -> ?.outstream
      val openString: string -> ?.instream
      val output: ?.outstream * string -> unit
      val output1: ?.outstream * char -> unit
      val outputSubstr: ?.outstream * char VectorSlice.slice -> unit
      val print: string -> unit
      val scanStream: ((?.StreamIO.instream
			-> (char * ?.StreamIO.instream) option)
		       -> ?.StreamIO.instream
			  -> ('a * ?.StreamIO.instream) option)
		      -> ?.instream -> 'a option
      val setInstream: ?.instream * ?.StreamIO.instream -> unit
      val setOutstream: ?.outstream * ?.StreamIO.outstream -> unit
      val setPosOut: ?.outstream * ?.StreamIO.out_pos -> unit
      val stdErr: ?.outstream
      val stdIn: ?.instream
      val stdOut: ?.outstream
      structure StreamIO: TEXT_STREAM_IO
			  where type instream = ?.StreamIO.instream
			  where type out_pos = ?.StreamIO.out_pos
			  where type outstream = ?.StreamIO.outstream
			  where type pos = Int64.int
			  where type reader = TextPrimIO.reader
			  where type writer = TextPrimIO.writer
   end
signature TEXT_STREAM_IO = 
   sig
      eqtype elem = char
      type instream = ?.instream
      type out_pos = ?.out_pos
      type outstream = ?.outstream
      type pos = ?.pos
      type reader = ?.reader
      eqtype vector = string
      type writer = ?.writer
      val canInput: ?.instream * int -> int option
      val closeIn: ?.instream -> unit
      val closeOut: ?.outstream -> unit
      val endOfStream: ?.instream -> bool
      val filePosIn: ?.instream -> ?.pos
      val filePosOut: ?.out_pos -> ?.pos
      val flushOut: ?.outstream -> unit
      val getBufferMode: ?.outstream -> IO.buffer_mode
      val getPosOut: ?.outstream -> ?.out_pos
      val getReader: ?.instream -> ?.reader * string
      val getWriter: ?.outstream -> ?.writer * IO.buffer_mode
      val input: ?.instream -> string * ?.instream
      val input1: ?.instream -> (char * ?.instream) option
      val inputAll: ?.instream -> string * ?.instream
      val inputLine: ?.instream -> (string * ?.instream) option
      val inputN: ?.instream * int -> string * ?.instream
      val mkInstream: ?.reader * string -> ?.instream
      val mkOutstream: ?.writer * IO.buffer_mode -> ?.outstream
      val output: ?.outstream * string -> unit
      val output1: ?.outstream * char -> unit
      val outputSubstr: ?.outstream * char VectorSlice.slice -> unit
      val setBufferMode: ?.outstream * IO.buffer_mode -> unit
      val setPosOut: ?.out_pos -> ?.outstream
   end
signature TIME = 
   sig
      eqtype time = ?.time
      val + : ?.time * ?.time -> ?.time
      val - : ?.time * ?.time -> ?.time
      val < : ?.time * ?.time -> bool
      val <= : ?.time * ?.time -> bool
      val > : ?.time * ?.time -> bool
      val >= : ?.time * ?.time -> bool
      exception Time_0
      val compare: ?.time * ?.time -> order
      val fmt: int -> ?.time -> string
      val fromMicroseconds: IntInf.int -> ?.time
      val fromMilliseconds: IntInf.int -> ?.time
      val fromNanoseconds: IntInf.int -> ?.time
      val fromReal: real -> ?.time
      val fromSeconds: IntInf.int -> ?.time
      val fromString: string -> ?.time option
      val now: unit -> ?.time
      val scan: ('a -> (char * 'a) option) -> 'a -> (?.time * 'a) option
      val toMicroseconds: ?.time -> IntInf.int
      val toMilliseconds: ?.time -> IntInf.int
      val toNanoseconds: ?.time -> IntInf.int
      val toReal: ?.time -> real
      val toSeconds: ?.time -> IntInf.int
      val toString: ?.time -> string
      val zeroTime: ?.time
   end
signature TIMER = 
   sig
      type cpu_timer = ?.cpu_timer
      type real_timer = ?.real_timer
      val checkCPUTimer: ?.cpu_timer -> {sys: Time.time, usr: Time.time}
      val checkCPUTimes: ?.cpu_timer
			 -> {gc: {sys: Time.time, usr: Time.time},
			     nongc: {sys: Time.time, usr: Time.time}}
      val checkGCTime: ?.cpu_timer -> Time.time
      val checkRealTimer: ?.real_timer -> Time.time
      val startCPUTimer: unit -> ?.cpu_timer
      val startRealTimer: unit -> ?.real_timer
      val totalCPUTimer: unit -> ?.cpu_timer
      val totalRealTimer: unit -> ?.real_timer
   end
signature UNIX = 
   sig
      datatype exit_status = W_EXITED
			   | W_EXITSTATUS of Word8.word
			   | W_SIGNALED of ?.signal
			   | W_STOPPED of ?.signal
      type ('a, 'b) proc = ('a, 'b) ?.proc
      type signal = ?.signal
      val binInstreamOf: (BinIO.instream, 'a) ?.proc -> BinIO.instream
      val binOutstreamOf: ('a, BinIO.outstream) ?.proc -> BinIO.outstream
      val execute: string * string list -> ('a, 'b) ?.proc
      val executeInEnv: string * string list * string list -> ('a, 'b) ?.proc
      val exit: Word8.word -> 'a
      val fromStatus: OS.Process.status -> ?.exit_status
      val kill: ('a, 'b) ?.proc * ?.signal -> unit
      val reap: ('a, 'b) ?.proc -> OS.Process.status
      val streamsOf: (TextIO.instream, TextIO.outstream) ?.proc
		     -> TextIO.instream * TextIO.outstream
      val textInstreamOf: (TextIO.instream, 'a) ?.proc -> TextIO.instream
      val textOutstreamOf: ('a, TextIO.outstream) ?.proc -> TextIO.outstream
   end
signature UNIX_SOCK = 
   sig
      type dgram_sock = (?.unix, Socket.dgram) Socket.sock
      type 'a sock = (?.unix, 'a) Socket.sock
      type sock_addr = ?.unix Socket.sock_addr
      type 'a stream_sock = (?.unix, 'a Socket.stream) Socket.sock
      type unix = ?.unix
      val fromAddr: ?.unix Socket.sock_addr -> string
      val toAddr: string -> ?.unix Socket.sock_addr
      val unixAF: NetHostDB.addr_family
      structure DGrm:
	 sig
	    val socket: unit -> (?.unix, Socket.dgram) Socket.sock
	    val socketPair: unit
			    -> (?.unix, Socket.dgram) Socket.sock
			       * (?.unix, Socket.dgram) Socket.sock
	 end
      structure Strm:
	 sig
	    val socket: unit -> (?.unix, 'a Socket.stream) Socket.sock
	    val socketPair: unit
			    -> (?.unix, 'a Socket.stream) Socket.sock
			       * (?.unix, 'a Socket.stream) Socket.sock
	 end
   end
signature UNSAFE = 
   sig
      structure Array:
	 sig
	    val create: int * 'a -> 'a array
	    val sub: 'a array * int -> 'a
	    val update: 'a array * int * 'a -> unit
	 end
      structure BoolArray:
	 sig
	    type array = ?.BoolArray.array
	    type elem = ?.BoolArray.elem
	    val create: int -> ?.BoolArray.array
	    val sub: ?.BoolArray.array * int -> ?.BoolArray.elem
	    val update: ?.BoolArray.array * int * ?.BoolArray.elem -> unit
	 end
      structure BoolVector:
	 sig
	    type elem = ?.BoolVector.elem
	    type vector = ?.BoolVector.vector
	    val sub: ?.BoolVector.vector * int -> ?.BoolVector.elem
	 end
      structure CharArray:
	 sig
	    type array = ?.CharArray.array
	    type elem = ?.CharArray.elem
	    val create: int -> ?.CharArray.array
	    val sub: ?.CharArray.array * int -> ?.CharArray.elem
	    val update: ?.CharArray.array * int * ?.CharArray.elem -> unit
	 end
      structure CharVector:
	 sig
	    type elem = ?.CharVector.elem
	    type vector = ?.CharVector.vector
	    val sub: ?.CharVector.vector * int -> ?.CharVector.elem
	 end
      structure Int16Array:
	 sig
	    type array = ?.Int16Array.array
	    type elem = ?.Int16Array.elem
	    val create: int -> ?.Int16Array.array
	    val sub: ?.Int16Array.array * int -> ?.Int16Array.elem
	    val update: ?.Int16Array.array * int * ?.Int16Array.elem -> unit
	 end
      structure Int16Vector:
	 sig
	    type elem = ?.Int16Vector.elem
	    type vector = ?.Int16Vector.vector
	    val sub: ?.Int16Vector.vector * int -> ?.Int16Vector.elem
	 end
      structure Int32Array:
	 sig
	    type array = ?.Int32Array.array
	    type elem = ?.Int32Array.elem
	    val create: int -> ?.Int32Array.array
	    val sub: ?.Int32Array.array * int -> ?.Int32Array.elem
	    val update: ?.Int32Array.array * int * ?.Int32Array.elem -> unit
	 end
      structure Int32Vector:
	 sig
	    type elem = ?.Int32Vector.elem
	    type vector = ?.Int32Vector.vector
	    val sub: ?.Int32Vector.vector * int -> ?.Int32Vector.elem
	 end
      structure Int64Array:
	 sig
	    type array = ?.Int64Array.array
	    type elem = ?.Int64Array.elem
	    val create: int -> ?.Int64Array.array
	    val sub: ?.Int64Array.array * int -> ?.Int64Array.elem
	    val update: ?.Int64Array.array * int * ?.Int64Array.elem -> unit
	 end
      structure Int64Vector:
	 sig
	    type elem = ?.Int64Vector.elem
	    type vector = ?.Int64Vector.vector
	    val sub: ?.Int64Vector.vector * int -> ?.Int64Vector.elem
	 end
      structure Int8Array:
	 sig
	    type array = ?.Int8Array.array
	    type elem = ?.Int8Array.elem
	    val create: int -> ?.Int8Array.array
	    val sub: ?.Int8Array.array * int -> ?.Int8Array.elem
	    val update: ?.Int8Array.array * int * ?.Int8Array.elem -> unit
	 end
      structure Int8Vector:
	 sig
	    type elem = ?.Int8Vector.elem
	    type vector = ?.Int8Vector.vector
	    val sub: ?.Int8Vector.vector * int -> ?.Int8Vector.elem
	 end
      structure IntArray:
	 sig
	    type array = ?.IntArray.array
	    type elem = ?.IntArray.elem
	    val create: int -> ?.IntArray.array
	    val sub: ?.IntArray.array * int -> ?.IntArray.elem
	    val update: ?.IntArray.array * int * ?.IntArray.elem -> unit
	 end
      structure IntInfArray:
	 sig
	    type array = ?.IntInfArray.array
	    type elem = ?.IntInfArray.elem
	    val create: int -> ?.IntInfArray.array
	    val sub: ?.IntInfArray.array * int -> ?.IntInfArray.elem
	    val update: ?.IntInfArray.array * int * ?.IntInfArray.elem -> unit
	 end
      structure IntInfVector:
	 sig
	    type elem = ?.IntInfVector.elem
	    type vector = ?.IntInfVector.vector
	    val sub: ?.IntInfVector.vector * int -> ?.IntInfVector.elem
	 end
      structure IntVector:
	 sig
	    type elem = ?.IntVector.elem
	    type vector = ?.IntVector.vector
	    val sub: ?.IntVector.vector * int -> ?.IntVector.elem
	 end
      structure LargeIntArray:
	 sig
	    type array = ?.LargeIntArray.array
	    type elem = ?.LargeIntArray.elem
	    val create: int -> ?.LargeIntArray.array
	    val sub: ?.LargeIntArray.array * int -> ?.LargeIntArray.elem
	    val update: ?.LargeIntArray.array * int * ?.LargeIntArray.elem
			-> unit
	 end
      structure LargeIntVector:
	 sig
	    type elem = ?.LargeIntVector.elem
	    type vector = ?.LargeIntVector.vector
	    val sub: ?.LargeIntVector.vector * int -> ?.LargeIntVector.elem
	 end
      structure LargeRealArray:
	 sig
	    type array = ?.LargeRealArray.array
	    type elem = ?.LargeRealArray.elem
	    val create: int -> ?.LargeRealArray.array
	    val sub: ?.LargeRealArray.array * int -> ?.LargeRealArray.elem
	    val update: ?.LargeRealArray.array * int * ?.LargeRealArray.elem
			-> unit
	 end
      structure LargeRealVector:
	 sig
	    type elem = ?.LargeRealVector.elem
	    type vector = ?.LargeRealVector.vector
	    val sub: ?.LargeRealVector.vector * int -> ?.LargeRealVector.elem
	 end
      structure LargeWordArray:
	 sig
	    type array = ?.LargeWordArray.array
	    type elem = ?.LargeWordArray.elem
	    val create: int -> ?.LargeWordArray.array
	    val sub: ?.LargeWordArray.array * int -> ?.LargeWordArray.elem
	    val update: ?.LargeWordArray.array * int * ?.LargeWordArray.elem
			-> unit
	 end
      structure LargeWordVector:
	 sig
	    type elem = ?.LargeWordVector.elem
	    type vector = ?.LargeWordVector.vector
	    val sub: ?.LargeWordVector.vector * int -> ?.LargeWordVector.elem
	 end
      structure Real32Array:
	 sig
	    type array = ?.Real32Array.array
	    type elem = ?.Real32Array.elem
	    val create: int -> ?.Real32Array.array
	    val sub: ?.Real32Array.array * int -> ?.Real32Array.elem
	    val update: ?.Real32Array.array * int * ?.Real32Array.elem -> unit
	 end
      structure Real32Vector:
	 sig
	    type elem = ?.Real32Vector.elem
	    type vector = ?.Real32Vector.vector
	    val sub: ?.Real32Vector.vector * int -> ?.Real32Vector.elem
	 end
      structure Real64Array:
	 sig
	    type array = ?.Real64Array.array
	    type elem = ?.Real64Array.elem
	    val create: int -> ?.Real64Array.array
	    val sub: ?.Real64Array.array * int -> ?.Real64Array.elem
	    val update: ?.Real64Array.array * int * ?.Real64Array.elem -> unit
	 end
      structure Real64Vector:
	 sig
	    type elem = ?.Real64Vector.elem
	    type vector = ?.Real64Vector.vector
	    val sub: ?.Real64Vector.vector * int -> ?.Real64Vector.elem
	 end
      structure RealArray:
	 sig
	    type array = ?.RealArray.array
	    type elem = ?.RealArray.elem
	    val create: int -> ?.RealArray.array
	    val sub: ?.RealArray.array * int -> ?.RealArray.elem
	    val update: ?.RealArray.array * int * ?.RealArray.elem -> unit
	 end
      structure RealVector:
	 sig
	    type elem = ?.RealVector.elem
	    type vector = ?.RealVector.vector
	    val sub: ?.RealVector.vector * int -> ?.RealVector.elem
	 end
      structure Vector:
	 sig
	    val sub: 'a vector * int -> 'a
	 end
      structure Word16Array:
	 sig
	    type array = ?.Word16Array.array
	    type elem = ?.Word16Array.elem
	    val create: int -> ?.Word16Array.array
	    val sub: ?.Word16Array.array * int -> ?.Word16Array.elem
	    val update: ?.Word16Array.array * int * ?.Word16Array.elem -> unit
	 end
      structure Word16Vector:
	 sig
	    type elem = ?.Word16Vector.elem
	    type vector = ?.Word16Vector.vector
	    val sub: ?.Word16Vector.vector * int -> ?.Word16Vector.elem
	 end
      structure Word32Array:
	 sig
	    type array = ?.Word32Array.array
	    type elem = ?.Word32Array.elem
	    val create: int -> ?.Word32Array.array
	    val sub: ?.Word32Array.array * int -> ?.Word32Array.elem
	    val update: ?.Word32Array.array * int * ?.Word32Array.elem -> unit
	 end
      structure Word32Vector:
	 sig
	    type elem = ?.Word32Vector.elem
	    type vector = ?.Word32Vector.vector
	    val sub: ?.Word32Vector.vector * int -> ?.Word32Vector.elem
	 end
      structure Word64Array:
	 sig
	    type array = ?.Word64Array.array
	    type elem = ?.Word64Array.elem
	    val create: int -> ?.Word64Array.array
	    val sub: ?.Word64Array.array * int -> ?.Word64Array.elem
	    val update: ?.Word64Array.array * int * ?.Word64Array.elem -> unit
	 end
      structure Word64Vector:
	 sig
	    type elem = ?.Word64Vector.elem
	    type vector = ?.Word64Vector.vector
	    val sub: ?.Word64Vector.vector * int -> ?.Word64Vector.elem
	 end
      structure Word8Array:
	 sig
	    type array = ?.Word8Array.array
	    type elem = ?.Word8Array.elem
	    val create: int -> ?.Word8Array.array
	    val sub: ?.Word8Array.array * int -> ?.Word8Array.elem
	    val update: ?.Word8Array.array * int * ?.Word8Array.elem -> unit
	 end
      structure Word8Vector:
	 sig
	    type elem = ?.Word8Vector.elem
	    type vector = ?.Word8Vector.vector
	    val sub: ?.Word8Vector.vector * int -> ?.Word8Vector.elem
	 end
      structure WordArray:
	 sig
	    type array = ?.WordArray.array
	    type elem = ?.WordArray.elem
	    val create: int -> ?.WordArray.array
	    val sub: ?.WordArray.array * int -> ?.WordArray.elem
	    val update: ?.WordArray.array * int * ?.WordArray.elem -> unit
	 end
      structure WordVector:
	 sig
	    type elem = ?.WordVector.elem
	    type vector = ?.WordVector.vector
	    val sub: ?.WordVector.vector * int -> ?.WordVector.elem
	 end
   end
signature VECTOR = 
   sig
      eqtype 'a vector = 'a vector
      val all: ('a -> bool) -> 'a vector -> bool
      val app: ('a -> unit) -> 'a vector -> unit
      val appi: (int * 'a -> unit) -> 'a vector -> unit
      val collate: ('a * 'a -> order) -> 'a vector * 'a vector -> order
      val concat: 'a vector list -> 'a vector
      val exists: ('a -> bool) -> 'a vector -> bool
      val find: ('a -> bool) -> 'a vector -> 'a option
      val findi: (int * 'a -> bool) -> 'a vector -> (int * 'a) option
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
      val fromList: 'a list -> 'a vector
      val length: 'a vector -> int
      val map: ('a -> 'b) -> 'a vector -> 'b vector
      val mapi: (int * 'a -> 'b) -> 'a vector -> 'b vector
      val maxLen: int
      val sub: 'a vector * int -> 'a
      val tabulate: int * (int -> 'a) -> 'a vector
      val update: 'a vector * int * 'a -> 'a vector
   end
signature VECTOR_SLICE = 
   sig
      type 'a slice = 'a ?.slice
      val all: ('a -> bool) -> 'a ?.slice -> bool
      val app: ('a -> unit) -> 'a ?.slice -> unit
      val appi: (int * 'a -> unit) -> 'a ?.slice -> unit
      val base: 'a ?.slice -> 'a vector * int * int
      val collate: ('a * 'a -> order) -> 'a ?.slice * 'a ?.slice -> order
      val concat: 'a ?.slice list -> 'a vector
      val exists: ('a -> bool) -> 'a ?.slice -> bool
      val find: ('a -> bool) -> 'a ?.slice -> 'a option
      val findi: (int * 'a -> bool) -> 'a ?.slice -> (int * 'a) option
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a ?.slice -> 'b
      val full: 'a vector -> 'a ?.slice
      val getItem: 'a ?.slice -> ('a * 'a ?.slice) option
      val isEmpty: 'a ?.slice -> bool
      val length: 'a ?.slice -> int
      val map: ('a -> 'b) -> 'a ?.slice -> 'b vector
      val mapi: (int * 'a -> 'b) -> 'a ?.slice -> 'b vector
      val slice: 'a vector * int * int option -> 'a ?.slice
      val sub: 'a ?.slice * int -> 'a
      val subslice: 'a ?.slice * int * int option -> 'a ?.slice
      val vector: 'a ?.slice -> 'a vector
   end
signature WORD = 
   sig
      eqtype word = ?.word
      val * : ?.word * ?.word -> ?.word
      val + : ?.word * ?.word -> ?.word
      val - : ?.word * ?.word -> ?.word
      val < : ?.word * ?.word -> bool
      val << : ?.word * word -> ?.word
      val <= : ?.word * ?.word -> bool
      val > : ?.word * ?.word -> bool
      val >= : ?.word * ?.word -> bool
      val >> : ?.word * word -> ?.word
      val andb: ?.word * ?.word -> ?.word
      val compare: ?.word * ?.word -> order
      val div: ?.word * ?.word -> ?.word
      val fmt: StringCvt.radix -> ?.word -> string
      val fromInt: int -> ?.word
      val fromLarge: Word64.word -> ?.word
      val fromLargeInt: IntInf.int -> ?.word
      val fromLargeWord: Word64.word -> ?.word
      val fromString: string -> ?.word option
      val max: ?.word * ?.word -> ?.word
      val min: ?.word * ?.word -> ?.word
      val mod: ?.word * ?.word -> ?.word
      val notb: ?.word -> ?.word
      val orb: ?.word * ?.word -> ?.word
      val scan: StringCvt.radix
		-> ('a -> (char * 'a) option) -> 'a -> (?.word * 'a) option
      val toInt: ?.word -> int
      val toIntX: ?.word -> int
      val toLarge: ?.word -> Word64.word
      val toLargeInt: ?.word -> IntInf.int
      val toLargeIntX: ?.word -> IntInf.int
      val toLargeWord: ?.word -> Word64.word
      val toLargeWordX: ?.word -> Word64.word
      val toLargeX: ?.word -> Word64.word
      val toString: ?.word -> string
      val wordSize: int
      val xorb: ?.word * ?.word -> ?.word
      val ~ : ?.word -> ?.word
      val ~>> : ?.word * word -> ?.word
   end
