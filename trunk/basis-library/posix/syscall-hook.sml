(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)


signature SYSCALL_HOOK=
sig
  type decn
  type status
  val startSyscall : unit -> status
  val endSyscall : status -> unit
end

structure SyscallHook=
struct
  datatype decn = STAT | PREE
  type status = decn * string
  val startSyscall = ref (fn () => (STAT,"~999"))
  val endSyscall = ref (fn (f:status) => ())
end
