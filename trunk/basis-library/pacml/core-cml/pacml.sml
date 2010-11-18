structure MLtonPacml : MLTON_PACML=
struct
  open Thread
  open Event
  open Channel
  open Lock
  open Main
  open Timeout
  structure MutexLock : MUTEX_LOCK = MutexLock
  structure SyncVar : SYNC_VAR = SyncVar
  structure Mailbox : MAILBOX = Mailbox
  structure Multicast : MULTICAST = Multicast
  structure SimpleRPC : SIMPLE_RPC = SimpleRPC
  structure NonBlocking : NON_BLOCKING_EXTRA = NonBlocking
end
