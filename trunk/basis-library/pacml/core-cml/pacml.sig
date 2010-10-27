signature MLTON_PACML=
sig
  include THREAD_ID
  include THREAD
  include EVENT
  include CHANNEL
  include MAIN
  include TIME_OUT_EXTRA
  structure Lock : LOCK
  structure SyncVar : SYNC_VAR
  structure Mailbox : MAILBOX
  structure Multicast : MULTICAST
  structure SimpleRPC : SIMPLE_RPC
  structure NonBlocking : NON_BLOCKING_EXTRA
end
