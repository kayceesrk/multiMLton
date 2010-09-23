structure MLtonPacml : MLTON_PACML=
struct
  open Thread
  open Event
  open Channel
  open Main
  structure SyncVar : SYNC_VAR = SyncVar
  structure Mailbox : MAILBOX = Mailbox
end
