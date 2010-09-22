signature MLTON_PACML=
sig
  include THREAD
  include EVENT
  include CHANNEL
  include MAIN
  structure SyncVar : SYNC_VAR
end
