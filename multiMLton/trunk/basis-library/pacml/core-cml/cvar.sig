signature CVAR =
sig
  datatype cvar = datatype RepTypes.cvar
  datatype cvar_state = datatype RepTypes.cvar_state

  val new : unit -> cvar
end
