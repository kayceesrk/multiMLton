structure CVar : CVAR =
struct
  structure R = RepTypes

  (* Condition variables are essentially unit valued ivars, and
  * are used for various internal synchronization conditions
  * (e.g., nack events, I/O synchronization, and thread termination).
  *)
  datatype cvar = datatype R.cvar
  datatype cvar_state = datatype R.cvar_state

  fun new () = CVAR (ref (CVAR_unset []))
end
