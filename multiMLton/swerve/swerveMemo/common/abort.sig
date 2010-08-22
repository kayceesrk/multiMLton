
(* An abort value is represented by an IVar (SyncVar.ivar). This can
 * be set like a flag, tested to see if it is set and an event is
 * available for when it becomes set.

 * Setting the IVar is useful if it is discovered that a socket
 * connection has broken.  It is important that each connection has
 * its own IVar so that if the IVar is set it only forces an abort on
 * the one connection.

 * For scalability, we only have a single timer implemented in a
 * singleton manager which maps from times to IVars.  The manager counts
 * time in seconds since the start of the manager.  It maintains a list
 * of IVars for each future second where a time-out is needed.

 * IVars that have expired are just let go. They will be collected if
 * their connection has gone away. *)

signature ABORT =
sig
    type t

    (*  The arg is the time-out in seconds. *)
    val create: int -> int -> t

    (*	This never times out. *)
    val never: unit -> t

    (*	This returns an abort event for synchronising. *)
    val evt: t -> unit CML.event

    (*	This tests if the event has occurred. *)
    val aborted: t -> bool

    (*	This forces the abort to happen early even if it is the 'never'
     * condition.*)
    val force: t -> unit

    val getCurr : t -> int
end
