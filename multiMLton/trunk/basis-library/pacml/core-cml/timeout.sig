signature TIMEOUT =
sig
  val preempt : unit -> unit option
  val reset : unit -> unit
end
