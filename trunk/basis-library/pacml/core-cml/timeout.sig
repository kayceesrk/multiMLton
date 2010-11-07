signature TIME_OUT =
sig
  val timeOutEvt : Time.time -> unit Event.sevt
  val atTimeEvt  : Time.time -> unit Event.sevt
end

signature TIME_OUT_EXTRA =
sig
  include TIME_OUT

  val reset : unit -> unit
  val preempt : unit -> Time.time option option
  val preemptTime : unit -> unit
end
