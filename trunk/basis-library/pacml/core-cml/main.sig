signature MAIN =
sig
  val run : (unit -> unit) -> OS.Process.status
  val shutdown : OS.Process.status -> 'a
  val numberOfProcessors : int
end
