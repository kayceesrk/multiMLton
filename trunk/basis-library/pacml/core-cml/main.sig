signature MAIN =
sig
  val run : (unit -> unit) -> OS.Process.status
  val shutdown : OS.Process.status -> 'a
end
