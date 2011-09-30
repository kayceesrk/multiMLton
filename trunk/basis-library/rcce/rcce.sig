signature MLTON_RCCE =
sig
  val send : 'a * int -> unit
  val recv : int -> 'a
end
