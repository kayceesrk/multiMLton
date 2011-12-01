signature MLTON_RCCE =
sig
  val send : 'a * int -> unit
  val recv : int -> 'a
  val wtime : unit -> real
end
