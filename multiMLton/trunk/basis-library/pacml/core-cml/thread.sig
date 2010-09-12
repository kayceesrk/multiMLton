signature THREAD =
sig
  val exit : unit -> unit
  val reifyHostFromParasite : RepTypes.parasite -> RepTypes.runnable_host
end
