structure ParasiteFFI =
struct

  type parasite = RepTypes.parasite
  type primHost = RepTypes.primHost

  structure Debug = LocalDebug(val debug = false)
  open Critical

  fun debug msg = Debug.sayDebug ([atomicMsg], msg)
  fun debug' msg = debug (fn () => msg)

  fun copyParasite i =
  let
    val _ = debug' ("copyParasite")
    val copyParasite = _import "GC_copyParasite" : int -> parasite;
  in
    copyParasite i
  end

  val extractParasiteFromHost = _import "GC_extractParasite" : primHost * int -> parasite;
  val proceedToExtractParasite = _import "GC_proceedToExtract" : primHost * int -> bool;
  val getFrameBottomAsOffset = _import "GC_getFrameBottomAsOffset" : unit -> int;

  fun prefixAndSwitchTo p =
  let
    val _ = debug' ("prefixAndSwitchTo")
  in
    Primitive.MLton.Parasite.prefixAndSwitchTo p
  end

  fun jumpDown b =
  let
    val _ = debug' ("jumpDown")
  in
    Primitive.MLton.Parasite.jumpDown b
  end


end
