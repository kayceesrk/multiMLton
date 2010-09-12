structure ParasiteFFI =
struct

  type parasite = RepTypes.parasite
  type primHost = RepTypes.primHost

  val copyParasite = _import "GC_copyParasite" : int -> parasite;
  val extractParasiteFromHost = _import "GC_extractParasite" : primHost * int -> parasite;
  val proceedToExtractParasite = _import "GC_proceedToExtract" : primHost * int -> bool;
  val getFrameBottomAsOffset = _import "GC_getFrameBottomAsOffset" : unit -> int;
  val prefixAndSwitchTo = Primitive.MLton.Parasite.prefixAndSwitchTo
  val jumpDown = Primitive.MLton.Parasite.jumpDown

end
