signature SAFEREF =
  sig
    
    type 'a sref = 'a RepTypes.sref
    val newSR   : 'a -> 'a sref
    val readSR  : 'a sref -> 'a
    val writeSR : ('a * 'a sref) -> unit
    
  end