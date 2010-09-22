signature STABLE =
  sig

     val stable: ('a -> 'b) -> ('a -> 'b)
     val memoFix : (('a -> 'b) -> ('a -> 'b)) -> 'a -> 'b

(*
     val stableCP:(('a -> 'b) * (unit -> unit)) -> (('a -> ('b * bool)) *  checkpoint)
     val stabilize : unit -> 'a
     val stabilizeCP : checkpoint -> unit

     val unmonitoredAssign : ('a ref * 'a) -> unit
     val monitoredAssign   : ('a ref * 'a) -> unit*)
  end
