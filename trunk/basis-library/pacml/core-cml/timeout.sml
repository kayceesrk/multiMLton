structure Timeout : TIMEOUT =
struct
  fun preempt () = NONE
  fun reset () = ()
end
