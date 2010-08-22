(* Rebinds the assignemnt operator to monitor when in CML *)
val (op :=) = Stable.monitoredAssign 
