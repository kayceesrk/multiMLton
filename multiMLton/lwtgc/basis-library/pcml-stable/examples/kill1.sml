(**
 * Check that a thread spawned in a stable section is killed
 * when that section is unrolled.
 *
 *    | tMain    | t0
 * ---+----------+---
 * 01:  enterSS
 * 02:  spawn
 * 03:  stabilize
 * ----------------------------------------
 * Now tMain should restore to line 01
 * and t0 should be killed (look in "Api - tids2Kill = 1")
 **)

(***** CML BOILERPLATE *****)

structure CML = MLton.PCML
val _ = run (fn() => let
(***** START CODE *****)

fun t0 () = error "t0 should never run"

fun f() = CML.spawn(t0)

val _ = print "Before Stabilize\n"
val _ = f ()
val _ = stabilize()
val _ = print "After Stabilize\n"

(**** CML BOILERPLATE ****)
in () end)

