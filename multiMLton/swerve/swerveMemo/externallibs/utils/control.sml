signature CONTROL = 
sig

type 'a control

val mkControl: 'a -> 'a control

val set: 'a control -> 'a -> unit
val get: 'a control -> 'a
val setControl: 'a control -> 'a -> unit
val getControl: 'a control -> 'a   
			      
end
    
structure Control: CONTROL =
struct

datatype 'a control = Control of {set: ('a -> unit),
				  get: (unit -> 'a)}

fun mkControl init = let val x = ref init
		     in
			 Control {set = fn x' => x := x',
				  get = fn () => !x}
		     end

fun setControl (Control {set,...}) x = set x

fun getControl (Control {get,...}) = get ()

val set = setControl
val get = getControl

end

