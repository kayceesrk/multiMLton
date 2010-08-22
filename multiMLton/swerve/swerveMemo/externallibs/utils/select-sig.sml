signature SELECT = 
sig

    type 'a select

    val mkSelect: ('a  -> bool) -> 'a select
    val always: 'a select
    val never: 'a select
    val and_o: 'a select * 'a select -> 'a select
    val or_o: 'a select * 'a select -> 'a select
    val andfold: 'a select list -> 'a select
    val orfold: 'a select list -> 'a select
    val app: 'a select -> 'a -> bool    
end
