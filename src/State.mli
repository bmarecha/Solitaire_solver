val compare_state : 's -> 's -> int 
val verif_depot : 's -> bool
val do_move : 'a -> 's -> 's
type dest = Temp | Vide | Top  

type rank = int (* 1 to 13, valet=11, dame=12, roi=13 *)
type suit = Trefle | Pique | Coeur | Carreau
type card = rank * suit

type move = card * dest 

val get_move : string -> (int -> card) * dest
val move_to_str : int * dest -> string

