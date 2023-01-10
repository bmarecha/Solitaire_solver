type game = Freecell | Seahaven | Midnight | Baker

type dest = Temp | Vide | Top of (Card.card)

type move = Card.card * dest

type state = {
  depot : int * int * int * int;
  registres: Card.card list;
  colonnes: (Card.card list) FArray.t;
  moves : move list;
  game : game
}

val get_move : string -> move
val move_to_str : move -> string

val print_moves : out_channel -> state -> unit

val has_registres : game -> bool
val match_rule : Card.card -> Card.card -> game -> bool

val compare_state : state -> state -> int 
val verif_depot : state -> int
val do_move : move -> state ->state


val fill_depot : state -> state

