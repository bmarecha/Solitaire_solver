type state = {
  depot : int * int * int * int;
  registres: Card.card list;
  colonnes: (Card.card list) FArray.t;
  mouvements : move list;
  game : game ;
}


let compare_state s1 s2 =
  match Stdlib.compare s1.depot s2.depot with
    | r -> r
    |0 -> match Stdlib.compare s1.registres s2.registres with 
            |0-> Stdlib.compare s1.colonnes s2.colonnes 
            |r -> r

let verif_depot st =
  if st.depot = 13 13 13 13 then true else false

let do_move move st =
  match st with
  | {depot; registres; colonnes; mouvements; game} ->
  match move with
  | (card, Temp) -> {depot; registres = (card::r);
        colonnes = FArray.map (
          fun col -> match col with 
          | [] -> if FArray.exists st.colonnes [Card.num_of_suit] then [] else [Card.num_of_suit] 
          | c::rest -> if c = card then rest else col
        )
        colonnes; move::mouvements ; game}
  | (card, Vide) -> {depot; 
      registres = List.filter (fun x -> if x = card then false else true) registres; 
      colonnes = FArray.map (
        fun col -> match col with 
        | [] -> if FArray.exists st.colonnes [Card.num_of_suit] then [] else [Card.num_of_suit] 
        | c::rest -> if c = card then rest else col
      )
      colonnes; move::mouvements; game}

  | (card, Top (card2))