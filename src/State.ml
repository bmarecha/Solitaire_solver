type game = Freecell | Seahaven | Midnight | Baker

type dest = Temp | Vide | Top of (Card.card)

type move = Card.card * dest

type state = {
  depot : int * int * int * int;
  registres: Card.card list;
  colonnes: (Card.card list) FArray.t;
  moves : move list;
  game : game ;
}

let move_to_str = function
  | (c, Temp) -> string_of_int (Card.to_num c) ^ " T"
  | (c, Vide) -> string_of_int (Card.to_num c) ^ " V"
  | (c, Top(x)) -> string_of_int (Card.to_num c) ^ " " ^ string_of_int (Card.to_num x)

let get_move str =
  Scanf.sscanf str "%d %s" (fun x s ->
    match s with
    | "T" -> Card.of_num x, Temp
    | "V" -> Card.of_num x, Vide
    | s -> Card.of_num x, Top (Card.of_num (int_of_string s))
  )

let print_moves oc s =
  List.iter (fun move -> output_string oc (move_to_str move); output_char oc '\n') (List.rev s.moves)

let has_registres mode =
  match mode with
  | Seahaven | Freecell -> true
  | _ -> false

let match_rule card toadd game =
  match card, toadd with
  | (x,y), (z,t) ->
  if (x != z - 1) then false else
  match game with
  | Baker -> true
  | Seahaven | Midnight -> (y = t)
  | Freecell -> ((Card.num_of_suit y) < 2 && (Card.num_of_suit t) > 1) || (Card.num_of_suit y > 1 && Card.num_of_suit t < 2)

let compare_state s1 s2 =
  match Stdlib.compare s1.depot s2.depot with
    | 0 -> (match Stdlib.compare s1.registres s2.registres with 
          |0-> Stdlib.compare s1.colonnes s2.colonnes 
          |r -> r)
    | r -> r

let verif_depot st =
  match st.depot with
  | (x,y,z,t) -> x + y + z + t

let add_depot depot couleur =
  match depot with
  | (piq, coe, tre, car) -> match couleur with
    | 0 -> (piq + 1, coe, tre, car)
    | 1 -> (piq, coe + 1, tre, car)
    | 2 -> (piq, coe, tre + 1, car)
    | 3 -> (piq, coe, tre, car + 1)
    | _ -> depot

let first_same col y =
  match col with
  | [] -> false
  | fst::_ -> fst = y

let rec add_registres x registres =
  match registres with
  | [] -> [x]
  | c::rest -> if c < x then x::c::rest else c::(add_registres x registres)

let rec fill_depot state =
  match state with
  | {depot; registres; colonnes; moves; game} ->
  match (depot) with
  | (piq, coe, tre, car) -> let maxp, maxco, maxt, maxca = (piq + 1, Card.suit_of_num 1), (coe + 1, Card.suit_of_num 2), (tre + 1, Card.suit_of_num 0), (car + 1, Card.suit_of_num 3) in
  if (FArray.exists (fun col -> first_same col maxp) colonnes) then
    fill_depot {depot = add_depot depot 0; registres;
    colonnes = FArray.map (fun col -> match col with
      | [] -> []
      | x::rest -> if (x = maxp) then rest else x::rest) colonnes; moves; game;}
  else if (FArray.exists (fun col -> first_same col maxco) colonnes) then
    fill_depot {depot = add_depot depot 1; registres;
    colonnes = FArray.map (fun col -> match col with
      | [] -> []
      | x::rest -> if (x = maxco) then rest else x::rest) colonnes; moves; game;}
  else if (FArray.exists (fun col -> first_same col maxt) colonnes) then
    fill_depot {depot = add_depot depot 2; registres;
    colonnes = FArray.map (fun col -> match col with
      | [] -> []
      | x::rest -> if (x = maxt) then rest else x::rest) colonnes; moves; game;}
  else if (FArray.exists (fun col -> first_same col maxca) colonnes) then
    fill_depot {depot = add_depot depot 3; registres;
    colonnes = FArray.map (fun col -> match col with
      | [] -> []
      | x::rest -> if (x = maxca) then rest else x::rest) colonnes; moves; game;}
  else if (List.exists (fun c -> c = maxp) registres) then
    fill_depot {depot = add_depot depot 0;
    registres = List.filter (fun x -> x = maxp) registres; colonnes; moves; game;}
  else if (List.exists (fun c -> c = maxco) registres) then
    fill_depot {depot = add_depot depot 1;
    registres = List.filter (fun x -> x = maxco) registres; colonnes; moves; game;}
  else if (List.exists (fun c -> c = maxt) registres) then
    fill_depot {depot = add_depot depot 2;
    registres = List.filter (fun x -> x = maxt) registres; colonnes; moves; game;}
  else if (List.exists (fun c -> c = maxca) registres) then
    fill_depot {depot = add_depot depot 3;
    registres = List.filter (fun x -> x = maxca) registres; colonnes; moves; game;}
  else
    state

let do_move move st =
  match st with
  | {depot; registres; colonnes; moves; game} ->
  match move with
  | (card, Temp) -> fill_depot {depot; registres = add_registres card registres;
        colonnes = FArray.map (
          fun col -> match col with 
          | [] -> if FArray.exists (fun c -> c = [card]) colonnes then [] else [card]
          | c::rest -> if c = card then rest else col
        ) colonnes;
        moves = move::moves ; game}
  | (card, Vide) -> fill_depot {depot; 
      registres = List.filter (fun x -> if x = card then false else true) registres; 
      colonnes = FArray.map (
        fun col -> match col with 
        | [] -> if FArray.exists (fun c -> c = [card]) colonnes then [] else [card]
        | c::rest -> if c = card then rest else col
      ) colonnes;
      moves = move::moves ; game}
  | (card, Top (card2)) -> fill_depot {depot; 
      registres = List.filter (fun x -> if x = card then false else true) registres;
      colonnes = FArray.map (fun col -> match col with | [] -> [] | c::rest ->
        if c = card then rest 
        else if c = card2 then (card::card2::rest) else col) colonnes; moves = move::moves ; game}