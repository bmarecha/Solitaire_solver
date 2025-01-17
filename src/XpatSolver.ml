
open XpatLib

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type 'a t =
  | Leaf of 'a
  | Node of 'a t * 'a t

type config = {
  mutable game : State.game;
  mutable seed: int;
  mutable mode: mode;
  mutable depot: int * int * int * int;
  mutable registres: Card.card list;
  mutable colonnes: (Card.card list) FArray.t;
  mutable perm : int list
}
let config = { game = State.Freecell; seed = 1; mode = Search ""; depot = (0,0,0,0); registres = []; colonnes = FArray.make 1 []; perm = []}

let getgame = function
  | "FreeCell"|"fc" -> State.Freecell
  | "Seahaven"|"st" -> State.Seahaven
  | "MidnightOil"|"mo" -> State.Midnight
  | "BakersDozen"|"bd" -> State.Baker
  | _ -> raise Not_found

let game_tostring = function
  | State.Freecell -> "FreeCell"
  | State.Seahaven -> "Seahaven"
  | State.Midnight -> "MidnightOil"
  | State.Baker -> "BakersDozen"

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

let print_list_card cards =
  List.iter (fun c -> Printf.printf "%s " (Card.to_string c)) cards;
  print_newline ()

let print_colonnes colonnes =
  print_string "Colonnes :\n";
  FArray.iter (fun cards -> if cards = [] then print_string "Empty colonne\n" else print_list_card cards) colonnes

let print_game_conf conf =
  let mode = game_tostring conf.game in
  Printf.printf "Details of a game of %s with seed : %d\nDepot :" mode conf.seed;
  match conf.depot with
  | (x,y,z,t) -> Printf.printf " Piques %d, Coeur %d, Trefles %d, Carreaux %d" x y z t;
  print_newline ();
  match (conf.game, conf.registres) with
  | (Baker,_) | (Midnight,_) -> print_string "Pas de registres\n"; print_colonnes conf.colonnes;
  | (_,[]) -> print_string "Registres vides\n"; print_colonnes conf.colonnes;
  | (_,registres) -> List.iter (fun c -> Printf.printf "%s " (Card.to_string c)) registres;
  print_newline ();
  print_colonnes conf.colonnes


let rec fill_colonne list conf n =
  match n, conf.perm with
  | 0,_ -> list
  | _,[] -> list
  | nb,x::permut -> conf.perm <- permut; fill_colonne ((Card.of_num x)::list) conf (n - 1)


let rec move_king colonne plusieurs = (*mettre le roi fin de colonne pour Baker *)
  match colonne with 
  |[]-> if plusieurs!= 0 then move_king colonne 0 else []
  |(x,z)::[] -> [(x,z)]
  |(x,z)::(y,t)::r -> 
    if x = 13 && y = 13 then (move_king ((x,z)::r) 1)@[(y,t)] else if x = 13 then (y,t)::(move_king ((x,z)::r) 0) else (x,z)::(move_king ((y,t)::r) 0)

let deal_cards conf =
  match conf.game with
  | State.Freecell -> conf.colonnes <- FArray.init 8 (fun x -> fill_colonne [] conf (if x < 4 then 6 else 7))
  | State.Baker -> conf.colonnes <- FArray.init 13 (fun _ -> fill_colonne [] conf 4); conf.colonnes <- FArray.map (fun c -> move_king c 0) conf.colonnes
  | State.Midnight -> conf.colonnes <- FArray.init 18 (fun _ -> fill_colonne [] conf 3);
  | State.Seahaven -> conf.colonnes <- FArray.init 10 (fun _ -> fill_colonne [] conf 5);
  conf.registres <- fill_colonne [] conf 4

let first_same col y =
  match col with
  | [] -> false
  | fst::_ -> fst = y

let check_move move conf =
  match move with
  | card, _ ->
    if not ((FArray.exists (fun col -> first_same col card) conf.colonnes) || List.exists (fun el -> el = card) conf.registres) then
      false
    else
      match move with
      | (_, State.Temp) -> (State.has_registres conf.game) && (4 > List.length conf.registres)
      | ((r, _), State.Vide) -> conf.game != Midnight && conf.game != Baker
            && (conf.game != Seahaven || r = 13)
            && (FArray.exists (fun col -> col = []) conf.colonnes)
      | (x, State.Top (y)) -> 
        Printf.printf "Moving %s to %s\n" (Card.to_string card) (Card.to_string y);if not (State.match_rule x y conf.game) then false else (
      (FArray.exists (fun col -> first_same col y) conf.colonnes)
      )

let do_move move conf =
  match move with
  | (card, State.Temp) -> conf.registres <- (card::conf.registres);
        conf.colonnes <- FArray.map (fun col -> match col with | [] -> fill_colonne [] conf 1 | c::rest -> if c = card then rest else col)
        conf.colonnes
  | (card, State.Vide) -> conf.perm <- [Card.to_num card];
        conf.colonnes <- FArray.map (fun col -> match col with | [] -> fill_colonne [] conf 1 | c::rest -> if c = card then rest else col)
        conf.colonnes;
        conf.registres <- List.filter (fun x -> if x = card then false else true) conf.registres
  | (card, State.Top (card2)) -> conf.colonnes <- FArray.map (fun col -> match col with | [] -> [] | c::rest ->
                if c = card then
                  rest 
                else if c = card2 then 
                  (card::card2::rest)
                else
                  col)
    conf.colonnes;
    conf.registres <- List.filter (fun x -> if x = card then false else true) conf.registres

let add_depot conf couleur =
  match conf.depot with
  | (piq, coe, tre, car) -> match couleur with
    | 0 -> conf.depot <- (piq + 1, coe, tre, car)
    | 1 -> conf.depot <- (piq, coe + 1, tre, car)
    | 2 -> conf.depot <- (piq, coe, tre + 1, car)
    | 3 -> conf.depot <- (piq, coe, tre, car + 1)
    | _ -> conf.depot <- conf.depot

let rec fill_depot conf =
  match (conf.depot) with
  | (piq, coe, tre, car) -> let maxp, maxco, maxt, maxca = (piq + 1, Card.suit_of_num 1), (coe + 1, Card.suit_of_num 2), (tre + 1, Card.suit_of_num 0), (car + 1, Card.suit_of_num 3) in
  if (FArray.exists (fun col -> first_same col maxp || first_same col maxca || first_same col maxco || first_same col maxt) conf.colonnes) then (
    conf.colonnes <- FArray.map (fun col -> match col with
    | [] -> []
    | x::rest -> if x = maxp then (add_depot conf 0; rest)
        else if x = maxca then (add_depot conf 3; rest)
        else if x = maxco then (add_depot conf 1; rest)
        else if x = maxt then (add_depot conf 2; rest)
        else x::rest) conf.colonnes;
    fill_depot conf)
  else if (List.exists (fun c -> c = maxca || c = maxco || c = maxp || c = maxt) conf.registres) then (
    conf.registres <- List.filter (fun x -> if x = maxp then (add_depot conf 0; false)
    else if x = maxca then (add_depot conf 3; false)
    else if x = maxco then (add_depot conf 1; false)
    else if x = maxt then (add_depot conf 2; false)
    else true) conf.registres;
    fill_depot conf
  ) else
    ()

let rec check_each_line ic conf nth =
  fill_depot conf;
  try
    let str = input_line ic in
    let move = State.get_move str in
    if (check_move move conf) then (
      do_move move conf;
      print_string "Move nb";
      print_int nth;
      print_newline ();
      print_game_conf conf;
      check_each_line ic conf (nth + 1))
    else
      nth
  with
  | End_of_file -> if conf.depot = (13, 13, 13, 13) then 0 else nth

let check_file conf filename =
  let ic = open_in filename in
  let echec = check_each_line ic conf 1 in
  if echec = 0 then (
    close_in ic; print_string "SUCCES\n"; exit 0
  ) else
    close_in ic;
    print_string "ECHEC ";
    print_int echec;
    print_newline ();
    exit 1

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  conf.perm <- permut;
  print_string "Let's deal the cards\n";
  deal_cards conf;
  print_game_conf conf;
  print_string "Finished the cards\n";
  print_newline ();
  match conf.mode with
  | Search("") -> print_string "Test Over\n"; exit 0
  | Check(filename) -> check_file conf filename
  | Search(filename) -> XpatSearch.start_search filename {colonnes = conf.colonnes; depot = conf.depot; registres = conf.registres; State.moves = []; game = conf.game}

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()
