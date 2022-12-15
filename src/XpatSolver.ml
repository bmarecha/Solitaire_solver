
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type 'a t =
  | Leaf of 'a
  | Node of 'a t * 'a t

type config = {
  mutable game : game;
  mutable seed: int;
  mutable mode: mode;
  mutable depot: int * int * int * int;
  mutable registres: Card.card list;
  mutable colonnes: (Card.card list) FArray.t;
  mutable perm : int list
}
let config = { game = Freecell; seed = 1; mode = Search ""; depot = (0,0,0,0); registres = []; colonnes = FArray.make 1 []; perm = []}

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let game_tostring = function
  | Freecell -> "FreeCell"
  | Seahaven -> "Seahaven"
  | Midnight -> "MidnightOil"
  | Baker -> "BakersDozen"

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


  let rec move_king colonne plusieurs =
    match colonne with 
    |[]-> if plusieurs!= 0 then move_king colonne 0 else []
    |(x,z)::[] -> [(x,z)]
    |(x,z)::(y,t)::r -> 
      if x = 13 && y = 13 then (move_king ((x,z)::r) 1)@[(y,t)] else if x = 13 then (y,t)::(move_king ((x,z)::r) 0) else (x,z)::(move_king ((y,t)::r) 0)

let deal_cards conf =
  match conf.game with
  | Freecell -> conf.colonnes <- FArray.init 8 (fun x -> fill_colonne [] conf (if x < 4 then 6 else 7))
  | Baker -> conf.colonnes <- FArray.init 13 (fun _ -> fill_colonne [] conf 4); conf.colonnes <- FArray.map (fun c -> move_king c 0) conf.colonnes
  | Midnight -> conf.colonnes <- FArray.init 18 (fun _ -> fill_colonne [] conf 3);
  | Seahaven -> conf.colonnes <- FArray.init 10 (fun _ -> fill_colonne [] conf 5);
  conf.registres <- fill_colonne [] conf 4

let check_file conf filename =
  print_string "Checking Coming Soon\n"

(* TODO : La fonction suivante est à coder en partie 2 dans un autre fichier *)

let save_search conf filename =
  print_string "Not Done Yet\n"

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
  match conf.mode with
  | Search("") -> print_string "Test Over\n"; exit 0
  | Check(filename) -> check_file conf filename; exit 0
  | Search(filename) -> save_search conf filename; exit 0

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
