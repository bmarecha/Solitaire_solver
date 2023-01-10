
module States = Set.Make (struct type t = State let compare = State.compare_state end)

type next_move =
  | Solved of State
  | Search of Fifo.State, States.t

let print_solved s filename =
  let oc = open_out filename in
  List.iter (fun move -> output_string oc (move_to_str move); output_char oc '\n');
  close_out oc;
  print_string "SUCCESS\n";
  exit 0

let add_if_correct card (game, dest, movelist) =
  match card, dest with
  | (_, Temp) -> (state, dest, (card, dest)::movelist)
  | ((r,_), Vide) -> 
    if (game != Midnight && game != Baker && (game != Seahaven || r = 13)) 
    then (state, dest, (card, dest)::movelist)
    else (state, dest, movelist)
  | (_, Top(x)) ->
    if not (match_rule x y game)
    then (state, dest, movelist)
    else (state, dest, move::movelist)

let moves_to col (state, movelist) =
  match col with
  | [] -> let _,_,moves = FArray.fold (add_if_correct) state.colonnes (state.game, Vide, movelist) in
        (state, moves @ movelist)
  | x::r -> let _,_,moves = FArray.fold (add_if_correct) state.colonnes (state.game, Top(x), movelist) in
        (state, moves @ movelist)
  

let all_moves state =
  let _, _, moves = FArray.fold (moves_to) state.colonnes (state, []) in
  if ((has_registres state.game) && (4 > List.length state.registres)) then
    let _, _, fmoves = FArray.fold (add_if_correct) states.colonnes (state.game, Temp, moves) in fmoves
  else
    moves

let new_state_creation move curr f states =
  let new_state = State.do_move move curr in
  if (States.mem new_state states) then
    Search (f, states)
  else if (State.solved new_state) then
    Solved new_state
  else
    Search (Fifo.add new_state, States.add new_state states)

let rec check_state_moves curr f states = function
      | [] -> Search(f, states)
      | move::rest -> match new_state_creation move curr f states with
        | Solved(s) -> Solved(s)
        | Search(fifo,states) -> check_state_moves curr fifo states rest

let rec parcour_main states filename = function
  | Fifo.empty -> print_string "INSOLUBLE"; exit 2
  | fifo -> let curr, f = Fifo.pop fifo in
    resolve_next curr f states filename
and resolve_next curr f states filename =
  let next_state = check_state_moves curr f states (all_moves curr) in
  match next_state with
  | Solved(s) -> print_solved s
  | Search(fifo, states) -> parcour_main states filename fifo

let start_search filename state =
  let first_state =  in
  let states = States.singleton first_state in
  resolve_next first_state Fifo.empty states filename
