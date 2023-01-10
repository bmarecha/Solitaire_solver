
module States = Set.Make (struct type t = State.state let compare = State.compare_state end)

type next_move =
  | Solved of State.state
  | Search of State.state Fifo.t * States.t

let print_solved s filename =
  let oc = open_out filename in
  State.print_moves oc s;
  close_out oc;
  print_string "SUCCESS\n";
  exit 0

let add_if_correct col (game, dest, movelist) =
  match col, dest with
  | ([], _) -> (game, dest, movelist)
  | (card::rest, State.Temp) -> (game, dest, (card, dest)::movelist)
  | ((r,c)::rest, State.Vide) -> 
    if (game != State.Midnight && game != State.Baker && (game != State.Seahaven || r = 13) && rest != []) 
    then (game, dest, ((r,c), dest)::movelist)
    else (game, dest, movelist)
  | (card::rest, State.Top(x)) ->
    if not (State.match_rule card x game)
    then (game, dest, movelist)
    else (game, dest, (card, dest)::movelist)

let moves_to col ((state:State.state), movelist) =
  match state with
  | {depot; registres; colonnes; moves; game} ->
  match col with
  | [] -> let _,_,moves = FArray.fold (add_if_correct) colonnes (game, State.Vide, movelist) in
        (state, moves @ movelist)
  | x::r -> let _,_,moves = FArray.fold (add_if_correct) colonnes (game, State.Top(x), movelist) in
        (state, moves @ movelist)
  

let all_moves (state:State.state) =
  let _, moves = FArray.fold (moves_to) state.colonnes (state, []) in
  if ((State.has_registres state.game) && (4 > List.length state.registres)) then
    let _, _, fmoves = FArray.fold (add_if_correct) state.colonnes (state.game, State.Temp, moves) in fmoves
  else
    moves

let new_state_creation move curr f states =
  let new_state = State.do_move move curr in
  if (States.exists (fun state -> (State.verif_depot state > State.verif_depot new_state + 8)
    || State.compare_state state new_state = 0
    ) states) then
    Search (f, states)
  else (
  print_string "States size :";
  print_int (States.cardinal states);
  print_newline ();
  if (52 = State.verif_depot new_state) then
    Solved new_state
  else
    Search (Fifo.push new_state f, States.add new_state states)
  )

let rec check_state_moves curr f states = function
      | [] -> Search(f, states)
      | move::rest -> match new_state_creation move curr f states with
        | Solved(s) -> Solved(s)
        | Search(fifo,states) -> check_state_moves curr fifo states rest

let rec parcour_main states filename (fifo:(State.state Fifo.t)) =
  if fifo = Fifo.empty
  then (print_string "INSOLUBLE"; exit 2)
  else let curr, f = Fifo.pop fifo in
    resolve_next curr f states filename
and resolve_next curr f states filename =
  print_newline ();
  let next_state = check_state_moves curr f states (all_moves curr) in
  match next_state with
  | Solved(s) -> print_solved s filename
  | Search(fifo, states) -> parcour_main states filename fifo

let start_search filename first_state =
  let states = States.singleton first_state in
  resolve_next first_state Fifo.empty states filename
