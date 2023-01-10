(** A FIFO structure (First-In First-Out),
    implemented in functional style
    (NB: the Queue module of OCaml stdlib is imperative)

    NB: l'implémentation fournie initialement ci-dessous est inefficace,
    l'améliorer (tout en restant fonctionnel). Par exemple on peut utiliser
    une paire de listes pour implémenter ['a t].

*)

type 'a t = {o : 'a list; i : 'a list} (* o = output list, i = input list
    as soon as output list is empty we List.rev inputlist to fill it
    if it is still empty the FIFO is empty *)
let empty = {o = []; i = []}
let push x = function
| {o = []; _} -> {o = [x]; i = []} (* if output is empty we directly add there *)
| {o; i} -> {o; i = x :: i}
let pop = function
| {o = []; _} -> raise Not_found
| {o = [f]; i} -> f, {o = List.rev i; i = []}
| {o = f :: t; i} -> f, {o = t; i}
let of_list l = {o = List.rev l; i = []}
let to_list {o; i} = o @ List.rev i
