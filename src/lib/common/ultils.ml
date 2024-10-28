open Ast

let rec tlist_of_list l =
  match l with
  | [] -> Cst Nil
  | t :: ts -> Bop (Con, t, (tlist_of_list ts))

let rec list_of_tlist l =
  match l with
  | Cst Nil -> []
  | Bop (Con, t, ts) -> t :: (list_of_tlist ts)
  | _ -> [l]
