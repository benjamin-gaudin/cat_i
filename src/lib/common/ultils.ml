open Ast

type opt =
  | Fv | Eq

let rec tlist_of_list l =
  match l with
  | [] -> Nil
  | t :: ts -> Con (t, (tlist_of_list ts))

let rec list_of_tlist l =
  match l with
  | Nil -> []
  | Con (t, ts) -> t :: (list_of_tlist ts)
