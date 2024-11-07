open Ast

let assoc_keys  l = List.map (fun (k, _) -> k) l
let assoc_value l = List.map (fun (_, v) -> v) l

let l_is_uniq = function
  | []      -> true
  | x :: xs -> List.for_all (fun y -> x = y) xs


let rec l_no_duplicate = function
  | [] -> true
  | h :: tail -> (h = "" || not (List.mem h tail)) && l_no_duplicate tail

let rec tlist_of_list l =
  match l with
  | [] -> Cst Nil
  | t :: ts -> Bop (Con, t, (tlist_of_list ts))

let rec list_of_tlist l =
  match l with
  | Cst Nil -> []
  | Bop (Con, t, ts) -> t :: (list_of_tlist ts)
  | _ -> [l]

let zip l1 l2 = 
  List.map2 (fun x1 x2 -> (x1, x2)) l1 l2

let rec list_to_l_pair_trans = function
  | []               -> []
  | _ :: []          -> []
  | [h1; h2; h3]     -> [(h1, h2); (h2, h3)]
  | hd1 :: hd2 :: tl -> (hd1, hd2) :: list_to_l_pair_trans tl
