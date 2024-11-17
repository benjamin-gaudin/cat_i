open Ast
open Type

let alpha_t = 
  ["α";"β";"δ";"γ";"ω";"χ";"θ";"ψ";"σ";"ε";"φ";"ν";"ρ";"τ";"ζ";"υ"; "ι";"η"]

let assoc_keys  l = List.map (fun (k, _) -> k) l
let assoc_value l = List.map (fun (_, v) -> v) l

let rec concat_uniq l1 l2 =
  match l1 with
  | [] -> l2
  | x :: xs when List.mem x l2 -> concat_uniq xs l2
  | x :: xs ->  x :: concat_uniq xs l2

let l_is_uniq = function
  | []      -> true
  | x :: xs -> List.for_all (fun y -> x = y) xs

let rec l_no_duplicate = function
  | [] -> true
  | h :: tail -> (h = "" || not (List.mem h tail)) && l_no_duplicate tail

let rec map_type f = function
  | Cst c        -> Cst c
  | Var x        -> Var (f x)
  | Lis t        -> Lis (map_type f t)
  | Rcd ts -> Rcd (List.map (fun (l, t) -> l, map_type f t) ts)
  | Vrt ts -> Vrt (List.map (fun (l, t) -> l, map_type f t) ts)
  | Gen (t1, t2) -> Gen (map_type f t1, map_type f t2)
  | Arr (t1, t2) -> Arr (map_type f t1, map_type f t2)

let rec vars_of_type = function
  | Cst _        -> []
  | Var x        -> [x]
  | Lis t        -> vars_of_type t
  | Rcd ts  | Vrt ts ->
      List.fold_left (fun acc (_, t) -> concat_uniq (vars_of_type t) acc) [] ts
  | Gen (t1, t2) | Arr (t1, t2) ->
      concat_uniq (vars_of_type t1) (vars_of_type t2)

let vars_of_equas = List.fold_left 
  (fun acc (t1, t2) -> concat_uniq 
    (concat_uniq (vars_of_type t1) (vars_of_type t2)) acc) []

let subs_type_t new_vars =
  List.mapi (fun i v -> (v, List.nth new_vars i))

let subs_type sub_t =
  map_type (fun t -> try List.assoc t sub_t with Not_found -> t)

let subs_type_equa sub_t =
  List.map (fun (t1, t2) -> (subs_type sub_t t1, subs_type sub_t t2))

let rn_eq t = subs_type_equa (subs_type_t alpha_t (vars_of_equas t)) t
let rn_ty t = subs_type (subs_type_t alpha_t (vars_of_type t)) t

let rec tlist_of_list l =
  match l with
  | [] -> Ast.Cst Nil
  | t :: ts -> Bop (Con, t, (tlist_of_list ts))

let rec list_of_tlist (l : Ast.term) =
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
