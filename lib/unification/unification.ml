open Type

(* Verifie si une variable de type existe dans le reste des equations *)
let rec occurCheck (ty1 : ptype) (ty2 : ptype) : bool =
  match ty2 with
  | Var _        -> ty1 = ty2
  | Arr (t1, t2) -> (occurCheck ty1 t1) || (occurCheck ty1 t2)

let rec subs_type (ty1 : ptype) (ty2 : ptype) (ty3 : ptype) : ptype =
  match ty3 with
  | Var _            -> if ty1 = ty3 then ty2 else ty3
  | Arr (ty1', ty2') -> Arr (subs_type ty1 ty2 ty1', subs_type ty1 ty2 ty2')

let rec subs_equ (ty : ptype) (ty' : ptype) (eq : equa) : equa =
  match eq with
  | [] -> []
  | (ty1, ty2) :: tail -> 
      (subs_type ty ty' ty1, subs_type ty ty' ty2) :: subs_equ ty ty' tail

exception Unification_Fail

let rec uni_step (eq : equa) : equa =
  match eq with
  | (ty1, ty2) :: tail when ty1 = ty2 -> 
      print_endline "unit";
      tail
  (* | (Var "but", ty2) :: tail -> (Var "but", ty2) :: uni_step tail *)
  (* | (Var "but", ty2) :: tail -> uni_step (List.concat [tail; [Var "but", ty2]]) (* TODO Boucle quand plus d'autres equations*) *)
  | (Var "but", ty2) :: tail -> (List.concat [tail; [Var "but",ty2]]) 
  (* | (ty1, Var "but") :: tail -> (ty1, Var "but") :: uni_step tail *)
  (* | (ty1, Var "but") :: tail -> uni_step (List.concat [tail; [ty1 ,Var "but"]]) *)
  | (ty1, Var "but") :: tail -> (List.concat [tail; [ty1 ,Var "but"]])
  | (Var x, ty') :: tail when not (occurCheck (Var x) ty') ->
      print_endline "def";
      subs_equ (Var x) (ty') tail
  | (ty', Var x) :: tail when not (occurCheck (Var x) ty') ->
      print_endline "def";
      subs_equ (Var x) (ty') tail
  | (Arr (tga, tgr), Arr (tda, tdr)) :: tail ->
      print_endline "Arr";
      (tga, tda) :: (tgr, tdr) :: tail
  | _ :: tail -> uni_step tail
  | [] ->
      print_endline "base";
      []

let rec resolve_rec (n : int) (eq : equa) : equa =
  if n > 100 then failwith "fail unification" else
  let eq' = uni_step eq in
  print_endline (string_of_equa eq);
  if eq' = eq then eq' else
  resolve_rec (n + 1) eq'

let resolve = resolve_rec 0

