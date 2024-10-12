(* open DeBrujin *)
open Type

(* Check if a type occur in anothen type *)
let rec occurCheck ty1 ty2 =
  match ty2 with
  | Var _        -> ty1 = ty2
  | Arr (t1, t2) -> (occurCheck ty1 t1) || (occurCheck ty1 t2)

(* Substitute a type ty1 by a type ty2 in ty3 *)
let rec subs_type ty1 ty2 ty3 =
  match ty3 with
  | Var _            -> if ty1 = ty3 then ty2 else ty3
  | Arr (ty1', ty2') -> Arr (subs_type ty1 ty2 ty1', subs_type ty1 ty2 ty2')

(* Substitute a type ty by a type ty' in the equations system eq *)
let rec subs_equ ty ty' eq =
  match eq with
  | [] -> []
  | (ty1, ty2) :: tail -> 
      (subs_type ty ty' ty1, subs_type ty ty' ty2) :: subs_equ ty ty' tail

(* Step of a naive unification algorithm *)
let rec uni_step eq =
  match eq with
  | (ty1, ty2) :: tail when ty1 = ty2 -> tail
  | (Var "goal", ty2) :: tail -> (List.concat [tail; [Var "goal",ty2]])
  | (ty1, Var "goal") :: tail -> (List.concat [tail; [ty1 ,Var "goal"]])
  | (Var x, ty') :: tail when not (occurCheck (Var x) ty') -> 
      subs_equ (Var x) (ty') tail
  | (ty', Var x) :: tail when not (occurCheck (Var x) ty') -> 
      subs_equ (Var x) (ty') tail
  | (Arr (tga, tgr), Arr (tda, tdr)) :: tail -> (tga, tda) :: (tgr, tdr) :: tail
  | _ :: tail -> uni_step tail
  | [] -> []

(* Resolve a equations systeme for a "goal" *)
let rec resolve_rec n eq =
  if n > 500 then 
    failwith ("fail unification of : " ^ (string_of_equa eq)) 
  else
  let eq' = uni_step eq in
  (* print_endline (string_of_equa eq); *)
  if eq' = eq then eq' else
  resolve_rec (n + 1) eq'

let resolve = resolve_rec 0

(* Infere type of a term *)
let ptype_of_term t =
  let eq = gen_equa t (Var "goal") in
  let req = resolve eq in
  try snd (List.hd req) with Failure _ -> failwith "non typable"

