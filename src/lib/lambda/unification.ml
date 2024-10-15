(* open DeBrujin *)
open Type

(* Check if a type occur in another type *)
let rec occurCheck ty1 ty2 =
  match ty2 with
  | Nat          -> false
  | Var _        -> ty1 = ty2
  | Arr (t1, t2) -> (occurCheck ty1 t1) || (occurCheck ty1 t2)

(* Substitute a type ty1 by a type ty2 in ty3 *)
let rec subs_type ty1 ty2 ty3 =
  match ty3 with
  | Nat              -> Nat
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
  | (ty1,        ty2)        :: tail when ty1 = ty2 -> Some tail
  | (Var "goal", ty') :: tail -> Some (tail @ [Var "goal", ty'])
  | (ty', Var "goal") :: tail -> Some (tail @ [ty', Var "goal"])
  | (Var x,      ty') :: _ when occurCheck (Var x) ty' -> None
  | (Var x,      ty') :: tail -> Some (subs_equ (Var x) (ty') tail)
  | (ty',      Var x) :: _ when occurCheck (Var x) ty' -> None
  | (ty',      Var x) :: tail -> Some (subs_equ (Var x) (ty') tail)
  | (Arr (tga, tgr), Arr (tda, tdr)) :: tail -> Some ((tga, tda) :: (tgr, tdr) :: tail)
  | _                 :: tail -> Option.bind (uni_step tail) (fun x -> Some x)
  | [] -> Some ([])

(* Resolve a equations systeme for a "goal" *)
let rec resolve_rec n eq =
  if n > 500 then None else
  match uni_step eq with
  | None -> None
  | Some x -> if x = eq then Some x else
              resolve_rec (n + 1) x

let resolve = resolve_rec 0

(* Infere type of a term *)
let ptype_of_term t =
  let eq = gen_equa t (Var "goal") in
  let req = resolve eq in
  match req with
  | None     -> None
  | Some []  -> None
  | Some req -> Some (snd (List.hd req))

