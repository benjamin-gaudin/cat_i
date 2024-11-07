open Common.Type

(* Check if a type occur in another type *)
let rec occurCheck ty1 ty2 =
  match ty2 with
  | Cst _        -> false
  | Var _        -> ty1 = ty2
  | Lis tyl      -> occurCheck ty1 tyl
  | Rcd tys      -> List.exists (fun (_,ty) -> occurCheck ty1 ty) tys
  | Vrt tys      -> List.exists (fun (_,ty) -> occurCheck ty1 ty) tys
  | Gen (_,  ty) -> occurCheck ty1 ty
  | Arr (t1, t2) -> (occurCheck ty1 t1) || (occurCheck ty1 t2)

(* Substitute a type ty1 by a type ty2 in ty3 *)
let rec subs_type ty1 ty2 ty3 =
  match ty3 with
  | Cst cst          -> Cst cst
  | Var _            -> if ty1 = ty3 then ty2 else ty3
  | Lis tyl          -> if ty1 = ty3 then ty2 else Lis (subs_type ty1 ty2 tyl)
  | Rcd tys          -> 
      Rcd (List.map (fun (l,ty) -> (l, subs_type ty1 ty2 ty)) tys)
  | Vrt tys          -> 
      Vrt (List.map (fun (l,ty) -> (l, subs_type ty1 ty2 ty)) tys)
  | Gen (x,  ty)     -> Gen (x, subs_type ty1 ty2 ty)
  | Arr (ty1', ty2') -> Arr (subs_type ty1 ty2 ty1', subs_type ty1 ty2 ty2')

(* Substitute a type ty by a type ty' in the equations system eq *)
let rec subs_equ ty ty' eq =
  match eq with
  | [] -> []
  | (ty1, ty2) :: tail -> 
      (subs_type ty ty' ty1, subs_type ty ty' ty2) :: subs_equ ty ty' tail

let diff_consructor t1 t2 =
  match t1, t2 with
  | (Gen _, _)  | (_, Gen _) | (Var _, _) | (_, Var _) | (Lis _, Lis _) | 
    (Arr _, Arr _) | (Rcd _, Rcd _) | (Vrt _, Vrt _) -> false
  | (Cst c1, Cst c2) when c1 = c2 -> false
  | _  -> true

(* Step of a naive unification algorithm *)
let rec uni_step eq goal =
  match eq with
  | (ty1,     ty2) :: tail when ty1 = goal || ty2 = goal ->
      Some (tail @ [(ty1, ty2)])
  | (Lis ty1, Lis ty2) :: tail -> Some ((ty1, ty2) :: tail)
  | (ty1,     ty2) :: tail when ty1 = ty2 -> Some tail
  | (Gen (x, ty1), ty2) :: tail -> 
      let ta = new_ptype() in
      Some ((subs_type x ta ty1, ty2) :: tail) 
  | (ty2, Gen (x, ty1)) :: tail -> 
      let ta = new_ptype() in
      Some ((ty2, subs_type x ta ty1) :: tail) 
  | (ty1,     ty2) :: _ when diff_consructor ty1 ty2 -> None
  | (Var x,   ty') :: _ when occurCheck (Var x) ty' -> None
  | (Var x,   ty') :: tail -> Some (subs_equ (Var x) (ty') tail)
  | (ty',   Var x) :: _ when occurCheck (Var x) ty' -> None
  | (ty',   Var x) :: tail -> Some (subs_equ (Var x) (ty') tail)
  | (Arr (t, t'), Arr (u, u')) :: tail -> Some ((t, u) :: (t', u') :: tail)
  | (Rcd rs,      Rcd rs') :: tail -> 
      if List.exists2 (fun (l,_) (l',_) -> not (l = l')) rs rs' then None else
      Some (List.fold_left2
        (fun acc (_,ty) (_,ty') -> (ty, ty') :: acc) tail rs rs')
  | _              :: tail -> uni_step tail goal
  | [] -> Some ([])

(* Resolve a equations systeme for a "goal"*)
let rec resolve_rec n eq goal =
  (* Format.printf "-----\n%a\n" Pp.equas eq; *)
  if n > 500 then None else
  match uni_step eq goal with
  | None -> None
  | Some eq' -> if eq = eq' then Some eq' else
              resolve_rec (n + 1) eq' goal

let resolve = resolve_rec 0


