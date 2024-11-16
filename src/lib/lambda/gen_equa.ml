open Common.Utils
open Common.Type
open Common.Ast
open Error

let rec fv_of_ty (ty : ptype) =
  match ty with
  | Cst _ | Gen _     -> []
  | Var x             -> [Common.Type.Var x]
  | Lis ty            -> fv_of_ty ty
  | Rcd tys | Vrt tys ->
      List.fold_left
        (fun acc (_, ty) -> concat_uniq (fv_of_ty ty) acc) [] tys
  | Arr (ty1, ty2)    -> concat_uniq (fv_of_ty ty1) (fv_of_ty ty2)

let gen_fv_ty ty =
  List.fold_left (fun acc ty -> (Gen(ty, acc))) ty (fv_of_ty ty)

let rec rm_gen (ty : ptype) =
  match ty with
  | Cst _ | Var _     -> ty
  | Gen (_, tys)      -> rm_gen tys
  | Lis ty            -> rm_gen ty
  | Rcd tys | Vrt tys -> Rcd (List.map (fun (l, ty) -> (l, (rm_gen ty))) tys)
  | Arr (ty1, ty2)    -> Arr (rm_gen ty1, rm_gen ty2)


(* Create the equations system for the type of a terms *)
let rec gen_eq_r d e (t : term) ty : (ptype * ptype) list =
  match t with
  | Cst Tru | Cst Fal   -> [ty, Cst Bol]
  | Cst Nil             -> [ty, Lis (new_ptype ())]
  | Cst Uni             -> [ty, Cst Uni]
  | Nat _               -> [ty, Cst Nat]
  | Lbl _               -> failwith "ERROR : Typing a label"
  | Var v               -> (try [ty, (List.assoc (d - 1 - v) e)] with
                            | Not_found -> eraise (EFVNotFound (Var v)))
  | Rcd ts              -> gen_eq_r_Rcd d e ts ty
  | Vrt ts              -> gen_eq_r_Vrt d e ts ty 
  | Cas (v, ts)         -> gen_eq_r_Cas d e v ts ty
  | As  (_, typ)        -> [(ty, typ)]
  | Uop (u, t)          -> gen_eq_r_Uop d e u t ty
  | Bop (b, t1, t2)     -> gen_eq_r_Bop d e b t1 t2 ty
  | Let (x, t1, t2)     -> gen_eq_r_Let d e x t1 t2 ty
  | Cod (co, c, t1, t2) -> gen_eq_r_Cod d e co c t1 t2 ty

and get_type d e t err =
  let ta = new_ptype() in
  let eq = gen_eq_r d e t ta in 
  let vrt_t0 = Unification.resolve eq ta in
  match vrt_t0  with
    | Some ty_t0 -> snd (List.hd ty_t0)
    | None -> eraise err

and gen_eq_r_Rcd d e ts ty =
  let new_vars = List.map (fun _ -> new_ptype()) ts in
  let lbls = assoc_keys ts in
  if not (l_no_duplicate lbls) then eraise (ELabelRcdUniq (Rcd ts)) else
  let fields = zip lbls new_vars in
  let eqs = List.fold_left2
    (fun acc (_,t) i -> (gen_eq_r d e t i) @ acc) [] ts new_vars in
  (ty, Rcd fields) :: eqs

and gen_eq_r_Vrt d e ts ty =
  let new_vars = List.map (fun _ -> new_ptype()) ts in
  let lbls = assoc_keys ts in
  if not (l_no_duplicate lbls) then eraise (ELabelVrtUniq (Vrt ts)) else
  let vrt = zip lbls new_vars in
  let eqs = List.fold_left2
    (fun acc var (_,t) -> gen_eq_r d e t var @ acc) [] new_vars ts in
  (ty, Vrt vrt) :: eqs

and gen_eq_r_Cas d e v ts ty =
  let vrt_t0 = get_type d e v (ECaseNotVrt v) in
  match vrt_t0 with
  | Vrt vrt_t0 -> 
      let list_typs =
      List.fold_left2
        (fun acc (l_t0, ty_t0) (l, t) ->
         if not (l_t0 = l) then eraise (ECaseNotSameVrt (Vrt ts, v)) else
         let ta = new_ptype() in
         let eq = gen_eq_r (d + 1) ((d, ty_t0) :: e) t ta in 
         (ta, eq) :: acc
        ) [] vrt_t0 ts
      in
      (* all branches and return type have same type @ equations for all branches *)
        list_to_l_pair_trans (ty :: assoc_keys (list_typs)) @
        List.fold_left (fun acc (_,y) -> y @ acc) [] list_typs
  | _       -> eraise (ECaseNotVrt v)


and gen_eq_r_Uop d e u t ty =
  match (u, t) with
  | (HD, t) -> gen_eq_r d e t (Lis ty)
  | (TL, t) ->gen_eq_r d e t ty
  | (Abs, t') ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let eqs = gen_eq_r (d + 1) ((d, ta) :: e) t' tr in
      (ty, Arr (ta, tr)) :: eqs
  | (Fix, t) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let t' = 
        (match t with
        | Uop (Abs, t') -> t'
        | _             -> eraise (EUntypeableFix (Uop (Fix, t))))
      in
      let eqs = gen_eq_r (d + 1) ((d, Arr (ta, tr)) :: e) t' (Arr (ta, tr)) in
      (ty, Arr(ta,tr)) :: eqs

and gen_eq_r_Bop d e b t1 t2 ty =
  match (b, t1, t2) with
  | (And, t1, t2) | (Or, t1, t2) ->
      let eqs1 = gen_eq_r d e t1 (Cst Bol) in
      let eqs2 = gen_eq_r d e t2 (Cst Bol) in
      (ty, Common.Type.Cst Bol) :: eqs1 @ eqs2
  | (Prj, t, ts) ->
    (match t, ts with
      | Nat n, Rcd ts ->
        if n >= List.length ts then eraise (EPrjOutOfBound (n, (Rcd ts))) else
        let new_vars = List.map (fun _ -> new_ptype()) ts in
        let eqs = List.fold_left2
          (fun acc (_, t) i -> (gen_eq_r d e t i) @ acc) [] ts new_vars in
        (ty, List.nth new_vars n) :: eqs
      | Lbl l, Rcd ts ->
        let fields = 
          try List.map (fun (l, _) -> (l, new_ptype())) ts with
          | Not_found -> eraise (EPrjLabelNotFound (l, (Rcd ts)))
        in
        let eqs = List.fold_left2
          (fun acc (_, t) (_, t') -> (gen_eq_r d e t t') @ acc) [] ts fields in
        (ty, List.assoc l fields) :: eqs
      | Nat n, Var x ->
          let record_in_e = 
            try rm_gen (List.assoc (d - 1 - x) e) with
            | Not_found -> eraise (EFVNotFound (Var x))
          in
          (match record_in_e with
          | Rcd ty_record ->
              let ty_x = 
                try snd (List.nth ty_record n) with
                | Not_found -> eraise (EPrjOutOfBound (n, Var x))
              in
              [ty, gen_fv_ty ty_x]
          | _ -> eraise (EPrjNotVrt (Var x)))
      | Lbl l, Var x ->
          let record_in_e = 
            try rm_gen (List.assoc (d - 1 - x) e) with
            | Not_found -> eraise (EFVNotFound (Var x))
          in
          (match record_in_e with
            | Rcd ty_record ->
              let ty_x =
                try List.assoc l ty_record with
                | Not_found ->  eraise (EPrjLabelNotFound (l, ts))
              in
              [ty, gen_fv_ty ty_x]
            | _ -> eraise (EPrjNotVrt (Var x)))
      | _ -> eraise (EPrjNotNatOrLbl (t, ts)))
  | (Con, t, ts) ->
      let eqs1 = gen_eq_r d e t ty in
      let eqs2 = gen_eq_r d e ts (Lis ty) in
      (ty, Common.Type.Lis ty) :: eqs1 @ eqs2
  | (App, t1, t2) ->
      let ta = new_ptype() in
      let eqs1 = gen_eq_r d e t1 (Arr (ta, ty)) in
      let eqs2 = gen_eq_r d e t2 ta in
      eqs1 @ eqs2
  | (Add, t1, t2) | (Mul, t1, t2) | (Sub, t1, t2) ->
      let eqs1 = gen_eq_r d e t1 (Cst Nat) in
      let eqs2 = gen_eq_r d e t2 (Cst Nat) in
      (ty, Common.Type.Cst Nat) :: eqs1 @ eqs2

and gen_eq_r_Let d e x t1 t2 ty =
    let ta = new_ptype() in
    let eq = gen_eq_r d e t1 ta in 
    let eq_ta = Unification.resolve eq ta in
    match x, eq_ta with
    | Var x, Some ((_, ta) :: _ ) ->
        let fvs = fv_of_ty ta in
        let ta = List.fold_left (fun acc ty -> (Gen(ty, acc))) ta fvs in
        let eqs = gen_eq_r d ((d -1 - x, ta) :: e) t2 ty in
        eqs
    | _                   -> eraise (EUntypeableLet t1)

and gen_eq_r_Cod d e co c t1 t2 ty =
  match co, c, t1, t2 with
  | (If, c, t1, t2) ->
      let eqsc = gen_eq_r d e c (Cst Bol) in
      let eqs1 = gen_eq_r d e t1 ty in
      let eqs2 = gen_eq_r d e t2 ty in
      eqsc @ eqs1 @ eqs2
  | (Ifz, c, t1, t2) ->
      let eqsc = gen_eq_r d e c (Cst Nat) in
      let eqs1 = gen_eq_r d e t1 ty in
      let eqs2 = gen_eq_r d e t2 ty in
      eqsc @ eqs1 @ eqs2
  | (Ifn, c, t1, t2) ->
      let eqsc = gen_eq_r d e c (Lis (new_ptype ())) in
      let eqs1 = gen_eq_r d e t1 ty in
      let eqs2 = gen_eq_r d e t2 ty in
      eqsc @ eqs1 @ eqs2

let gen_equa = gen_eq_r 0 []

